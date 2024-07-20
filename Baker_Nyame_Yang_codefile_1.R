# install relevant packages as needed
#install.packages(c("tidyverse", "tidyr", "ggplot2", "dplyr", "geodata", "sf", "devtools",
#                   "raster", "exactextractr", "viridis", "viridisLite", "readr"))
#devtools::install_github("yutannihilation/ggsflabel")

# Clear workspace and load packages
rm(list=ls())
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(geodata)
library(raster)
library(exactextractr)
library(ggplot2)
library(viridisLite)
library(ggsflabel)
library(sf)

# STEP 1. PREPARE DATA  ********************************************************

# load administrative boundaries and designate Colonia counties 
usa_lv2 <- read_rds("data/gadm41_USA_2_pk.rds")

counties_sf <- usa_lv2 %>% unwrap() %>% st_as_sf() %>% 
  filter(NAME_1 == "New Mexico" | NAME_1 == "Texas")

tx_colonias <- c("El Paso", "Hudspeth", "Reeves", "Jeff Davis", "Nueces", "Culberson",
                 "Presidio", "Brewster", "Pecos", "Terrell", "Val Verde", "Edwards",
                 "Kinney", "Uvalde", "Maverick", "Dimmit", "Frio", "La Salle", 
                 "Webb", "Duval", "Bee", "San Patricio", "Zavala", "Brooks", 
                 "Zapata", "Starr", "Hidalgo", "Willacy", "Cameron", "Jim Wells", "Jim Hogg")

nm_colonias <- c("Dona Ana", "Grant", "Luna", "Otero", "Hidalgo")

colonias <- c(tx_colonias, nm_colonias)

# create binary indicator for whether county has Colonias
counties_sf$iscolonia <- as.integer(counties_sf$NAME_2 %in% colonias)
colonias_sf <- counties_sf %>% filter(iscolonia == 1)


# download and combine 2023 cotton farming with general crop farming data 

NAICS_cotton_2023 <- read.csv("C:/Users/katie/Dropbox/DataVizChallenge/analysis/data/2023.annual 111920 NAICS 111920 Cotton farming.csv")
NAICS_111_2023 <- read.csv("C:/Users/katie/Dropbox/DataVizChallenge/analysis/data/2023.annual 111 NAICS 111 Crop production.csv")


df_cot <- NAICS_cotton_2023 %>% 
  dplyr::select(annual_avg_emplvl, total_annual_wages, taxable_annual_wages,  
                disclosure_code, annual_avg_estabs_count, area_fips) %>%
  rename("cotton_emplvl" = "annual_avg_emplvl", "cotton_wages" = "total_annual_wages", 
         "cotton_wages_taxable" = "taxable_annual_wages", "cotton_disc" = "disclosure_code",
         "cotton_estabs" = "annual_avg_estabs_count")

df <- NAICS_111_2023 %>% 
  separate(area_title, c("county", "state"), sep = ' County, ') %>% 
  filter(state == "Texas" | state == "New Mexico") %>%
  left_join(df_cot, by = join_by(area_fips)) %>% 
  filter(own_title == "Private") %>%
  mutate(missing = disclosure_code == "N" | cotton_disc == "N" | is.na(cotton_disc),
         share_emp = cotton_emplvl/annual_avg_emplvl,
         share_wage = cotton_wages/total_annual_wages) %>%
  dplyr::select(county, state, share_emp, share_wage, cotton_emplvl, 
                cotton_wages, cotton_estabs, annual_avg_emplvl, missing)

df$iscolonia <- as.integer(df$county %in% colonias)
df$cotton_wages[df$cotton_wages == 0 ] <- NaN
df$cotton_wages_mil <- paste0(as.character(round(df$cotton_wages/1000000, 1)), "M")
df$cotton_wages_mil[df$cotton_wages_mil == "NAM" | df$cotton_wages_mil == "NaNM"] <- NA

# combine data into main geospatial dataframe for making a map
counties_sf$county <- counties_sf$NAME_2
counties_sf$state <- counties_sf$NAME_1

sf <- left_join(counties_sf, df) %>% 
  dplyr::select(county, iscolonia, share_emp, share_wage, cotton_wages, 
                cotton_wages_mil, annual_avg_emplvl, cotton_emplvl)

# keep only variables needed 
colonias_sf <- sf %>% subset(iscolonia == 1) %>% group_by(iscolonia) %>% summarise()
rm(list = setdiff(ls(), c("colonias_sf", "sf")))


# STEP 2. MAKE MAP *************************************************************

# plot the share of crop farming employment from cotton  
base <- ggplot(sf) +
  geom_sf(aes(fill = share_emp), size = 1) +
  scale_fill_gradientn(colours=rev(mako(6)), na.value = "lightgrey") +
  theme_void() + 
  theme(legend.position = c(0.2, 0.25), plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title = "Share of Total Crop Farming Employment from Cotton",
       subtitle = "with annual wages from cotton farming (USD, millions)",
       fill = "Share",
       caption = "Notes: Data come from the 2023 Quarterly Census of Employment and Wages. Some counties
colored light green have no wage data due to censoring. Colonia counties are outlined in bold.")

# add layers for Colonias border and annual wages from cotton farming
final <- base + 
  geom_sf(data = colonias_sf, alpha = 0, color = "black", linewidth = 0.7) +
  geom_sf(data = sf %>% summarise(), alpha = 0, color = "black", linewidth = 0.5) + 
  geom_sf_label_repel(data = sf, aes(label = cotton_wages_mil), color = "black", 
                      fill = "white", size = 2.8, max.overlaps = 15) 

# export output as pdf 
pdf(file = "output/cotton_share_emp_wages_2023.pdf", width = 6.5, height = 6.5)
  final
dev.off()


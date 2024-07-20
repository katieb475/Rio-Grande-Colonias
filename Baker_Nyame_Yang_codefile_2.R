#########################################################################
## Data Clean for Plot: Yields in Cotton Farming (TX and NM, 2018-2022)##
#########################################################################

## set working direction
setwd('H:/Yongwen/Data Visulization challenge/Data Visulazation')

## Cotton Production _ Line Chart _ Yongwen
Cotton_tx_planted <- read_excel("H:/Yongwen/Data Visulization challenge/Data Visulazation/Crop_Production.xlsx", 
                                sheet='Cotton_TX',
                                range = "A1:F117")
Cotton_tx_prod <- read_excel("H:/Yongwen/Data Visulization challenge/Data Visulazation/Crop_Production.xlsx", 
                             sheet='Cotton_TX',
                             range = "A1:F117")
Cotton_nm_planted <- read_excel("H:/Yongwen/Data Visulization challenge/Data Visulazation/Crop_Production.xlsx", 
                                sheet='Cotton_NM',
                                range = "A1:F10")
Cotton_nm_prod <- read_excel("H:/Yongwen/Data Visulization challenge/Data Visulazation/Crop_Production.xlsx", 
                             sheet='Cotton_NM',
                             range = "A1:F10")

##Pivot wide to long shape
Cotton_tx_planted <- Cotton_tx_planted %>%
  pivot_longer(cols=Planted_2018:Planted_2022,
               names_to = "year",
               values_to = "planted_acres")

Cotton_tx_planted$year <- stri_sub(Cotton_tx_planted$year, -4)



Cotton_nm_planted <- Cotton_nm_planted %>%
  pivot_longer(cols=Planted_2018:Planted_2022,
               names_to = "year",
               values_to = "planted_acres")
Cotton_nm_planted$year <- stri_sub(Cotton_nm_planted$year, -4)

## pivot wide data to long
Cotton_tx_prod <- Cotton_tx_prod %>%
  pivot_longer(cols=Prod_2018:Prod_2022,
               names_to = "year",
               values_to = "production")

Cotton_tx_prod$year <- stri_sub(Cotton_tx_prod$year, -4)



Cotton_nm_prod <- Cotton_nm_prod %>%
  pivot_longer(cols=Prod_2018:Pord_2022,
               names_to = "year",
               values_to = "production")
Cotton_nm_prod$year <- stri_sub(Cotton_nm_prod$year, -4)


## merge planted and production data
cotton_tx <- Cotton_tx_planted %>% 
  left_join(Cotton_tx_prod, by = c("County","year"))

cotton_nm <- Cotton_nm_planted %>% 
  left_join(Cotton_nm_prod, by = c("County","year"))


## Calculate product efficiency
cotton_tx$prod_per_acre <- cotton_tx$production/(cotton_tx$planted_acres*1000)
cotton_nm$prod_per_acre <- cotton_nm$production/(cotton_nm$planted_acres*1000)

## get counties containing missing values in Texas
missing_tx <- cotton_tx %>% filter(is.na(production))
missing_tx <- missing_tx %>% distinct(County)
missing_tx$missing <- 1

## delete counties with missing values in Texas cotton yields data for later plot
cotton_tx_plot <- cotton_tx %>% left_join(missing_tx, by="County")


## label colonial counties 
tx_colonias <- c("El Paso", "Hudspeth", "Reeves", "Jeff Davis", 
                 "Presidio", "Brewster", "Pecos", "Terrell", "Val Verde", "Edwards",
                 "Kinney", "Uvalde", "Maverick", "Dimmit", "Frio", "La Salle", 
                 "Webb", "Duval", "Bee", "San Patricio", "Zavala", "Brooks", 
                 "Zapata", "Starr", "Hidalgo", "Willacy", "Cameron", "Jim Wells", "Jim Hogg")

nm_colonias <- c("Dona Ana", "Grant", "Luna", "Otero", "Hidalgo")

colonias_tx <- data.frame(tx_colonias)
colnames(colonias_tx)[1] <- "County"
colonias_tx$colonia <- 1
cotton_tx <- cotton_tx %>% left_join(colonias_tx, by="County")

colonias_nm <- data.frame(nm_colonias)
colnames(colonias_nm)[1] <- "County"
colonias_nm$colonia <- 1
cotton_nm <- cotton_nm %>% left_join(colonias_nm, by="County")



cotton_tx_plot_2 <- cotton_tx_plot %>% filter(is.na(missing)) 
cotton_tx_plot_2$production <-  cotton_tx_plot_2$production/1000

## get counties without missing yields data over five years (2018-2022)
cotton_nm_plot  <-  cotton_nm %>% filter(County=="Dona Ana" | County=="Lea")

cotton_tx_plot_2 <- cotton_tx_plot_2 %>% left_join(colonias_tx, by="County")
cotton_tx_plot_2$colonia[is.na(cotton_tx_plot_2$colonia)] <- 0
cotton_nm_plot$colonia[is.na(cotton_nm_plot$colonia)] <- 0
cotton_nm_plot$region_code <- NA
cotton_nm_plot$missing <- NA

tx_nm <- rbind(cotton_tx_plot_2,cotton_nm_plot )

tx_nm_plot <- tx_nm %>% 
  group_by(state, year, colonia) %>%
  summarise(production = sum(production))

## clean unemployment data
unemploy <- read_excel("H:/Yongwen/Data Visulization challenge/Data Visulazation/unemployment/laucnty18.xlsx",
                       sheet="unemployment")
colonia_counties <- rbind(colonias_nm, colonias_tx)

unemploy <- unemploy %>% 
  left_join(colonia_counties, by="County",relationship = "many-to-many")

unemploy$colonia[is.na(unemploy$colonia)] <- 0

unemploy_plot <- unemploy %>% 
  dplyr::group_by(colonia, year) %>%
  dplyr::summarise(avg_unemploy = mean(`Unemployment Rate`))

unemploy_plot$colonia <- as.factor(unemploy_plot$colonia)

## final data for visualization
write_csv(tx_nm_plot, file="./final_code_plot/data_figure2.csv")

###############################################################
## Plot Code: Yields in Cotton Farming (TX and NM, 2018-2022)##
###############################################################

## main plot
 ggplot(tx_nm_plot, aes(x=year, y=mirrored_per_acre, fill=colonia_code)) +
  geom_bar(stat = "identity", width=0.75) +
  geom_line(aes(y = mirrored_employ_per_acre/30, color = colonia, group = colonia), size = 1.2,linetype="longdash") +
  geom_hline(yintercept = 0) + 
  geom_point(aes(y = mirrored_employ_per_acre/30 , color = colonia), size = 2,show.legend = FALSE) +
  scale_x_discrete() +
  labs(x = "Year",
       y = "Annual Yields per Acre (1000 bales) (Bar)",
       caption = "Data source: TX and NM Agricultural Statistics, 
       US National Agricultural Statistics Service",
       title = "            Yields and Wage in Cotton Farming 
                                 (Texas and New Mexico)") +
  scale_y_continuous(limits = c(-2,2), breaks=seq(-2,2, by=0.5),labels = abs,
                     sec.axis = sec_axis(~ . *5,  # Transformation for the secondary y-axis
                                         name = "Employment per Acre (6 counts)(Line)",
                                         breaks = seq(-5, 5, by = 2.5),  # Define breaks for secondary y-axis
                                         labels = function(x) abs(x) ))+
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("0"="#DEF5E5FF","1"="#357BA2FF"),
                    labels = c("Non-Colonia Counties","Colonia Counties"))+
  scale_color_manual(values = c("0"="#6CD3ADFF","1"="#403A75FF"),
                     labels = c("Non-Colonia Counties","Colonia Counties"))+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(family = "Courier",size = 12))


##
write_csv(tx_nm_plot, file="./final_code_plot/data_figure2.csv")





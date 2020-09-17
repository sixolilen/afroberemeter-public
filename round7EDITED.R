library(readr)
library(tidyverse)
library(gganimate)
library(plotly)
library(googleVis)
library(zoo)
library(dplyr)
library(lubridate)
library(readxl)

extrafont::loadfonts(quiet = T) # add extra fonts to R

library(readxl)
r7_merged_data_34ctry <- read_excel("C:/Users/Sixolile/Desktop/GGA-2020/GIA/Afrobarometer Corruption Data/r7_merged_data_34ctry.xlsx")
View(r7_merged_data_34ctry)

round7_afroberometer <- r7_merged_data_34ctry

#audit factor

class(round7_afroberometer)

dim(round7_afroberometer)

colnames(round7_afroberometer)

colnames(r7_merged_data_34ctry)

str(round7_afroberometer)

glimpse(round7_afroberometer)

summary(round7_afroberometer) #to get an idea on the idea

summary(round7_afroberometer$ID_country_name)

dataset1NAS <- subset(round7_afroberometer, is.na(round7_afroberometer$overall_governance))

head(round7_afroberometer)

head(round7_afroberometer, n = 15)

tail(round7_afroberometer) 

complete.cases(round7_afroberometer)


#create new data subset


round7_afro_test <- r7_merged_data_34ctry[1:159]

colnames(round7_afro_test)

round7_afro_test1 <- round7_afro_test[-11:-90]

colnames(round7_afro_test1)

round7_afro_test2 <- r7_merged_data_34ctry[c(1,6:159)]

round7_test_set <- round7_afro_test1

#select rows

round7_zim <- round7_test_set %>% 
  filter(COUNTRY == "Zimbabwe")

# recode countries

round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 1] <- "Benin"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 2] <- "Botswana"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 3] <- "Burkina Faso"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 4] <- "Cabo Varde"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 5] <- "Cameroon"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 6] <- "Côte d’Ivoire"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 7] <- 'eSwatini'
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 8] <- "Gabon"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 9] <- "Gambia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 10] <- "Ghana"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 11] <- "Guinea"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 12] <- "Kenya"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 13] <- "Lesotho"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 14] <- "Liberia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 15] <- "Madagascar"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 16] <- "Malawi"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 17] <- "Mali"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 18] <- "Mauritius"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 19] <- "Morocco"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 20] <- "Mozambique"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 21] <- "Nambia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 22] <- "Niger"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 23] <- "Nigeria"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 24] <- "São Tomé and Príncipe"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 25] <- "Senegal"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 26] <- "Sierra Leone"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 27] <- "South Africa"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 28] <- "Sudan"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 29] <- "Tanzania"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 30] <- "Togo"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 31] <- "Tunisia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 32] <- "Uganda"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 33] <- "Zambia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 34] <- "Zimbabwe"

# recode regions

round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 1] <- "West Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 2] <- "East Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 3] <- "Southern Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 4] <- "North Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 5] <- "Central Africa"



#rename variables

r7 <- round7_afro_test1 %>% 
  rename(country_name = COUNTRY, region = COUNTRY.BY.REGION, location_1 = LOCATION.LEVEL.1, age = Q1,
         area_type = URBRUR, president_ignores_laws = Q39B, fear_political_intimidation_violence = Q40,
         corruption_Presidency = Q44A, corruption_parliament = Q44B, corruption_gov_officials = Q44C,
         corruption_local_government_councillors = Q44D, corruption_police = Q44E, 
         corruption_judges_magistrates = Q44F,  corruption_traditional_leaders = Q44G, 
         corruption_religious_leaders = Q44H, corruption_business_executives = Q44I,
         corruption_nongov_organisations = Q44J, level_of_corruption = Q45)



## factors by initiated by Monique 

area_labels <- c("Urban", "Rural", "Semi_urban", "Peri_urban")

r7$area_type.f <- factor(r7$area_type, 
                         levels = c(1, 2, 3, 460),
                         labels = area_labels) 

r7$age[r7$age == -1] <- NA
r7$age[r7$age == 998] <- NA
r7$age[r7$age == 999] <- NA

cor.labels <- c("missing", "none", "some of them", "most of them", "all of them", "refused", "don't know")

r7$fear_political_intimidation_violence.f <- factor(r7$fear_political_intimidation_violence,
                                                    levels = c(-1, 0, 1, 2, 3, 8, 9),
                                                    labels = cor.labels)

r7$corruption_Presidency.f <- factor(r7$corruption_Presidency,
                                     levels = c(-1, 0, 1, 2, 3, 8, 9),
                                     labels = cor.labels)

r7$corruption_parliment.f <- factor(r7$corruption_parliament,
                                    levels = c(-1, 0, 1, 2, 3, 8, 9),
                                    labels = cor.labels)

r7$corruption_gov_officials.f <- factor(r7$fear_political_intimidation_violence,
                                        levels = c(-1, 0, 1, 2, 3, 8, 9),
                                        labels = cor.labels)

r7$corruption_local_gov_councillors.f <- factor(r7$corruption_local_government_councillors,
                                                levels = c(-1, 0, 1, 2, 3, 8, 9),
                                                labels = cor.labels)

r7$corruption_police.f <- factor(r7$corruption_police,
                                 levels = c(-1, 0, 1, 2, 3, 8, 9),
                                 labels = cor.labels)

r7$corruption_judges_magistrates.f <- factor(r7$corruption_judges_magistrates,
                                             levels = c(-1, 0, 1, 2, 3, 8, 9),
                                             labels = cor.labels)

r7$corruption_traditional_leaders.f <- factor(r7$corruption_traditional_leaders,
                                              levels = c(-1, 0, 1, 2, 3, 8, 9),
                                              labels = cor.labels)

r7$corruption_religious_leaders.f <- factor(r7$corruption_religious_leaders,
                                            levels = c(-1, 0, 1, 2, 3, 8, 9),
                                            labels = cor.labels)

r7$corruption_business_executives.f <- factor(r7$corruption_business_executives,
                                              levels = c(-1, 0, 1, 2, 3, 8, 9),
                                              labels = cor.labels)

r7$corruption_nongov_organ.f <- factor(r7$corruption_nongov_organisations,
                                       levels = c(-1, 0, 1, 2, 3, 8, 9),
                                       labels = cor.labels)

pres.lab <- c("missing", "never", "rarely", "often", "always", "refused", "don't know")

r7$president_ignores_laws.f <- factor(r7$president_ignores_laws,
                                      levels = c(-1, 0, 1, 2, 3, 8, 9),
                                      labels = pres.lab)

level.labs <- c("missing", "increased a lot", "increased somewhat", "stayed the same", "decreased somewhat", "decreased a lot", "refused", "don't know")

r7$level_of_corruption.f <- factor(r7$level_of_corruption,
                                   levels = c(-1, 1, 2, 3, 4, 5, 8, 9),
                                   labels = level.labs)

### Only Zim

r7_zim <- r7 %>% 
  filter(country_name == "Zimbabwe")

#ploting

plot(r7_zim$president_ignores_laws.f)

plot(r7_zim$corruption_Presidency.f)

plot(r7_zim$corruption_parliment.f)

plot(r7_zim$corruption_gov_officials.f)

plot(r7_zim$corruption_local_gov_councillors.f)

plot(r7_zim$corruption_police.f)

plot(r7_zim$corruption_judges_magistrates.f)

plot(r7_zim$corruption_traditional_leaders.f)

plot(r7_zim$corruption_religious_leaders.f)

plot(r7_zim$corruption_business_executives.f)

plot(r7_zim$corruption_nongov_organ.f)

plot(r7_zim$level_of_corruption.f)



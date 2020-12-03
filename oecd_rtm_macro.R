
# OECD RTM Macro-level analysis
###############################

library(ggplot2)
    library(tidyverse)
    library(XML2R)
    library(rsdmx)
    library(Rilostat)


# Import 2-stage FGLS functions from Lewis/Linzer
devtools::source_url("http://www.sscnet.ucla.edu/polisci/faculty/lewis/software/edvreg.R")
  rm(edvreg.run.test,edvreg.sim,edvreg.test)

# Regression coefficients, 1st stage regressions
################################################

betas_passive <- na.omit(read.csv("betas_passive.csv")) # Passive

# Graph results - betas
betas_passive %>% 
  ggplot(aes(x=reorder(country,-b),y=b)) +
    geom_col()

hist(betas_passive$omega)

betas_active <- na.omit(read.csv("betas_active.csv")) # Passive

# Graph results - betas
betas_active %>% 
  ggplot(aes(x=reorder(country,-b),y=b)) +
    geom_col()
  
hist(betas_active$omega)  
  
# Macro-level data
##################

# OECD Unemployment rate
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/LFS_SEXAGE_I_R/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+CRI+LTU.MW.1564.UR.A/all?startTime=2019&endTime=2019"
unem <- as.data.frame(readSDMX(url)) %>% 
    dplyr::select(-SEX,-FREQ,-TIME_FORMAT,-obsTime,-SERIES,-AGE) %>% 
    dplyr::rename(country=COUNTRY,yunem=obsValue)




# OECD Educational attainment data
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_NEAC/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.L3T4+L3+L3_C5+L4+L4_C5+L3T4_C4+L3T4_C5+L3T4_X+L5T8.T+F+M.Y25T64+Y25T34.T.VALUE.NEAC_SHARE_EA/all?startTime=9999&endTime=9999"

edudata <- as.data.frame(readSDMX(url)) %>% 
    filter(SEX=="T") %>% 
    dplyr::select(-FIELD,-SEX,-TIME_FORMAT,-UNIT,-POWERCODE,-obsTime,-OBS_STATUS,-TIME_FORMAT,-INDICATOR,-MEASURE) %>% 
    rename(Country = COUNTRY) %>% 
        mutate(isced = dplyr::recode(ISC11A,
                              "L3T4" = "Up. sec. + post-sec. non-tert.",
                              "L3_C5" = "Up. sec. (voc.)",
                              "L4_C5" = "Post-sec. non-tert. (voc.)",
                              "L3T4_C4" = "Up. sec. + post-sec. non-tert. (general)",
                              "L3T4_C5" = "Up. sec. + post-sec. non-tert. (voc.)",
                              "L5T8" = "Tertiary",
                              "L3T4_X" = "Not spec."))

edudata %>% 
    filter(AGE=="Y25T34" & ISC11A %in% c("L3T4_C5","L5T8")) %>% 
    ggplot(aes(x=Country,y=obsValue,fill=isced)) +
        geom_bar(position = "dodge",stat = "identity")

isced <- edudata %>%
    filter(AGE=="Y25T34" & ISC11A %in% c("L3T4_C5")) %>% 
    dplyr::select(Country,obsValue) %>% 
    rename(voc = obsValue)





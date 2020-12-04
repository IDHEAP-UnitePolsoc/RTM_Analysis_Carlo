
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

# OECD EPL Data
###############
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EPL_OV/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+CRI+LVA+LTU.EPRC_V4/all?startTime=2019&endTime=2019"

epl <- as.data.frame(readSDMX(url)) %>% 
    dplyr::select(COUNTRY,obsValue) %>% 
    rename(country=COUNTRY,
           epl=obsValue)

# OECD ALMP (Training) data
###########################
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/LMPEXP/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+USA+LTU.20.EXPPCT.A/all?startTime=2018&endTime=2018"

train <- as.data.frame(readSDMX(url)) %>% 
    dplyr::select(LFS_COUNTRY,obsValue) %>% 
    rename(country=LFS_COUNTRY,
           train=obsValue)

# OECD Benefit conditionality data
##################################
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/SBE/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BGR+HRV+CYP+MLT+ROU.TIER1.AVAIL+OVER/all?startTime=2020&endTime=2020"

cond <- as.data.frame(readSDMX(url)) %>% 
    dplyr::select(LOCATION,INDICATOR,obsValue) %>% 
    rename(country=LOCATION,
           ind=INDICATOR,
           val=obsValue) %>% 
    pivot_wider(values_from = val,
                names_from = ind,
                id_cols = country)

# OECD Tax rate data
####################

url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE_I6/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.ALL_IN_RATE+ALL_IN_RATE_SING+ALL_IN_RATE_SING_NO_CH+ALL_IN_RATE_SING_TWO_CH+ALL_IN_RATE_MAR+ALL_IN_RATE_MAR_NO_CH+ALL_IN_RATE_MAR_TWO_CH+ALL_IN_LESS_CASH+ALL_IN_LESS_CASH_SING+ALL_IN_LESS_CASH_SING_TWO_CH+ALL_IN_LESS_CASH_MAR+ALL_IN_LESS_CASH_MAR_NO_CH+ALL_IN_LESS_CASH_MAR_TWO_CH/all?startTime=2019&endTime=2019"

tax <- as.data.frame(readSDMX(url)) %>% # average wage! 
    dplyr::filter(ALL_IN %in% c("ALL_IN_RATE_SING_TWO_CH",
                                "ALL_IN_RATE_SING_NO_CH",
                                "ALL_IN_RATE_MAR_TWO_CH",
                                "ALL_IN_RATE_MAR_NO_CH")) %>% 
  pivot_wider(values_from = obsValue,
                     names_from = ALL_IN,
                     id_cols = COU) %>% 
  rename(country = COU) %>% 
  rowwise() %>% 
  mutate(taxrate = mean(c(ALL_IN_RATE_SING_TWO_CH,ALL_IN_RATE_SING_NO_CH,
                          ALL_IN_RATE_MAR_TWO_CH,ALL_IN_RATE_MAR_NO_CH)))

# Merging
#########


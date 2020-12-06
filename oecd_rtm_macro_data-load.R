
#################################################
# Loading macro-level data for 2nd stage analysis
#################################################

# Carlo Knotz

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
    dplyr::rename(country=COUNTRY,unem=obsValue)


# OECD Educational attainment data
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_NEAC/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.L3T4+L3+L3_C5+L4+L4_C5+L3T4_C4+L3T4_C5+L3T4_X+L5T8.T+F+M.Y25T64+Y25T34.T.VALUE.NEAC_SHARE_EA/all?startTime=9999&endTime=9999"

edudata <- as.data.frame(readSDMX(url)) %>% 
    filter(SEX=="T") %>% 
    dplyr::select(-FIELD,-SEX,-TIME_FORMAT,-UNIT,-POWERCODE,-obsTime,-OBS_STATUS,-TIME_FORMAT,-INDICATOR,-MEASURE) %>% 
    rename(country = COUNTRY) %>% 
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
    ggplot(aes(x=country,y=obsValue,fill=isced)) +
        geom_bar(position = "dodge",stat = "identity")

isced <- edudata %>%
    filter(AGE=="Y25T34" & ISC11A %in% c("L3T4_C5")) %>% 
    dplyr::select(country,obsValue) %>% 
    rename(voc = obsValue)

# OECD EPL Data
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EPL_OV/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+CRI+LVA+LTU.EPRC_V4/all?startTime=2019&endTime=2019"

epl <- as.data.frame(readSDMX(url)) %>% 
    dplyr::select(COUNTRY,obsValue) %>% 
    rename(country=COUNTRY,
           epl=obsValue)

# OECD ALMP (Training) data
url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/LMPEXP/AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+IRL+ISR+ITA+JPN+KOR+LVA+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+GBR+USA+LTU.20.EXPPCT.A/all?startTime=2018&endTime=2018"

train <- as.data.frame(readSDMX(url)) %>% 
    dplyr::select(LFS_COUNTRY,obsValue) %>% 
    rename(country=LFS_COUNTRY,
           train=obsValue)

# OECD Benefit conditionality data
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

# ILO Occupational structure data
occstruct <- get_ilostat(id = "EMP_TEMP_SEX_OCU_NB_A",
                         filters = list(timefrom = 2010)) %>% 
  filter(ref_area %in% betas_active$country) %>% 
  filter(sex == "SEX_T") %>% 
  dplyr::select(-note_classif,-note_indicator,-note_source,-indicator,-source) %>% 
  rename(country = ref_area,
         vals = obs_value,
         year = time) %>% 
  pivot_wider(id_cols = c(country,year),
              values_from = vals,
              names_from = classif1) %>% 
  dplyr::select(-contains("ISCO88"),-contains("SKILL")) %>% 
  mutate(isco_1 = (OCU_ISCO08_1/OCU_ISCO08_TOTAL),
         isco_2 = (OCU_ISCO08_2/OCU_ISCO08_TOTAL),
         isco_3 = (OCU_ISCO08_3/OCU_ISCO08_TOTAL),
         isco_4 = (OCU_ISCO08_4/OCU_ISCO08_TOTAL),
         isco_5 = (OCU_ISCO08_5/OCU_ISCO08_TOTAL),
         isco_6 = (OCU_ISCO08_6/OCU_ISCO08_TOTAL),
         isco_7 = (OCU_ISCO08_7/OCU_ISCO08_TOTAL),
         isco_8 = (OCU_ISCO08_8/OCU_ISCO08_TOTAL),
         isco_9 = (OCU_ISCO08_9/OCU_ISCO08_TOTAL),
         isco_0 = (OCU_ISCO08_0/OCU_ISCO08_TOTAL),
         isco_X = (OCU_ISCO08_X/OCU_ISCO08_TOTAL)) %>% 
  dplyr::select(-contains("_ISCO08_")) %>% 
  rowwise() %>% 
  mutate(high_rti = sum(c(isco_4,isco_7,isco_9,isco_6,isco_8))) %>%  # High-RTI occupational groups: 4, 7, 9, 6, 8
  filter(year>2010) %>% 
  group_by(country) %>% 
  mutate_each(funs(delta),c(high_rti)) %>% 
  group_by(country) %>% 
  summarise(sum_del_highrti = sum(high_rti, na.rm = T))

occstruct %>% 
  ggplot(aes(x=reorder(country, sum_del_highrti),y=sum_del_highrti)) +
    geom_col()

# Merging
#########

betas_active <- betas_active %>% 
    rename(b_act = b,
           tval_act = tval,
           pval_act = pval,
           ul_act = ul,
           ll_act = ll,
           omega_act = omega)

betas_passive <- betas_passive %>% 
    rename(b_pas = b,
           tval_pas = tval,
           pval_pas = pval,
           ul_pas = ul,
           ll_pas = ll,
           omega_pas = omega)


macrodata <- betas_active %>% # outcome vars
  left_join(betas_passive,
            by = c("country"))
  rm(betas_active,betas_passive)

macrodata <- macrodata %>% # occupational structure
  left_join(occstruct,
            by = c("country"))
  rm(occstruct)

macrodata <- macrodata %>% # educational attainment
  left_join(isced,
             by = c("country"))
  rm(isced,edudata)
  
macrodata <- macrodata %>% # unemployment benefit conditionality
  left_join(cond,
             by = c("country"))
  rm(cond)

macrodata <- macrodata %>% # avg. income tax rates
  left_join(tax,
            by = c("country"))
  rm(tax)
  
macrodata <- macrodata %>% # dismissal rules
  left_join(epl,
            by = c("country"))
  rm(epl)
  
macrodata <- macrodata %>% # labor market training spending
  left_join(train,
            by = c("country"))
  rm(train)

macrodata <- macrodata %>% # unemployment rate
  left_join(unem,
            by = c("country"))
  rm(unem)
  
  
# Effective spending on ALMP (following Rueda, 2015, CP)
macrodata <- macrodata %>% 
  mutate(train_pc = (train*100)/unem)



# OECD RTM Macro-level analysis
###############################

library(ggplot2)
    library(tidyverse)
    library(XML2R)
    library(rsdmx)

effects <- readxl::read_excel("cntryeffects.xlsx")


# Graph results - betas
effects %>% 
    mutate(cntry = factor(effects$Country, levels = effects$Country[order(effects$beta_passive, decreasing = T)])) %>% 
    select(cntry,beta_passive, beta_active) %>% 
    pivot_longer(cols = c("beta_active","beta_passive"),
                 names_to="inds",
                 values_to="vals") %>% 
    ggplot(aes(x=cntry,y=vals,fill=inds)) +
        geom_bar(position = "dodge", stat = "identity")


# Graph results - rhos
effects %>% 
    #mutate(cntry = factor(effects$Country, levels = effects$Country[order(effects$rho_passive, decreasing = T)])) %>% 
    select(Country,rho_passive) %>% 
    #pivot_longer(cols = c("rho_active","rho_passive"),
     #            names_to="inds",
      #           values_to="vals") %>% 
    ggplot(aes(x=reorder(Country, -rho_passive),y=rho_passive)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_rect(aes(ymin=-.4,ymax=.4,xmin=-Inf,xmax=Inf), alpha=0.05,fill="pink")

effects %>% 
    select(Country, rho_active) %>% 
    ggplot(aes(x=reorder(Country, -rho_active),y=rho_active)) +
        geom_bar(position = "dodge", stat = "identity")  +
        geom_rect(aes(ymin=-.4,ymax=.4,xmin=-Inf,xmax=Inf), alpha=0.05,fill="pink")


effects %>%
    select(Country,rho_passive, rho_active) %>% 
    ggplot(aes(x=rho_passive,y=rho_active)) +
        geom_rect(aes(xmin=-0.3,ymin=-Inf,ymax=Inf,xmax=.3), alpha=0.01,fill="pink") +
        geom_rect(aes(xmin=-Inf,ymin=-0.3,ymax=0.3,xmax=Inf), alpha=0.01,fill="green") +
        geom_point() +
        geom_text(aes(label=Country)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0)

# Reading in OECD Educational attainment data
#############################################

url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EAG_NEAC/AUS+AUT+BEL+CAN+CHL+COL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA.L3T4+L3+L3_C5+L4+L4_C5+L3T4_C4+L3T4_C5+L3T4_X+L5T8.T+F+M.Y25T64+Y25T34.T.VALUE.NEAC_SHARE_EA/all?startTime=9999&endTime=9999"

edudata <- as.data.frame(readSDMX(url)) %>% 
    filter(SEX=="T") %>% 
    select(-FIELD,-SEX,-TIME_FORMAT,-UNIT,-POWERCODE,-obsTime,-OBS_STATUS,-TIME_FORMAT,-INDICATOR,-MEASURE) %>% 
    rename(Country = COUNTRY) %>% 
        mutate(isced = recode(ISC11A,
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
    select(Country,obsValue) %>% 
    rename(voc = obsValue)

data <- merge(effects,isced,
                 by.x = "Country")

data %>% 
    ggplot(aes(x=voc,y=rho_passive)) +
        geom_point() +
        geom_text(aes(label=Country)) +
        geom_smooth(method = "lm")

data %>% 
    ggplot(aes(x=voc,y=rho_active)) +
        geom_point() +
        geom_text(aes(label=Country)) +
        geom_smooth(method = "lm")

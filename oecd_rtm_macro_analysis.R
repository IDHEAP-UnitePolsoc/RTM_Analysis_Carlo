###############################
# OECD RTM Macro-level analysis
###############################

library(ggplot2)
    library(tidyverse)
    library(XML2R)
    library(rsdmx)
    library(Rilostat)
    library(texreg)
    library(jtools)


# Import 2-stage FGLS functions from Lewis/Linzer
devtools::source_url("http://www.sscnet.ucla.edu/polisci/faculty/lewis/software/edvreg.R")
  rm(edvreg.run.test,edvreg.sim,edvreg.test)
  
# Helper function for ILO data below
delta <- function(x) {x-lag(x)}

# Load data
source("oecd_rtm_macro_data-load.R")

# Analysis - passive measures
#############################

# Overall unemployment (.05)
mod1 <- edvreg(macrodata$b_pas~macrodata$unem, 
               omegasq = macrodata$omega_pas)
summary(mod1)

# training spending (NAs excluded)
mod2 <- edvreg(macrodata$b_pas[which(!is.na(macrodata$train_pc))]~macrodata$train_pc[which(!is.na(macrodata$train_pc))],
               omegasq = macrodata$omega_pas[which(!is.na(macrodata$train_pc))]) 
summary(mod2)

# EPL (.1)
mod3 <- edvreg(macrodata$b_pas~macrodata$epl, 
              omegasq = macrodata$omega_pas)
summary(mod3)

# Overall benefit conditionality
mod4 <- edvreg(macrodata$b_pas[which(!is.na(macrodata$OVER))]~macrodata$OVER[which(!is.na(macrodata$OVER))],
               omegasq = macrodata$omega_pas[which(!is.na(macrodata$OVER))]) 
summary(mod4)

# Availability requirements
mod5 <- edvreg(macrodata$b_pas[which(!is.na(macrodata$AVAIL))]~macrodata$AVAIL[which(!is.na(macrodata$AVAIL))],
               omegasq = macrodata$omega_pas[which(!is.na(macrodata$AVAIL))]) 
summary(mod5)

# Occupational change (.05)!
mod6 <- edvreg(macrodata$b_pas~macrodata$sum_del_highrti,
               omegasq = macrodata$omega_pas)
summary(mod6)

# Vocational training intensity
mod7 <- edvreg(macrodata$b_pas[which(!is.na(macrodata$voc))]~macrodata$voc[which(!is.na(macrodata$voc))],
               omegasq = macrodata$omega_pas[which(!is.na(macrodata$voc))])
summary(mod7)

# Tax burden on income
mod8 <- edvreg(macrodata$b_pas~macrodata$taxrate,
               omegasq = macrodata$omega_pas)
summary(mod8)

# So far: Overall unemployment, EPL, occupational change - tentative test of multivariate estimations
mvmod1 <- edvreg(macrodata$b_pas~macrodata$unem + macrodata$epl,
                 omegasq = macrodata$omega_pas)
summary(mvmod1) # Unemployment itself, not EPL

mvmod2 <- edvreg(macrodata$b_pas ~ macrodata$unem + macrodata$sum_del_highrti,
                 omegasq = macrodata$omega_pas)
summary(mvmod2) # both significant! 

# Graphical inspection
macrodata %>% 
    ggplot(aes(x=unem,y=b_pas)) +
        geom_point() +
        geom_text(aes(label=country),
                  check_overlap = F,
                  size = 3,
                  nudge_y = 0.005) +
        stat_smooth(method = "lm",
                    linetype = "solid",
                    color = "black",
                    size = .25) +
    ylab("Est. effect of automation vulnerability on preferences") +
    xlab("Unemployment rate (%)") +
    theme_bw() +
    theme(panel.grid = element_blank())
    ggsave("unem_pas.pdf")

macrodata %>% 
    ggplot(aes(x=(sum_del_highrti*100),y=b_pas)) +
        geom_point() +
        geom_text(aes(label=country),
                  check_overlap = F,
                  size = 3,
                  nudge_y = 0.005) +
        stat_smooth(method = "lm",
                    linetype = "solid",
                    color = "black",
                    size = .25) +
    ylab("Est. effect of automation vulnerability on preferences") +
    xlab("Cumul. change high-RTI occupations (%)") +
    theme_bw() +
    theme(panel.grid = element_blank())
    ggsave("occ_pas.pdf")

# Export via texreg
screenreg(list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8),
          custom.coef.names = c("Intercept",
                                "Unemployment rate",
                                "Labor market training",
                                "Dismissal rules",
                                "Benefit conditionality",
                                "Avail. requirements",
                                "Occupational change",
                                "Vocational training",
                                "Tax burden"),
          stars = c(0.05,0.1),
          include.rsquared = T, include.adjrs = F, include.nobs = T)

texreg(list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8),
       file = "bivarmods_passive.tex",
       custom.coef.names = c("Intercept",
                                "Unemployment rate",
                                "Labor market training",
                                "Dismissal rules",
                                "Benefit conditionality",
                                "Avail. requirements",
                                "Occupational change",
                                "Vocational training",
                                "Tax burden"),
          stars = c(0.05,0.1),
          include.rsquared = T, include.adjrs = F, include.nobs = T,
       booktabs = T,
       dcolumn = T,
       table=F,
       use.packages = F)

screenreg(list(mvmod1,mvmod2),
          stars = c(0.05,0.1),
          include.rsquared = T, include.adjrs = F, include.nobs = T,
          digits = 3,
          custom.coef.names = c("Intercept",
                                "Unemployment rate",
                                "Dismissal rules",
                                "Occupational change"))

texreg(list(mvmod1,mvmod2),
       file = "mvarmods_passive.tex",
          stars = c(0.05,0.1),
          include.rsquared = T, include.adjrs = F, include.nobs = T,
          digits = 3,
          custom.coef.names = c("Intercept",
                                "Unemployment rate",
                                "Dismissal rules",
                                "Occupational change"),
       booktabs = T,
       dcolumn = T,
       table=F,
       use.packages = F,
       single.row = T)
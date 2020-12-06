###############################
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
  
# Helper function for ILO data below
delta <- function(x) {x-lag(x)}

# Load data
source("oecd_rtm_macro_data-load.R")
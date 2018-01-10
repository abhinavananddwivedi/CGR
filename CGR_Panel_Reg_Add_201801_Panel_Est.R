### CGR Script: Started 201801 ###

## Final push: last few models to run and report. INTERNET to Internet_Usage
## with and without developemental PC1 + others

### Replicate and Extend Panel Estimation for CGR post 201707

### Libraries ####################################################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)

##################################################################################

## Parse and post process LHS and RHS data tidily ################################

file_script_process <- "CGR_Panel_Reg_Add_201707_Post_Processing.R" # Post Process

source(file_script_process, echo = F) # Post-processed data files

##################################################################################

### Models post 201801 ###

## Equity

Equity_devpc <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + Dev_PC1
Equity_pre_devpc <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Dev_PC1
Equity_post_devpc <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1

Equity_int_use <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + Internet_Usage
Equity_pre_int_use <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Internet_Usage
Equity_post_int_use <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage

## Bond

Bond_devpc <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + Dev_PC1
Bond_pre_devpc <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Dev_PC1
Bond_post_devpc <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1

Bond_int_use <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + Internet_Usage
Bond_pre_int_use <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Internet_Usage
Bond_post_int_use <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage

## REIT

REIT_devpc <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + Dev_PC1
REIT_pre_devpc <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM + Dev_PC1
REIT_post_devpc <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1

REIT_int_use <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + Internet_Usage
REIT_pre_int_use <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM + Internet_Usage
REIT_post_int_use <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage

#####################################################################################

## Country Names

name_country_equity <- LHS_equity %>%
  dplyr::select(-Year) %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK,
                `United Arab Emirates` = UAE) %>%
  colnames(.)

name_country_bond <- LHS_bond %>%
  dplyr::select(-Year) %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK) %>%
  colnames(.)

name_country_REIT <- LHS_REIT %>%
  dplyr::select(-Year) %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK) %>%
  colnames(.)

##

### Stratification into developed, emerging and frontier economies ##########################

index_eq_developing <- c(5,7,8,9,12,13,14,19,20,25,30,31,
                         35,37,38,39,42,45,46,48,49,50,51,
                         52,55,57,58,59,60,61,65,66,68,72,
                         73,75,78,79,80,81,82,83,87,88,89)

# The names of "frontier" markets have been taken from S&P 2011
index_eq_frontier_SP <- c(1, 4, 5, 7, 9, 14, 15, 16, 19, 21, 25, 35, 37:43, 
                          48, 52, 55, 57:59, 64, 65, 70, 71, 75, 80, 81, 
                          83, 84, 88:89)

# Which countries are common to the developing and frontier list? 
index_common_deving_front <- lubridate::intersect(index_eq_developing, index_eq_frontier_SP)

# Emerging country definition
# Which countries are developing but not in the common list? Classify as emerging
index_eq_emerging <- lubridate::setdiff(index_eq_developing, index_common_deving_front)

# Which countries are frontier in S&P but not in developing list? 
index_front_not_deving <- lubridate::setdiff(index_eq_frontier_SP, index_common_deving_front)

# Frontier country definition
index_eq_frontier <- lubridate::union(index_common_deving_front, index_front_not_deving)

# Developing country definition
index_eq_developing <- lubridate::union(index_eq_frontier, index_eq_emerging)

# Developed country definition
index_eq_developed <- lubridate::setdiff(1:length(name_country_equity), index_eq_developing)

name_eq_emerging <- name_country_equity[index_eq_emerging]
name_eq_frontier <- name_country_equity[index_eq_frontier]
name_eq_developed <- name_country_equity[index_eq_developed]

############################################################################################

### Partitioning Years #####################################################################

year_liq <- 1985:2016
year_pol <- 1984:2013
year_dev <- 1980:2016
year_pre_00 <- 1986:1999
year_post_00 <- 2000:2012
year_LHS <- 1986:2012
year_bal <- 1996:2010
year_bal_1 <- 1996:2003
year_bal_2 <- 2004:2010

###

### For new panel estimations ###

## With developmental principal component

data_equity_devpc <- Panel_equity %>% 
  dplyr::select(-c(INTERNET, US_bond_spread:Equity_Liq)) %>% 
  na.omit()

data_bond_devpc <- Panel_bond %>% 
  dplyr::select(-c(INTERNET, US_bond_spread:Bond_Liq)) %>% 
  na.omit()

data_REIT_devpc <- Panel_REIT %>% 
  dplyr::select(-c(INTERNET, US_bond_spread:REIT_Liq)) %>% 
  na.omit()

## With country level internet usage

data_equity_int_use <- Panel_equity %>% 
  dplyr::select(-c(INTERNET, US_bond_spread:Life_Exp, Equity_Liq:Dev_PC1)) %>% 
  na.omit()

data_bond_int_use <- Panel_bond %>% 
  dplyr::select(-c(INTERNET, US_bond_spread:Life_Exp, Bond_Liq:Dev_PC1)) %>% 
  na.omit()

data_REIT_int_use <- Panel_REIT %>% 
  dplyr::select(-c(INTERNET, US_bond_spread:Life_Exp, REIT_Liq:Dev_PC1)) %>% 
  na.omit()

###############################################################################

################################################################################################
### FUNCTION DECLARATION 
################################################################################################

panel_est <- function(form, data_matrix)
{
  mdl <- "within"
  ind <- c("Country", "Year")
  
  panel_reg <- plm::plm(formula = form, data = data_matrix, model = mdl, index = ind)
  panel_reg_rob <- lmtest::coeftest(panel_reg, vcovHC(panel_reg, type = "HC0", cluster = "group"))
  
  test_out <- summary(panel_reg)
  test_out$coefficients <- unclass(panel_reg_rob) #Include robust coefficients and T stats
  
  return(test_out)
}

################################################################################################

### Panel estimation with country level Internet Usage as explanatory variable

## Equity

Panel_Equity_Int_Use <- panel_est(Equity_int_use, data_equity_int_use)

Panel_Equity_Int_Use_Pre <- panel_est(Equity_pre_int_use, subset(data_equity_int_use, Year %in% year_pre_00))
Panel_Equity_Int_Use_Post <- panel_est(Equity_post_int_use, subset(data_equity_int_use, Year %in% year_post_00))

Panel_Equity_Int_Use_Dev <- panel_est(Equity_int_use, subset(data_equity_int_use, Country %in% name_eq_developed))
Panel_Equity_Int_Use_Emerg <- panel_est(Equity_int_use, subset(data_equity_int_use, Country %in% name_eq_emerging))
Panel_Equity_Int_Use_Front <- panel_est(Equity_int_use, subset(data_equity_int_use, Country %in% name_eq_frontier))

## Bond

Panel_Bond_Int_Use <- panel_est(Bond_int_use, data_bond_int_use)

Panel_Bond_Int_Use_Pre <- panel_est(Bond_pre_int_use, subset(data_bond_int_use, Year %in% year_pre_00))
Panel_Bond_Int_Use_Post <- panel_est(Bond_post_int_use, subset(data_bond_int_use, Year %in% year_post_00))

## REIT

Panel_REIT_Int_Use <- panel_est(REIT_int_use, data_REIT_int_use)

Panel_REIT_Int_Use_Pre <- panel_est(REIT_pre_int_use, subset(data_REIT_int_use, Year %in% year_pre_00))
Panel_REIT_Int_Use_Post <- panel_est(REIT_post_int_use, subset(data_REIT_int_use, Year %in% year_post_00))

###

### Panel estimation with country level Developmental PC1 as explanatory variable

## Equity

Panel_Equity_devpc <- panel_est(Equity_devpc, data_equity_devpc)

Panel_Equity_devpc_Pre <- panel_est(Equity_pre_devpc, subset(data_equity_devpc, Year %in% year_pre_00))
Panel_Equity_devpc_Post <- panel_est(Equity_post_devpc, subset(data_equity_devpc, Year %in% year_post_00))

Panel_Equity_devpc_Dev <- panel_est(Equity_devpc, subset(data_equity_devpc, Country %in% name_eq_developed))
Panel_Equity_devpc_Emerg <- panel_est(Equity_devpc, subset(data_equity_devpc, Country %in% name_eq_emerging))
Panel_Equity_devpc_Front <- panel_est(Equity_devpc, subset(data_equity_devpc, Country %in% name_eq_frontier))

## Bond

Panel_Bond_devpc <- panel_est(Bond_devpc, data_bond_devpc)

Panel_Bond_devpc_Pre <- panel_est(Bond_pre_devpc, subset(data_bond_devpc, Year %in% year_pre_00))
Panel_Bond_devpc_Post <- panel_est(Bond_post_devpc, subset(data_bond_devpc, Year %in% year_post_00))

## REIT

Panel_REIT_devpc <- panel_est(REIT_devpc, data_REIT_devpc)

Panel_REIT_devpc_Pre <- panel_est(REIT_pre_devpc, subset(data_REIT_devpc, Year %in% year_pre_00))
Panel_REIT_devpc_Post <- panel_est(REIT_post_devpc, subset(data_REIT_devpc, Year %in% year_post_00))

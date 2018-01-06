### CGR Script 2017 Sep ##########################################################

### Replicate and Extend Panel Estimation for CGR post 201707

### Libraries ####################################################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)

##################################################################################

### Directory Management #########################################################

#folder <- "July_2017"
#subfolder <- "Data_July_2017"
#file_path <- paste0(folder, "/", subfolder, "/")
#file_path <- paste0(subfolder, "/")

##################################################################################

## Parse and post process LHS and RHS data tidily ################################

file_script_process <- "CGR_Panel_Reg_Add_201707_Post_Processing.R" # Post Process
#file_path_script_process <- paste0(folder, "/", file_script_process)
#file_path_script_process <- paste0(subfolder, "/", file_script_process)

# Run the prior postprocessing file, note that it in turn sources the 
# script file that tidily reads and parses the data
#source(file_path_script_process, echo = F) # Post-processed data files
source(file_script_process, echo = F) # Post-processed data files

##################################################################################

### Full Panel Estimation Formulas (Unbalanced) ##################################

### Old Models ###################################################################

Equity_old <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Equity_old_Pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM 
Equity_old_Post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

Bond_old <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Bond_old_Pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM 
Bond_old_Post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

REIT_old <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
REIT_old_Pre <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM 
REIT_old_Post <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

#################################################################################

### New Models ##################################################################

# Equity_new <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro +
#   US_bond_spread + Credit_to_GDP + Market_cap_to_GDP +
#   Num_public_firms + Trade_to_GDP + Turnover_Domest_Total +
#   Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk + Equity_Liq + Dev_PC1

# Equity_new <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
#   US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
#   Agg_Pol_Risk + Equity_Liq + Life_Exp

Equity_new <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + Equity_Liq + Dev_PC1
Equity_new_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + Equity_Liq + Dev_PC1
Equity_new_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + Equity_Liq + Dev_PC1

# Bond_new <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro +
#   US_bond_spread + Credit_to_GDP + Market_cap_to_GDP +
#   Num_public_firms + Trade_to_GDP + Turnover_Domest_Total +
#   Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

# Bond_new <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
#   US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
#   Agg_Pol_Risk + Bond_Liq + Life_Exp

Bond_new <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + Bond_Liq + Dev_PC1
Bond_new_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + Bond_Liq + Dev_PC1
Bond_new_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + Bond_Liq + Dev_PC1

# REIT_new <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro +
#   US_bond_spread + Credit_to_GDP + Market_cap_to_GDP +
#   Num_public_firms + Trade_to_GDP + Turnover_Domest_Total +
#   Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk + REIT_Liq + Dev_PC1

# REIT_new <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
#   US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
#   Agg_Pol_Risk + REIT_Liq + Life_Exp

REIT_new <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + REIT_Liq + Dev_PC1
REIT_new_pre <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + ERM +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + REIT_Liq + Dev_PC1
REIT_new_post <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk +
  Agg_Pol_Risk + REIT_Liq + Dev_PC1

#################################################################################

### Model number 7 ##############################################################
# (old model without FEDFUNDS, INTERNET and with aggregate risk indices)

Equity_model_7 <- LHS_div_eq ~ TED + VIX + SENT + 
  ERM + Euro + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk
Equity_model_7_pre <- LHS_div_eq ~ TED + VIX + SENT +
  ERM + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk 
Equity_model_7_post <- LHS_div_eq ~ TED + VIX + SENT +
  Euro + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk

Bond_model_7 <- LHS_div_b ~ TED + VIX + SENT +
  ERM + Euro + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk
Bond_model_7_pre <- LHS_div_b ~ TED + VIX + SENT +
  ERM + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk 
Bond_model_7_post <- LHS_div_b ~ TED + VIX + SENT +
  Euro + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk

REIT_model_7 <- LHS_div_r ~ TED + VIX + SENT +
  ERM + Euro + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk
REIT_model_7_pre <- LHS_div_r ~ TED + VIX + SENT +
  ERM + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk 
REIT_model_7_post <- LHS_div_r ~ TED + VIX + SENT +
  Euro + Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk

########################################################################################

### Emerging and developed equity markets ##############################################

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

### Panel Estimation Begins ################################################################

# For old regressions in full

data_old_equity <- Panel_equity %>%
  dplyr::select(Country:Euro) %>%
  na.omit()

data_old_bond <- Panel_bond %>%
  dplyr::select(Country:Euro) %>%
  na.omit()

data_old_REIT <- Panel_REIT %>%
  dplyr::select(Country:Euro) %>%
  na.omit()

# For new regressions in full

## Look at regression formulas above before doing this step!!! ##

# data_new_equity <- Panel_equity %>% 
#   dplyr::select(-c(Market_cap_to_GDP:Turnover_Domest_Total, Dev_PC1)) %>% 
#   na.omit()
# 
# data_new_bond <- Panel_bond %>% 
#   dplyr::select(-c(Market_cap_to_GDP:Turnover_Domest_Total, Dev_PC1)) %>% 
#   na.omit()
# 
# data_new_REIT <- Panel_REIT %>% 
#   dplyr::select(-c(Market_cap_to_GDP:Turnover_Domest_Total, Dev_PC1)) %>% 
#   na.omit()

data_new_equity <- Panel_equity %>% 
  dplyr::select(-c(Market_cap_to_GDP:Turnover_Domest_Total, Life_Exp)) %>% 
  na.omit()

data_new_bond <- Panel_bond %>% 
  dplyr::select(-c(Market_cap_to_GDP:Turnover_Domest_Total, Life_Exp)) %>% 
  na.omit()

data_new_REIT <- Panel_REIT %>% 
  dplyr::select(-c(Market_cap_to_GDP:Turnover_Domest_Total, Life_Exp)) %>% 
  na.omit()

# For model number 7 

data_equity_7 <- Panel_equity %>%
  dplyr::select(Country:SENT, ERM, Euro, Agg_Fin_Risk:Agg_Pol_Risk) %>%
  na.omit()

data_bond_7 <- Panel_bond %>%
  dplyr::select(Country:SENT, ERM, Euro, Agg_Fin_Risk:Agg_Pol_Risk) %>%
  na.omit()

data_REIT_7 <- Panel_REIT %>%
  dplyr::select(Country:SENT, ERM, Euro, Agg_Fin_Risk:Agg_Pol_Risk) %>%
  na.omit()

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

### Equity Panel Estimations Full ########################################################

### OLD MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro (commmon RHS) ####

##########################################################################################

attach(data_old_equity)

Panel_Equity_Old <- panel_est(Equity_old, data_old_equity)

Panel_Equity_Old_Pre <- panel_est(Equity_old_Pre, subset(data_old_equity, Year %in% year_pre_00))
Panel_Equity_Old_Post <- panel_est(Equity_old_Post, subset(data_old_equity, Year %in% year_post_00))

Panel_Equity_Old_Dev <- panel_est(Equity_old, subset(data_old_equity, Country %in% name_eq_developed))
Panel_Equity_Old_Emerg <- panel_est(Equity_old, subset(data_old_equity, Country %in% name_eq_emerging))
Panel_Equity_Old_Front <- panel_est(Equity_old, subset(data_old_equity, Country %in% name_eq_frontier))

detach(data_old_equity)

attach(data_old_bond)

Panel_Bond_Old <- panel_est(Bond_old, data_old_bond)

Panel_Bond_Old_Pre <- panel_est(Bond_old_Pre, subset(data_old_bond, Year %in% year_pre_00))
Panel_Bond_Old_Post <- panel_est(Bond_old_Post, subset(data_old_bond, Year %in% year_post_00))

detach(data_old_bond)

attach(data_old_REIT)

Panel_REIT_Old <- panel_est(REIT_old, data_old_REIT)

Panel_REIT_Old_Pre <- panel_est(REIT_old_Pre, subset(data_old_REIT, Year %in% year_pre_00))
Panel_REIT_Old_Post <- panel_est(REIT_old_Post, subset(data_old_REIT, Year %in% year_post_00))

detach(data_old_REIT)

##########################################################################################

### NEW MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro + NEW STUFF ######

##########################################################################################

attach(data_new_equity)

Panel_Equity_New <- panel_est(Equity_new, data_new_equity)

Panel_Equity_New_Pre <- panel_est(Equity_new_pre, data_new_equity)
Panel_Equity_New_Post <- panel_est(Equity_new_post, data_new_equity)

Panel_Equity_New_Dev <- panel_est(Equity_new, subset(data_new_equity, Country %in% name_eq_developed))
Panel_Equity_New_Emerg <- panel_est(Equity_new, subset(data_new_equity, Country %in% name_eq_emerging))
Panel_Equity_New_Front <- panel_est(Equity_new, subset(data_new_equity, Country %in% name_eq_frontier))

detach(data_new_equity)

attach(data_new_bond)

Panel_Bond_New <- panel_est(Bond_new, data_new_bond)

Panel_Bond_New_Pre <- panel_est(Bond_new_pre, data_new_bond)
Panel_Bond_New_Post <- panel_est(Bond_new_post, data_new_bond)

detach(data_new_bond)

attach(data_new_REIT)

Panel_REIT_New <- panel_est(REIT_new, data_new_REIT)

Panel_REIT_New_Pre <- panel_est(REIT_new_pre, data_new_REIT)
Panel_REIT_New_Post <- panel_est(REIT_new_post, data_new_REIT)

detach(data_new_REIT)

#########################################################################################

### Model number 7: LHS ~ TED + VIX + SENT + ERM + Euro + Agg_Econ + Agg_Fin + Agg_Pol

#########################################################################################

attach(data_equity_7)

Panel_Equity_7 <- panel_est(Equity_model_7, data_equity_7)

Panel_Equity_7_Pre <- panel_est(Equity_model_7_pre, data_equity_7)
Panel_Equity_7_Post <- panel_est(Equity_model_7_post, data_equity_7)

Panel_Equity_7_Dev <- panel_est(Equity_model_7, subset(data_equity_7, Country %in% name_eq_developed))
Panel_Equity_7_Emerg <- panel_est(Equity_model_7, subset(data_equity_7, Country %in% name_eq_emerging))
Panel_Equity_7_Front <- panel_est(Equity_model_7, subset(data_equity_7, Country %in% name_eq_frontier))

detach(data_equity_7)

attach(data_bond_7)

Panel_Bond_7 <- panel_est(Bond_model_7, data_bond_7)

Panel_Bond_7_Pre <- panel_est(Bond_model_7_pre, data_bond_7)
Panel_Bond_7_Post <- panel_est(Bond_model_7_post, data_bond_7)

detach(data_bond_7)

attach(data_REIT_7)

Panel_REIT_7 <- panel_est(REIT_model_7, data_REIT_7)

Panel_REIT_7_Pre <- panel_est(REIT_model_7_pre, data_REIT_7)
Panel_REIT_7_Post <- panel_est(REIT_model_7_post, data_REIT_7)

detach(data_REIT_7)
#############################################################################################

#############################################################################################
### BALANCED REGRESSIONS CARRIED OUT HEREON #################################################
#############################################################################################

### Panel Estimation Formulas ###############################################################

# Note that ERM is never a factor in balanced regressions: 1996--2010

Equity_old_bal <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro
Equity_old_bal_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET
Equity_old_bal_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

Bond_old_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro
Bond_old_bal_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET
Bond_old_bal_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

REIT_old_bal <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro
REIT_old_bal_pre <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET
REIT_old_bal_post <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

Equity_new_bal <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + Equity_Liq + Dev_PC1
Equity_new_bal_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS +
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + Equity_Liq + Dev_PC1
Equity_new_bal_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + Equity_Liq + Dev_PC1

Bond_new_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + Bond_Liq + Dev_PC1
Bond_new_bal_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + Bond_Liq + Dev_PC1
Bond_new_bal_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + Bond_Liq + Dev_PC1

REIT_new_bal <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + REIT_Liq + Dev_PC1
REIT_new_bal_pre <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + REIT_Liq + Dev_PC1
REIT_new_bal_post <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Agg_Fin_Risk + Agg_Econ_Risk + 
  Agg_Pol_Risk + REIT_Liq + Dev_PC1

############################################################################################

############################################################################################
### OLD BALANCED MODELS REPLICATION ########################################################
############################################################################################

## Equity Panels Balanced ##################################################################

temp <- Panel_equity %>%
  dplyr::select(Country:Euro) %>%
  dplyr::select(-ERM) %>%
  dplyr::filter(Year %in% year_bal)

bal_eq_old <- c()

for (i in 1:length(name_country_equity))
{
  temp_new <- temp[which(temp$Country == name_country_equity[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_eq_old <- rbind(bal_eq_old, temp_new)
  }
}

attach(bal_eq_old)

Bal_Equity_Old <- panel_est(Equity_old_bal, bal_eq_old)

Bal_Equity_Old_Pre <- panel_est(Equity_old_bal_pre, subset(bal_eq_old, Year %in% year_bal_1))
Bal_Equity_Old_Post <- panel_est(Equity_old_bal_post, subset(bal_eq_old, Year %in% year_bal_2))

Bal_Equity_Old_Dev <- panel_est(Equity_old_bal, subset(bal_eq_old, Country %in% name_eq_developed))
Bal_Equity_Old_Emerg <- panel_est(Equity_old_bal, subset(bal_eq_old, Country %in% name_eq_emerging))
Bal_Equity_Old_Front <- panel_est(Equity_old_bal, subset(bal_eq_old, Country %in% name_eq_frontier))


detach(bal_eq_old)

## Bond Panels Balanced ##################################################################

temp <- Panel_bond %>%
  dplyr::select(Country:Euro) %>%
  dplyr::select(-ERM) %>%
  dplyr::filter(Year %in% year_bal)

bal_b_old <- c()

for (i in 1:length(name_country_bond))
{
  temp_new <- temp[which(temp$Country == name_country_bond[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_b_old <- rbind(bal_b_old, temp_new)
  }
}

attach(bal_b_old)

Bal_Bond_Old <- panel_est(Bond_old_bal, bal_b_old)

Bal_Bond_Old_Pre <- panel_est(Bond_old_bal_pre, subset(bal_b_old, Year %in% year_bal_1))
Bal_Bond_Old_Post <- panel_est(Bond_old_bal_post, subset(bal_b_old, Year %in% year_bal_2))

detach(bal_b_old)

## REIT Panels Balanced ##################################################################

temp <- Panel_REIT %>%
  dplyr::select(Country:Euro) %>%
  dplyr::select(-ERM) %>%
  dplyr::filter(Year %in% year_bal)

bal_r_old <- c()

for (i in 1:length(name_country_REIT))
{
  temp_new <- temp[which(temp$Country == name_country_REIT[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_r_old <- rbind(bal_r_old, temp_new)
  }
}

attach(bal_r_old)

Bal_REIT_Old <- panel_est(REIT_old_bal, bal_r_old)

Bal_REIT_Old_Pre <- panel_est(REIT_old_bal_pre, subset(bal_r_old, Year %in% year_bal_1))
Bal_REIT_Old_Post <- panel_est(REIT_old_bal_post, subset(bal_r_old, Year %in% year_bal_2))

detach(bal_r_old)
###########################################################################################

###########################################################################################
### NEW BALANCED MODELS ###################################################################
###########################################################################################

## Equity Panels Balanced #################################################################

temp <- Panel_equity %>%
  dplyr::select(-c(ERM, Market_cap_to_GDP:Turnover_Domest_Total, Life_Exp)) %>%
  dplyr::filter(Year %in% year_bal)

bal_eq_new <- c()

for (i in 1:length(name_country_equity))
{
  temp_new <- temp[which(temp$Country == name_country_equity[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_eq_new <- rbind(bal_eq_new, temp_new)
  }
}

attach(bal_eq_new)

Bal_Equity_New <- panel_est(Equity_new_bal, bal_eq_new)

Bal_Equity_New_Pre <- panel_est(Equity_new_bal_pre, subset(bal_eq_new, Year %in% year_bal_1))
Bal_Equity_New_Post <- panel_est(Equity_new_bal_post, subset(bal_eq_new, Year %in% year_bal_2))

Bal_Equity_New_Dev <- panel_est(Equity_new_bal, subset(bal_eq_new, Country %in% name_eq_developed))
Bal_Equity_New_Emerg <- panel_est(Equity_new_bal, subset(bal_eq_new, Country %in% name_eq_emerging))
#Bal_Equity_New_Front <- panel_est(Equity_new_bal, subset(bal_eq_new, Country %in% name_eq_frontier))


detach(bal_eq_new)


## Bond Panel Balanced ##############################################################################

temp <- Panel_bond %>%
  dplyr::select(-c(ERM, Market_cap_to_GDP:Turnover_Domest_Total, Life_Exp)) %>%
  dplyr::filter(Year %in% year_bal)

bal_b_new <- c()

for (i in 1:length(name_country_bond))
{
  temp_new <- temp[which(temp$Country == name_country_bond[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_b_new <- rbind(bal_b_new, temp_new)
  }
}

attach(bal_b_new)

Bal_Bond_New <- panel_est(Bond_new_bal, bal_b_new)

Bal_Bond_New_Pre <- panel_est(Bond_new_bal_pre, subset(bal_b_new, Year %in% year_bal_1))
Bal_Bond_New_Post <- panel_est(Bond_new_bal_post, subset(bal_b_new, Year %in% year_bal_2))

detach(bal_b_new)


## REIT Panel Balanced ##############################################################################

temp <- Panel_REIT %>%
  dplyr::select(-c(ERM, Market_cap_to_GDP:Turnover_Domest_Total, Life_Exp)) %>%
  dplyr::filter(Year %in% year_bal)

bal_r_new <- c()

for (i in 1:length(name_country_REIT))
{
  temp_new <- temp[which(temp$Country == name_country_REIT[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_r_new <- rbind(bal_r_new, temp_new)
  }
}

attach(bal_r_new)

Bal_REIT_New <- panel_est(REIT_new_bal, bal_r_new)

Bal_REIT_New_Pre <- panel_est(REIT_new_bal_pre, subset(bal_r_new, Year %in% year_bal_1))
Bal_REIT_New_Post <- panel_est(REIT_new_bal_post, subset(bal_r_new, Year %in% year_bal_2))

detach(bal_r_new)
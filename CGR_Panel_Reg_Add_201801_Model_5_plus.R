##################################################################################
### CGR Script: 201801 with augmented model 5 ####################################
##################################################################################

## Augmented model #5 as defined below in detail #################################

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

### Equity #######################################################################

# Model 5

Equity_model_5 <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

Equity_model_5_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

Equity_model_5_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

# Model 5 + Equity_Liq + Internet_Usage

Equity_model_5_aug_int <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Equity_Liq + Internet_Usage

Equity_model_5_aug_int_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM +
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Equity_Liq + Internet_Usage

Equity_model_5_aug_int_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Equity_Liq + Internet_Usage

# Model 5 + Equity_Liq + Developmental PC1

Equity_model_5_aug_devpc <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Equity_Liq + Dev_PC1

Equity_model_5_aug_devpc_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + ERM +
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Equity_Liq + Dev_PC1

Equity_model_5_aug_devpc_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Equity_Liq + Dev_PC1

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
 
data_equity_model_5 <- Panel_equity %>% 
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk)) %>% 
  na.omit()

data_equity_model_5_aug_int <- Panel_equity %>% 
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk, Internet_Usage:Equity_Liq)) %>% 
  na.omit()

data_equity_model_5_aug_devpc <- Panel_equity %>% 
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk, Equity_Liq, Dev_PC1)) %>% 
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

################################################################################################
### UNBALANCED PANEL ESTIMATIONS BEGIN HERERON #################################################
################################################################################################

### Panel estimation for model 5 and its augmentations begins here #############################

## Equity Model 5 

Panel_Equity_model_5 <- panel_est(Equity_model_5, 
                                  data_equity_model_5)

Panel_Equity_model_5_Pre <- panel_est(Equity_model_5_pre, 
                                      subset(data_equity_model_5, Year %in% year_pre_00))
Panel_Equity_model_5_Post <- panel_est(Equity_model_5_post, 
                                       subset(data_equity_model_5, Year %in% year_post_00))

Panel_Equity_model_5_Dev <- panel_est(Equity_model_5, 
                                      subset(data_equity_model_5, Country %in% name_eq_developed))
Panel_Equity_model_5_Emerg <- panel_est(Equity_model_5, 
                                        subset(data_equity_model_5, Country %in% name_eq_emerging))
Panel_Equity_model_5_Front <- panel_est(Equity_model_5, 
                                        subset(data_equity_model_5, Country %in% name_eq_frontier))

## Equity Model 5 augmented with equity liq and internet usage

Panel_Equity_model_5_aug_int <- panel_est(Equity_model_5_aug_int, 
                                  data_equity_model_5_aug_int)

Panel_Equity_model_5_aug_int_Pre <- panel_est(Equity_model_5_aug_int_pre, 
                                      subset(data_equity_model_5_aug_int, Year %in% year_pre_00))
Panel_Equity_model_5_aug_int_Post <- panel_est(Equity_model_5_aug_int_post, 
                                       subset(data_equity_model_5_aug_int, Year %in% year_post_00))

Panel_Equity_model_5_aug_int_Dev <- panel_est(Equity_model_5_aug_int, 
                                      subset(data_equity_model_5_aug_int, Country %in% name_eq_developed))
Panel_Equity_model_5_aug_int_Emerg <- panel_est(Equity_model_5_aug_int, 
                                        subset(data_equity_model_5_aug_int, Country %in% name_eq_emerging))
Panel_Equity_model_5_aug_int_Front <- panel_est(Equity_model_5_aug_int, 
                                        subset(data_equity_model_5_aug_int, Country %in% name_eq_frontier))

## Equity Model 5 augmented with equity liq and developmental PC1

Panel_Equity_model_5_aug_devpc <- panel_est(Equity_model_5_aug_devpc, 
                                          data_equity_model_5_aug_devpc)

Panel_Equity_model_5_aug_devpc_Pre <- panel_est(Equity_model_5_aug_devpc_pre, 
                                              subset(data_equity_model_5_aug_devpc, Year %in% year_pre_00))
Panel_Equity_model_5_aug_devpc_Post <- panel_est(Equity_model_5_aug_devpc_post, 
                                               subset(data_equity_model_5_aug_devpc, Year %in% year_post_00))

Panel_Equity_model_5_aug_devpc_Dev <- panel_est(Equity_model_5_aug_devpc, 
                                              subset(data_equity_model_5_aug_devpc, Country %in% name_eq_developed))
Panel_Equity_model_5_aug_devpc_Emerg <- panel_est(Equity_model_5_aug_devpc, 
                                                subset(data_equity_model_5_aug_devpc, Country %in% name_eq_emerging))
Panel_Equity_model_5_aug_devpc_Front <- panel_est(Equity_model_5_aug_devpc, 
                                                subset(data_equity_model_5_aug_devpc, Country %in% name_eq_frontier))

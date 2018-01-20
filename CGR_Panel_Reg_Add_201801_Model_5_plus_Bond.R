##################################################################################
### CGR Script: 201801 with augmented model 5 for bonds ##########################
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

### Bond #######################################################################

# Model 5

Bond_model_5 <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

Bond_model_5_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

Bond_model_5_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

# Model 5 + Bond_Liq + Internet_Usage

Bond_model_5_aug_int <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Internet_Usage

Bond_model_5_aug_int_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM +
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Internet_Usage

Bond_model_5_aug_int_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Internet_Usage

# Model 5 + Bond_Liq + Developmental PC1

Bond_model_5_aug_devpc <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

Bond_model_5_aug_devpc_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + ERM +
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

Bond_model_5_aug_devpc_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

#####################################################################################

## Country Names

name_country_bond <- LHS_bond %>%
  dplyr::select(-Year) %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK) %>%
  colnames(.)

##

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

data_bond_model_5 <- Panel_bond %>% 
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk)) %>% 
  na.omit()

data_bond_model_5_aug_int <- Panel_bond %>% 
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk, Internet_Usage:Bond_Liq)) %>% 
  na.omit()

data_bond_model_5_aug_devpc <- Panel_bond %>% 
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk, Bond_Liq, Dev_PC1)) %>% 
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

## Bond Model 5 

Panel_Bond_model_5 <- panel_est(Bond_model_5, 
                                  data_bond_model_5)

Panel_Bond_model_5_Pre <- panel_est(Bond_model_5_pre, 
                                      subset(data_bond_model_5, Year %in% year_pre_00))
Panel_Bond_model_5_Post <- panel_est(Bond_model_5_post, 
                                       subset(data_bond_model_5, Year %in% year_post_00))

## Bond Model 5 augmented with Bond liq and internet usage

Panel_Bond_model_5_aug_int <- panel_est(Bond_model_5_aug_int, 
                                          data_bond_model_5_aug_int)

Panel_Bond_model_5_aug_int_Pre <- panel_est(Bond_model_5_aug_int_pre, 
                                              subset(data_bond_model_5_aug_int, Year %in% year_pre_00))
Panel_Bond_model_5_aug_int_Post <- panel_est(Bond_model_5_aug_int_post, 
                                               subset(data_bond_model_5_aug_int, Year %in% year_post_00))

## Bond Model 5 augmented with Bond liq and developmental PC1

Panel_Bond_model_5_aug_devpc <- panel_est(Bond_model_5_aug_devpc, 
                                            data_bond_model_5_aug_devpc)

Panel_Bond_model_5_aug_devpc_Pre <- panel_est(Bond_model_5_aug_devpc_pre, 
                                                subset(data_bond_model_5_aug_devpc, Year %in% year_pre_00))
Panel_Bond_model_5_aug_devpc_Post <- panel_est(Bond_model_5_aug_devpc_post, 
                                                 subset(data_bond_model_5_aug_devpc, Year %in% year_post_00))

################################################################################################
##### BALANCED PANEL ESTIMATIONS BEGIN HERERON #################################################
################################################################################################

# Note that ERM is never a factor in balanced regressions: 1996--2010

### Panel Estimation Formulas ##################################################################

# Model 5

Bond_model_5_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

Bond_model_5_bal_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS +
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

Bond_model_5_bal_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk 

# Model 5 + Bond_Liq + Internet_Usage

Bond_model_5_aug_int_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Internet_Usage

Bond_model_5_aug_int_bal_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Internet_Usage

Bond_model_5_aug_int_bal_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Internet_Usage

# Model 5 + Bond_Liq + Developmental PC1

Bond_model_5_aug_devpc_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

Bond_model_5_aug_devpc_bal_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS +
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

Bond_model_5_aug_devpc_bal_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  Agg_Econ_Risk + Agg_Fin_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

#####################################################################################

### Balanced Panel Estimations: Model #5

temp_model_5 <- Panel_bond %>%
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk)) %>% 
  dplyr::filter(Year %in% year_bal)

bal_b_model_5 <- c()

for (i in 1:length(name_country_bond))
{
  temp_new <- temp_model_5[which(temp_model_5$Country == name_country_bond[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_b_model_5 <- rbind(bal_b_model_5, temp_new)
  }
}

attach(bal_b_model_5)

Bal_Bond_model_5 <- panel_est(Bond_model_5_bal, bal_b_model_5)

Bal_Bond_model_5_Pre <- panel_est(Bond_model_5_bal_pre, subset(bal_b_model_5, Year %in% year_bal_1))
Bal_Bond_model_5_Post <- panel_est(Bond_model_5_bal_post, subset(bal_b_model_5, Year %in% year_bal_2))

detach(bal_b_model_5)
####################################################################################################

### Balanced Panel Estimations: Model #5 Augmented with Bond_Liq and Int_Use

temp_model_5_aug_int <- Panel_bond %>%
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk, Internet_Usage:Bond_Liq)) %>% 
  dplyr::filter(Year %in% year_bal)

bal_b_model_5_aug_int <- c()

for (i in 1:length(name_country_bond))
{
  temp_new <- temp_model_5_aug_int[which(temp_model_5_aug_int$Country == name_country_bond[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_b_model_5_aug_int <- rbind(bal_b_model_5_aug_int, temp_new)
  }
}

attach(bal_b_model_5_aug_int)

Bal_Bond_model_5_aug_int <- panel_est(Bond_model_5_aug_int_bal, bal_b_model_5_aug_int)

Bal_Bond_model_5_aug_int_Pre <- panel_est(Bond_model_5_aug_int_bal_pre, 
                                            subset(bal_b_model_5_aug_int, Year %in% year_bal_1))
Bal_Bond_model_5_aug_int_Post <- panel_est(Bond_model_5_aug_int_bal_post, 
                                             subset(bal_b_model_5_aug_int, Year %in% year_bal_2))


detach(bal_b_model_5_aug_int)
####################################################################################################

### Balanced Panel Estimations: Model #5 Augmented with Bond_Liq and Dev_PC1

temp_model_5_aug_devpc <- Panel_bond %>%
  dplyr::select(c(Country:FEDFUNDS, ERM:Euro, Agg_Fin_Risk:Agg_Pol_Risk, Bond_Liq, Dev_PC1)) %>% 
  dplyr::filter(Year %in% year_bal)

bal_b_model_5_aug_devpc <- c()

for (i in 1:length(name_country_bond))
{
  temp_new <- temp_model_5_aug_devpc[which(temp_model_5_aug_devpc$Country == name_country_bond[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_b_model_5_aug_devpc <- rbind(bal_b_model_5_aug_devpc, temp_new)
  }
}

attach(bal_b_model_5_aug_devpc)

Bal_Bond_model_5_aug_devpc <- panel_est(Bond_model_5_aug_devpc_bal, bal_b_model_5_aug_devpc)

Bal_Bond_model_5_aug_devpc_Pre <- panel_est(Bond_model_5_aug_devpc_bal_pre, 
                                              subset(bal_b_model_5_aug_devpc, Year %in% year_bal_1))
Bal_Bond_model_5_aug_devpc_Post <- panel_est(Bond_model_5_aug_devpc_bal_post, 
                                               subset(bal_b_model_5_aug_devpc, Year %in% year_bal_2))


detach(bal_b_model_5_aug_devpc)
####################################################################################################

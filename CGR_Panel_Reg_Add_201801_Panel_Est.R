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

#########################################################################################################

#############################################################################################
### BALANCED REGRESSIONS CARRIED OUT HEREON #################################################
#############################################################################################

# Note that ERM is never a factor in balanced regressions: 1996--2010

### Panel Estimation Formulas ###############################################################

## Equity

Equity_devpc_bal <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1
Equity_devpc_bal_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Dev_PC1
Equity_devpc_bal_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1

Equity_int_use_bal <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage
Equity_int_use_bal_pre <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Internet_Usage
Equity_int_use_bal_post <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage

## Bond

Bond_devpc_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1
Bond_devpc_bal_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Dev_PC1
Bond_devpc_bal_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1

Bond_int_use_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage
Bond_int_use_bal_pre <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Internet_Usage
Bond_int_use_bal_post <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage

## REIT

REIT_devpc_bal <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1
REIT_devpc_bal_pre <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Dev_PC1
REIT_devpc_bal_post <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + Dev_PC1

REIT_int_use_bal <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage
REIT_int_use_bal_pre <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Internet_Usage
REIT_int_use_bal_post <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + Internet_Usage

#######################################################################################################

###########################################################################################
### BALANCED MODELS #######################################################################
###########################################################################################

### Equity Panels Balanced #################################################################

### With country level developmental PC1 in RHS

temp_devpc <- Panel_equity %>%
  dplyr::select(-c(INTERNET, US_bond_spread:Equity_Liq)) %>%
  dplyr::filter(Year %in% year_bal)

bal_eq_devpc <- c()

for (i in 1:length(name_country_equity))
{
  temp_new <- temp_devpc[which(temp_devpc$Country == name_country_equity[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_eq_devpc <- rbind(bal_eq_devpc, temp_new)
  }
}

attach(bal_eq_devpc)

Bal_Equity_devpc <- panel_est(Equity_devpc_bal, bal_eq_devpc)

Bal_Equity_devpc_Pre <- panel_est(Equity_devpc_bal_pre, subset(bal_eq_devpc, Year %in% year_bal_1))
Bal_Equity_devpc_Post <- panel_est(Equity_devpc_bal_post, subset(bal_eq_devpc, Year %in% year_bal_2))

Bal_Equity_devpc_Dev <- panel_est(Equity_devpc_bal, subset(bal_eq_devpc, Country %in% name_eq_developed))
Bal_Equity_devpc_Emerg <- panel_est(Equity_devpc_bal, subset(bal_eq_devpc, Country %in% name_eq_emerging))
#Bal_Equity_New_Front <- panel_est(Equity_new_bal, subset(bal_eq_new, Country %in% name_eq_frontier))


detach(bal_eq_devpc)

### With country level internet usage in RHS

temp_int_use <- Panel_equity %>%
  dplyr::select(-c(INTERNET, US_bond_spread:Life_Exp, Equity_Liq:Dev_PC1)) %>% 
  dplyr::filter(Year %in% year_bal)

bal_eq_int_use <- c()

for (i in 1:length(name_country_equity))
{
  temp_new <- temp_int_use[which(temp_int_use$Country == name_country_equity[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_eq_int_use <- rbind(bal_eq_int_use, temp_new)
  }
}

attach(bal_eq_int_use)

Bal_Equity_int_use <- panel_est(Equity_int_use_bal, bal_eq_int_use)

Bal_Equity_int_use_Pre <- panel_est(Equity_int_use_bal_pre, subset(bal_eq_int_use, Year %in% year_bal_1))
Bal_Equity_int_use_Post <- panel_est(Equity_int_use_bal_post, subset(bal_eq_int_use, Year %in% year_bal_2))

Bal_Equity_int_use_Dev <- panel_est(Equity_int_use_bal, subset(bal_eq_int_use, Country %in% name_eq_developed))
Bal_Equity_int_use_Emerg <- panel_est(Equity_int_use_bal, subset(bal_eq_int_use, Country %in% name_eq_emerging))
#Bal_Equity_New_Front <- panel_est(Equity_new_bal, subset(bal_eq_new, Country %in% name_eq_frontier))


detach(bal_eq_int_use)


##########################################################################################################

### Bond Panels Balanced #################################################################

### With country level developmental PC1 in RHS

temp_devpc <- Panel_bond %>%
  dplyr::select(-c(INTERNET, US_bond_spread:Bond_Liq)) %>%
  dplyr::filter(Year %in% year_bal)

bal_b_devpc <- c()

for (i in 1:length(name_country_bond))
{
  temp_new <- temp_devpc[which(temp_devpc$Country == name_country_bond[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_b_devpc <- rbind(bal_b_devpc, temp_new)
  }
}

attach(bal_b_devpc)

Bal_Bond_devpc <- panel_est(Bond_devpc_bal, bal_b_devpc)

Bal_Bond_devpc_Pre <- panel_est(Bond_devpc_bal_pre, subset(bal_b_devpc, Year %in% year_bal_1))
Bal_Bond_devpc_Post <- panel_est(Bond_devpc_bal_post, subset(bal_b_devpc, Year %in% year_bal_2))

detach(bal_b_devpc)

### With country level internet usage in RHS

temp_int_use <- Panel_bond %>%
  dplyr::select(-c(INTERNET, US_bond_spread:Life_Exp, Bond_Liq:Dev_PC1)) %>% 
  dplyr::filter(Year %in% year_bal)

bal_b_int_use <- c()

for (i in 1:length(name_country_bond))
{
  temp_new <- temp_int_use[which(temp_int_use$Country == name_country_bond[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_b_int_use <- rbind(bal_b_int_use, temp_new)
  }
}

attach(bal_b_int_use)

Bal_Bond_int_use <- panel_est(Bond_int_use_bal, bal_b_int_use)

Bal_Bond_int_use_Pre <- panel_est(Bond_int_use_bal_pre, subset(bal_b_int_use, Year %in% year_bal_1))
Bal_Bond_int_use_Post <- panel_est(Bond_int_use_bal_post, subset(bal_b_int_use, Year %in% year_bal_2))

detach(bal_b_int_use)


##########################################################################################################

##########################################################################################################

### REIT Panels Balanced #################################################################

### With country level developmental PC1 in RHS

temp_devpc <- Panel_REIT %>%
  dplyr::select(-c(INTERNET, US_bond_spread:REIT_Liq)) %>%
  dplyr::filter(Year %in% year_bal)

bal_r_devpc <- c()

for (i in 1:length(name_country_REIT))
{
  temp_new <- temp_devpc[which(temp_devpc$Country == name_country_REIT[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_r_devpc <- rbind(bal_r_devpc, temp_new)
  }
}

attach(bal_r_devpc)

Bal_REIT_devpc <- panel_est(REIT_devpc_bal, bal_r_devpc)

Bal_REIT_devpc_Pre <- panel_est(REIT_devpc_bal_pre, subset(bal_r_devpc, Year %in% year_bal_1))
Bal_REIT_devpc_Post <- panel_est(REIT_devpc_bal_post, subset(bal_r_devpc, Year %in% year_bal_2))

detach(bal_r_devpc)

### With country level internet usage in RHS

temp_int_use <- Panel_REIT %>%
  dplyr::select(-c(INTERNET, US_bond_spread:Life_Exp, REIT_Liq:Dev_PC1)) %>% 
  dplyr::filter(Year %in% year_bal)

bal_r_int_use <- c()

for (i in 1:length(name_country_REIT))
{
  temp_new <- temp_int_use[which(temp_int_use$Country == name_country_REIT[i]), ]
  
  if (all(is.na(temp_new) == F)) #If no missing values
  {
    bal_r_int_use <- rbind(bal_r_int_use, temp_new)
  }
}

attach(bal_r_int_use)

Bal_REIT_int_use <- panel_est(REIT_int_use_bal, bal_r_int_use)

Bal_REIT_int_use_Pre <- panel_est(REIT_int_use_bal_pre, subset(bal_r_int_use, Year %in% year_bal_1))
Bal_REIT_int_use_Post <- panel_est(REIT_int_use_bal_post, subset(bal_r_int_use, Year %in% year_bal_2))

detach(bal_r_int_use)


##########################################################################################################

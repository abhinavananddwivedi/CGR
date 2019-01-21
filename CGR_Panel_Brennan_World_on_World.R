### CGR Script, Michael Brennan Regressions---World Diversification on World Factors 

### Libraries ####################################################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)
library(sandwich)

##################################################################################

### Directory Management #########################################################

#folder <- "July_2017"
#folder <- "Data_July_2017"
#file_path <- paste0(folder, "/")

##################################################################################

## Parse and post process LHS and RHS data tidily ################################

file_path_script_process <- "CGR_Panel_Reg_Add_201707_Post_Processing.R" # Post Process
#file_path_script_process <- paste0(folder, "/", file_script_process)

# Run the prior postprocessing file, note that it in turn sources the 
# script file that tidily reads and parses the data
source(file_path_script_process, echo = F) # Post-processed data files

year_LHS <- 1986:2012

##################################################################################

### Country Names ########################################

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

#### Special 12 Country Results ############################################################

index_special_12 <- c(2, 6, 10, 23, 24, 34, 36, 53, 54, 72, 85, 86)
name_special_12 <- name_country_equity[index_special_12] 

### Brennan Regressions Follow #############################################################

## EQUITY
# World Avg Diversification and World Factors

LHS_world_eq <- LHS_equity %>% dplyr::select(-Year) %>% 
  rowMeans(., na.rm = T) %>% as_tibble(.) %>% 
  dplyr::mutate(., year_LHS) %>% 
  dplyr::select(year_LHS, everything()) %>%
  dplyr::rename(`Year` = year_LHS,
                `World_Avg` = value)

data_world_ols_eq <- dplyr::left_join(LHS_world_eq, RHS_common, by = "Year") %>%
  dplyr::select(-c(`BAA-AAA`, Year))

# Developed world avg diversifcation and world factors

LHS_dev_eq <- LHS_equity %>% dplyr::select(-Year) %>%
  dplyr::select(index_eq_developed) %>%
  rowMeans(., na.rm = T) %>% as_tibble(.) %>%
  dplyr::mutate(., year_LHS) %>%
  dplyr::select(year_LHS, everything()) %>%
  dplyr::rename(`Year` = year_LHS,
                `Dev_Avg` = value)

data_dev_ols_eq <- dplyr::left_join(LHS_dev_eq, RHS_common, by = "Year") %>%
  dplyr::select(-c(`BAA-AAA`, Year))

# Emerging world avg diversifcation and world factors

LHS_emerg_eq <- LHS_equity %>% dplyr::select(-Year) %>%
  dplyr::select(index_eq_emerging) %>%
  rowMeans(., na.rm = T) %>% as_tibble(.) %>%
  dplyr::mutate(., year_LHS) %>%
  dplyr::select(year_LHS, everything()) %>%
  dplyr::rename(`Year` = year_LHS,
                `Emerg_Avg` = value)

data_emerg_ols_eq <- dplyr::left_join(LHS_emerg_eq, RHS_common, by = "Year") %>%
  dplyr::select(-c(`BAA-AAA`, Year))

# Frontier world avg diversifcation and world factors

LHS_front_eq <- LHS_equity %>% dplyr::select(-Year) %>%
  dplyr::select(index_eq_frontier) %>%
  rowMeans(., na.rm = T) %>% as_tibble(.) %>%
  dplyr::mutate(., year_LHS) %>%
  dplyr::select(year_LHS, everything()) %>%
  dplyr::rename(`Year` = year_LHS,
                `Front_Avg` = value)

data_front_ols_eq <- dplyr::left_join(LHS_front_eq, RHS_common, by = "Year") %>%
  dplyr::select(-c(`BAA-AAA`, Year))

## BOND
# World Avg Diversification and World Factors

LHS_world_b <- LHS_bond %>% dplyr::select(-Year) %>% 
  rowMeans(., na.rm = T) %>% as_tibble(.) %>% 
  dplyr::mutate(., year_LHS) %>% 
  dplyr::select(year_LHS, everything()) %>%
  dplyr::rename(`Year` = year_LHS,
                `World_Avg` = value)

data_world_ols_b <- dplyr::left_join(LHS_world_b, RHS_common, by = "Year") %>%
  dplyr::select(-c(`BAA-AAA`, Year))

## REIT
# World Avg Diversification and World Factors

LHS_world_r <- LHS_REIT %>% dplyr::select(-Year) %>% 
  rowMeans(., na.rm = T) %>% as_tibble(.) %>% 
  dplyr::mutate(., year_LHS) %>% 
  dplyr::select(year_LHS, everything()) %>%
  dplyr::rename(`Year` = year_LHS,
                `World_Avg` = value)

data_world_ols_r <- dplyr::left_join(LHS_world_r, RHS_common, by = "Year") %>%
  dplyr::select(-c(`BAA-AAA`, Year))

### Total Average Across All Asset Classes---Equity, Bonds and REITs

LHS_world_all <- dplyr::full_join(LHS_equity, LHS_bond, by = "Year") %>%
  dplyr::full_join(., LHS_REIT, by = "Year") %>%
  dplyr::select(-Year) %>%
  rowMeans(., na.rm = T) %>% 
  as_tibble(.) %>%
  dplyr::mutate(., year_LHS) %>% 
  dplyr::select(year_LHS, everything()) %>%
  dplyr::rename(`Year` = year_LHS,
                `World_Avg_All` = value)

data_world_ols_all <- dplyr::left_join(LHS_world_all, RHS_common, by = "Year") %>%
  dplyr::select(-c(`BAA-AAA`, Year))

### Special 12 Average Across All Asset Classes

temp_eq <- LHS_equity_long %>% dplyr::filter(., Country %in% name_special_12)
temp_b <- LHS_bond_long %>% dplyr::filter(., Country %in% name_special_12)
temp_r <- LHS_REIT_long %>% dplyr::filter(., Country %in% name_special_12)

temp_sp_12_all <- dplyr::full_join(temp_eq, temp_b, by = c("Country", "Year")) %>%
  dplyr::full_join(., temp_r, by = c("Country", "Year"))

temp_sp_12_avg <- temp_sp_12_all %>% dplyr::select(-c(Country, Year)) %>%
  rowMeans(., na.rm = T) %>% dplyr::as_tibble(.) %>%
  dplyr::bind_cols(temp_sp_12_all, .) %>%
  dplyr::rename(`Diversification_Average_Special_12` = value)

data_special_12_all <- temp_sp_12_avg %>% 
  dplyr::select(Country, Year, Diversification_Average_Special_12) %>% 
  left_join(., RHS_common, by = "Year") %>% 
  dplyr::select(-c(`BAA-AAA`))

## Formulas for Brennan regressions

Form_world <- World_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Form_dev <- Dev_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Form_emerg <- Emerg_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Form_front <- Front_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro

Form_world_all <- World_Avg_All ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro

Form_special_12_all <- Diversification_Average_Special_12 ~ TED + VIX + SENT + FEDFUNDS + 
  INTERNET + ERM + Euro


## Linear Models for Brennan Regressions

lm_est <- function(form, data_ols)
{
  temp_est <- lm(formula = form, data = data_ols, na.action = na.omit)
  #temp_est_rob <- lmtest::coeftest(temp_est, vcov. = NeweyWest)
  
  temp_out <- summary(temp_est)
  #temp_out$coefficients <- unclass(temp_est_rob)
  
  return(temp_out)
}


# Equity Regressions

lm_world_eq <- lm_est(Form_world, data_world_ols_eq)
lm_dev_eq <- lm_est(Form_dev, data_dev_ols_eq)
lm_emerg_eq <- lm_est(Form_emerg, data_emerg_ols_eq)
lm_front_eq <- lm_est(Form_front, data_front_ols_eq)

# Bond

lm_world_b <- lm_est(Form_world, data_world_ols_b)

# REIT

lm_world_r <- lm_est(Form_world, data_world_ols_r)

# All assets together

lm_world_all <- lm_est(Form_world_all, data_world_ols_all)

# Special 12 Countries OLS Regressions 

res_sp_12 <- list()

for (i in 1:length(name_special_12))
{
  temp_res <- data_special_12_all[which(data_special_12_all$Country == name_special_12[i]), ] %>%
    dplyr::select(-c(Country, Year))
  
  lm_sp_12 <- lm_est(Form_special_12_all, temp_res)
  
  res_sp_12[[i]] <- lm_sp_12
}

names(res_sp_12) <- name_special_12


###############################################################################
### Changes post 201901 #######################################################
###############################################################################

####################################
### Change on change regressions ###
####################################

data_world_ols_all_change <- data_world_ols_all %>%
  dplyr::select(-c(ERM, Euro)) %>% #ignore dummies
  apply(., 2, diff) %>% #difference all columns
  tibble::as_tibble()

Form_world_all_change <- World_Avg_All ~ TED + VIX + SENT + FEDFUNDS + INTERNET

lm_world_all_change <- lm_est(Form_world_all_change, data_world_ols_all_change)

# Special 12 regressions

Form_sp_12_change <- Diversification_Average_Special_12 ~ TED + VIX + SENT + FEDFUNDS + 
  INTERNET 

res_sp_12_change <- list()

for (i in 1:length(name_special_12))
{
  temp_res_change <- data_special_12_all[which(data_special_12_all$Country == 
                                                 name_special_12[i]), ] %>%
    dplyr::select(-c(Country, Year, ERM, Euro)) %>%
    apply(., 2, diff) %>%
    tibble::as_tibble(.)
  
  lm_sp_12_change <- lm_est(Form_sp_12_change, temp_res_change)
  
  res_sp_12_change[[i]] <- lm_sp_12_change
}

names(res_sp_12_change) <- name_special_12

################################
### Level on lag regressions ###
################################

func_lag <- function(vec, lag_length = 1)
{
  # This function accepts a vector and a lag length
  # and returns the lagged vector of the same length
  # with NA appended at the end
  len <- length(vec)
  temp <- vec[lag_length+1:len]
  
  return(temp)
}

lag_columns_RHS <- data_world_ols_all %>%
  dplyr::select(TED:INTERNET) %>% 
  apply(., 2, func_lag) %>%
  tibble::as_tibble(.)

data_world_ols_all_level_lag <- lag_columns_RHS %>%
  tibble::add_column(World_Avg_All = data_world_ols_all$World_Avg_All) %>%
  dplyr::select(World_Avg_All, everything())

Form_world_all_level_lag <- World_Avg_All ~ TED + VIX + SENT + FEDFUNDS + INTERNET

lm_world_all_level_lag <- lm_est(Form_world_all_level_lag, 
                                 data_world_ols_all_level_lag)

# Special 12 regressions

Form_sp_12_level_lag <- Div_Ind ~ TED + VIX + SENT + FEDFUNDS + INTERNET 

res_sp_12_level_lag <- list()

for (i in 1:length(name_special_12))
{
  columns_RHS_level_lag <- data_special_12_all[which(data_special_12_all$Country == 
                                                 name_special_12[i]), ] %>%
    dplyr::select(TED:INTERNET) %>%
    apply(., 2, func_lag) %>%
    tibble::as_tibble(.) 
  
  columns_LHS_level_lag <- data_special_12_all[which(data_special_12_all$Country == 
                                                       name_special_12[i]), 
                                               'Diversification_Average_Special_12'] %>%
    tibble::as_tibble(.)
  
  temp_res_level_lag <- columns_RHS_level_lag %>%
    tibble::add_column(Div_Ind = columns_LHS_level_lag$Diversification_Average_Special_12) %>%
    dplyr::select(Div_Ind, everything()) 
  
  lm_sp_12_level_lag <- lm_est(Form_sp_12_level_lag, temp_res_level_lag)
  
  res_sp_12_level_lag[[i]] <- lm_sp_12_level_lag
}

names(res_sp_12_level_lag) <- name_special_12



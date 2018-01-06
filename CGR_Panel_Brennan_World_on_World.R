### CGR Script, Michael Brennan Regressions---World Diversification on World Factors 

### Libraries ####################################################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)
library(sandwich)

##################################################################################

### Directory Management #########################################################

folder <- "July_2017"
subfolder <- "Data_July_2017"
file_path <- paste0(folder, "/", subfolder, "/")

##################################################################################

## Parse and post process LHS and RHS data tidily ################################

file_script_process <- "CGR_Panel_Reg_Add_201707_Post_Processing.R" # Post Process
file_path_script_process <- paste0(folder, "/", file_script_process)

# Run the prior postprocessing file, note that it in turn sources the 
# script file that tidily reads and parses the data
source(file_path_script_process, echo = F) # Post-processed data files

##################################################################################

### Emerging and developed equity markets ########################################

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

############################################################################################

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


## Formulas for Brennan regressions

Form_world <- World_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Form_dev <- Dev_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Form_emerg <- Emerg_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro
Form_front <- Front_Avg ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro

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
### Replicate and Extend Panel Estimation for CGR post 201707

### Libraries ####################################################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)

###

### Directory Management #########################################################

folder <- "July_2017"
subfolder <- "Data_July_2017"
file_path <- paste0(folder, "/", subfolder, "/")

###

## Parse and post process LHS and RHS data tidily ################################

file_script_process <- "CGR_Panel_Reg_Add_201707_Post_Processing.R" # Post Process
file_path_script_process<- paste0(folder, "/", file_script_process)

# Run the prior postprocessing file, note that it in turn sources the 
# script file that tidily reads and parses the data
source(file_path_script_process, echo = F) # Post-processed data files

##################################################################################

### Full Panel Estimation Formulas (Unbalanced)

Equity_old <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro

Bond_old <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro

REIT_old <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro

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

###

### Emerging and developed equity markets ##############################################

name_country_equity <- LHS_equity %>%
  dplyr::select(-Year) %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK,
                `United Arab Emirates` = UAE
                ) %>%
  colnames()

name_country_bond <- LHS_bond %>%
  dplyr::select(-Year) %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK
                ) %>%
  colnames()

name_country_REIT <- LHS_REIT %>%
  dplyr::select(-Year) %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK
                ) %>%
  colnames()

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

### Partitioning Years ##################################################################

year_liq <- 1985:2016
year_pol <- 1984:2013
year_dev <- 1980:2016
year_pre_00 <- 1986:1999
year_post_00 <- 2000:2012
year_LHS <- 1986:2012
year_bal <- 1996:2010
year_bal_1 <- 1996:2003
year_bal_2 <- 2004:2010

### Panel Estimation Begins #############################################################

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


### Equity Panel Estimations Full ########################################################

attach(data_old_equity)
### OLD MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro (commmon RHS) ####

## Old Full (Table 7)

Panel_Equity_Old <- plm::plm(formula = Equity_old,
                             data = data_old_equity,
                             model = "within",
                             subset = Country %in% name_country_equity,
                             index = c("Country", "Year")
                             )
Panel_Equity_Old_Robust <- lmtest::coeftest(Panel_Equity_Old,
                                            vcov = vcovHC(Panel_Equity_Old,
                                                          type = "HC0",
                                                          cluster = "group"
                                                          )
                                            )

## Old Full Pre 2000 (Table 9)

Panel_Equity_Old_Pre_00 <- plm::plm(formula = Equity_old,
                                    data = data_old_equity,
                                    model = "within",
                                    subset = Year %in% year_pre_00,
                                    index = c("Country", "Year")
                                    )
Panel_Equity_Old_Robust_Pre_00 <- lmtest::coeftest(Panel_Equity_Old_Pre_00,
                                                   vcov = vcovHC(Panel_Equity_Old_Pre_00,
                                                                 type = "HC0",
                                                                 cluster = "group"
                                                                 )
                                                   )

## Old Full Post 2000 (Table 9)

Panel_Equity_Old_Post_00 <- plm::plm(formula = Equity_old,
                                     data = data_old_equity,
                                     model = "within",
                                     subset = Year %in% year_post_00,
                                     index = c("Country", "Year")
                                     )
Panel_Equity_Old_Robust_Post_00 <- lmtest::coeftest(Panel_Equity_Old_Post_00,
                                                    vcov = vcovHC(Panel_Equity_Old_Post_00,
                                                                  type = "HC0",
                                                                  cluster = "group"
                                                                  )
                                                    )

## Old Emerging (Table 8)

Panel_Equity_Old_Emerging <- plm::plm(formula = Equity_old,
                                      data = data_old_equity,
                                      model = "within",
                                      subset = Country %in% name_eq_emerging,
                                      index = c("Country", "Year")
                                      )
Panel_Equity_Old_Robust_Emerging <- lmtest::coeftest(Panel_Equity_Old_Emerging,
                                                     vcov = vcovHC(Panel_Equity_Old_Emerging,
                                                                   type = "HC0",
                                                                   cluster = "group"
                                                                   )
                                                     )

## Old Developed (Table 8)

Panel_Equity_Old_Dev <- plm::plm(formula = Equity_old,
                                 data = data_old_equity,
                                 model = "within",
                                 subset = Country %in% name_eq_developed,
                                 index = c("Country", "Year")
                                 )
Panel_Equity_Old_Robust_Dev <- lmtest::coeftest(Panel_Equity_Old_Dev,
                                                vcov = vcovHC(Panel_Equity_Old_Dev,
                                                              type = "HC0",
                                                              cluster = "group"
                                                              )
                                                )

detach(data_old_equity)
#########################################################################################

### NEW MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro + NEW STUFF ######

attach(data_new_equity)
## New Full (Table 7)

Panel_Equity_New <- plm::plm(formula = Equity_new,
                             data = data_new_equity,
                             model = "within",
                             subset = Country %in% name_country_equity,
                             index = c("Country", "Year")
                             )
Panel_Equity_New_Robust <- lmtest::coeftest(Panel_Equity_New,
                                            vcov = vcovHC(Panel_Equity_New,
                                                          type = "HC0",
                                                          cluster = "group"
                                                          )
                                            )

## New Full Pre 2000 (Table 9)

Panel_Equity_New_Pre_00 <- plm::plm(formula = Equity_new,
                                    data = data_new_equity,
                                    model = "within",
                                    subset = Year %in% year_pre_00,
                                    index = c("Country", "Year")
                                    )
Panel_Equity_New_Robust_Pre_00 <- lmtest::coeftest(Panel_Equity_New_Pre_00,
                                                   vcov = vcovHC(Panel_Equity_New_Pre_00,
                                                                 type = "HC0",
                                                                 cluster = "group"
                                                                 )
                                                   )

## New Full Post 2000 (Table 9)

Panel_Equity_New_Post_00 <- plm::plm(formula = Equity_new,
                                     data = data_new_equity,
                                     model = "within",
                                     subset = Year %in% year_post_00,
                                     index = c("Country", "Year")
                                     )
Panel_Equity_New_Robust_Post_00 <- lmtest::coeftest(Panel_Equity_New_Post_00,
                                                    vcov = vcovHC(Panel_Equity_New_Post_00,
                                                                  type = "HC0",
                                                                  cluster = "group"
                                                                  )
                                                    )

## New Emerging (Table 8)

Panel_Equity_New_Emerging <- plm::plm(formula = Equity_new,
                                      data = data_new_equity,
                                      model = "within",
                                      subset = Country %in% name_eq_emerging,
                                      index = c("Country", "Year")
                                      )
Panel_Equity_New_Robust_Emerging <- lmtest::coeftest(Panel_Equity_New_Emerging,
                                                     vcov = vcovHC(Panel_Equity_New_Emerging,
                                                                   type = "HC0",
                                                                   cluster = "group"
                                                                   )
                                                     )

## New Developed (Table 8)

Panel_Equity_New_Dev <- plm::plm(formula = Equity_new,
                                 data = data_new_equity,
                                 model = "within",
                                 subset = Country %in% name_eq_developed,
                                 index = c("Country", "Year")
                                 )
Panel_Equity_New_Robust_Dev <- lmtest::coeftest(Panel_Equity_New_Dev,
                                                vcov = vcovHC(Panel_Equity_New_Dev,
                                                              type = "HC0",
                                                              cluster = "group"
                                                              )
                                                )


detach(data_new_equity)
##########################################################################################################

### Bond Panel Estimations ################################################################
#attach(Panel_bond)
attach(data_old_bond)
### OLD MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro (commmon RHS) #####

## Old Full (Table 7)

Panel_Bond_Old <- plm::plm(formula = Bond_old,
                           data = data_old_bond,
                           model = "within",
                           subset = Country %in% name_country_bond,
                           index = c("Country", "Year")
                           )
Panel_Bond_Old_Robust <- lmtest::coeftest(Panel_Bond_Old,
                                          vcov = vcovHC(Panel_Bond_Old,
                                                        type = "HC0",
                                                        cluster = "group"
                                                        )
                                          )

## Old Full Pre 2000 (Table 9)

Panel_Bond_Old_Pre_00 <- plm::plm(formula = Bond_old,
                                  data = data_old_bond,
                                  model = "within",
                                  subset = Year %in% year_pre_00,
                                  index = c("Country", "Year")
                                  )
Panel_Bond_Old_Robust_Pre_00 <- lmtest::coeftest(Panel_Bond_Old_Pre_00,
                                                 vcov = vcovHC(Panel_Bond_Old_Pre_00,
                                                               type = "HC0",
                                                               cluster = "group"
                                                               )
                                                 )

## Old Full Post 2000 (Table 9)

Panel_Bond_Old_Post_00 <- plm::plm(formula = Bond_old,
                                   data = data_old_bond,
                                   model = "within",
                                   subset = Year %in% year_post_00,
                                   index = c("Country", "Year")
                                   )
Panel_Bond_Old_Robust_Post_00 <- lmtest::coeftest(Panel_Bond_Old_Post_00,
                                                  vcov = vcovHC(Panel_Bond_Old_Post_00,
                                                                type = "HC0",
                                                                cluster = "group"
                                                                )
                                                  )

detach(data_old_bond)
#########################################################################################

### NEW MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro + NEW STUFF #####

attach(data_new_bond)
## New Full (Table 7)

Panel_Bond_New <- plm::plm(formula = Bond_new,
                           data = data_new_bond,
                           model = "within",
                           subset = Country %in% name_country_bond,
                           index = c("Country", "Year")
                           )
Panel_Bond_New_Robust <- lmtest::coeftest(Panel_Bond_New,
                                          vcov = vcovHC(Panel_Bond_New,
                                                        type = "HC0",
                                                        cluster = "group"
                                                        )
                                          )

## New Full Pre 2000 (Table 9)

Panel_Bond_New_Pre_00 <- plm::plm(formula = Bond_new,
                                  data = data_new_bond,
                                  model = "within",
                                  subset = Year %in% year_pre_00,
                                  index = c("Country", "Year")
                                  )
Panel_Bond_New_Robust_Pre_00 <- lmtest::coeftest(Panel_Bond_New_Pre_00,
                                                 vcov = vcovHC(Panel_Bond_New_Pre_00,
                                                               type = "HC0",
                                                               cluster = "group"
                                                               )
                                                 )

## New Full Post 2000 (Table 9)

Panel_Bond_New_Post_00 <- plm::plm(formula = Bond_new,
                                   data = data_new_bond,
                                   model = "within",
                                   subset = Year %in% year_post_00,
                                   index = c("Country", "Year")
                                   )
Panel_Bond_New_Robust_Post_00 <- lmtest::coeftest(Panel_Bond_New_Post_00,
                                                  vcov = vcovHC(Panel_Bond_New_Post_00,
                                                                type = "HC0",
                                                                cluster = "group"
                                                                )
                                                  )


detach(data_new_bond)
#####################################################################################

### REIT Panel Estimations ##########################################################

#attach(Panel_REIT)
attach(data_old_REIT)
### OLD MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro (commmon RHS)####

## Old Full (Table 7)

Panel_REIT_Old <- plm::plm(formula = REIT_old,
                           data = data_old_REIT,
                           model = "within",
                           subset = Country %in% name_country_REIT,
                           index = c("Country", "Year")
                           )
Panel_REIT_Old_Robust <- lmtest::coeftest(Panel_REIT_Old,
                                          vcov = vcovHC(Panel_REIT_Old,
                                                        type = "HC0",
                                                        cluster = "group"
                                                        )
                                          )

## Old Full Pre 2000 (Table 9)

Panel_REIT_Old_Pre_00 <- plm::plm(formula = REIT_old,
                                  data = data_old_REIT,
                                  model = "within",
                                  subset = Year %in% year_pre_00,
                                  index = c("Country", "Year")
                                  )
Panel_REIT_Old_Robust_Pre_00 <- lmtest::coeftest(Panel_REIT_Old_Pre_00,
                                                 vcov = vcovHC(Panel_REIT_Old_Pre_00,
                                                               type = "HC0",
                                                               cluster = "group"
                                                               )
                                                 )

## Old Full Post 2000 (Table 9)

Panel_REIT_Old_Post_00 <- plm::plm(formula = REIT_old,
                                   data = data_old_REIT,
                                   model = "within",
                                   subset = Year %in% year_post_00,
                                   index = c("Country", "Year")
                                   )
Panel_REIT_Old_Robust_Post_00 <- lmtest::coeftest(Panel_REIT_Old_Post_00,
                                                  vcov = vcovHC(Panel_REIT_Old_Post_00,
                                                                type = "HC0",
                                                                cluster = "group"
                                                                )
                                                  )

detach(data_old_REIT)

### NEW MODEL LHS ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + Euro + NEW STUFF #####

attach(data_new_REIT)
## New Full (Table 7)

Panel_REIT_New <- plm::plm(formula = REIT_new,
                           data = data_new_REIT,
                           model = "within",
                           subset = Country %in% name_country_REIT,
                           index = c("Country", "Year")
                           )
Panel_REIT_New_Robust <- lmtest::coeftest(Panel_REIT_New,
                                          vcov = vcovHC(Panel_REIT_New,
                                                        type = "HC0",
                                                        cluster = "group"
                                                        )
                                          )

## New Full Pre 2000 (Table 9)

Panel_REIT_New_Pre_00 <- plm::plm(formula = REIT_new,
                                  data = data_new_REIT,
                                  model = "within",
                                  subset = Year %in% year_pre_00,
                                  index = c("Country", "Year")
                                  )
Panel_REIT_New_Robust_Pre_00 <- lmtest::coeftest(Panel_REIT_New_Pre_00,
                                                 vcov = vcovHC(Panel_REIT_New_Pre_00,
                                                               type = "HC0",
                                                               cluster = "group"
                                                               )
                                                 )

## New Full Post 2000 (Table 9)

Panel_REIT_New_Post_00 <- plm::plm(formula = REIT_new,
                                   data = data_new_REIT,
                                   model = "within",
                                   subset = Year %in% year_post_00,
                                   index = c("Country", "Year")
                                   )
Panel_REIT_New_Robust_Post_00 <- lmtest::coeftest(Panel_REIT_New_Post_00,
                                                  vcov = vcovHC(Panel_REIT_New_Post_00,
                                                                type = "HC0",
                                                                cluster = "group"
                                                                )
                                                  )

detach(data_new_REIT)

######################################################################################
## Balanced Panel Estimations ########################################################
######################################################################################

### Panel Estimation Formulas ########################################################

# Note that ERM is never a factor in balanced regressions: 1996--2010

Equity_old_bal <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

Bond_old_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

REIT_old_bal <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + INTERNET + Euro

Equity_new_bal <- LHS_div_eq ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Market_cap_to_GDP + 
  Num_public_firms + Trade_to_GDP + Turnover_Domest_Total + 
  Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk + Equity_Liq + Dev_PC1

Bond_new_bal <- LHS_div_b ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Market_cap_to_GDP + 
  Num_public_firms + Trade_to_GDP + Turnover_Domest_Total + 
  Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk + Bond_Liq + Dev_PC1

REIT_new_bal <- LHS_div_r ~ TED + VIX + SENT + FEDFUNDS + Euro + 
  US_bond_spread + Credit_to_GDP + Market_cap_to_GDP + 
  Num_public_firms + Trade_to_GDP + Turnover_Domest_Total + 
  Agg_Fin_Risk + Agg_Econ_Risk + Agg_Pol_Risk + REIT_Liq + Dev_PC1

#####################################################################################


### OLD MODELS ######################################################################

## Equity Panels Balanced ###########################################################

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

## Old Full (Table 7)

Bal_Equity_Old <- plm::plm(formula = Equity_old_bal,
                           data = bal_eq_old,
                           model = "within",
                           subset = Country %in% name_country_equity,
                           index = c("Country", "Year")
                           )
Bal_Equity_Old_Robust <- lmtest::coeftest(Bal_Equity_Old,
                                          vcov = vcovHC(Bal_Equity_Old,
                                                        type = "HC0",
                                                        cluster = "group"
                                                        )
                                          )

## Old Full Pre 2000 (Table 9)

Bal_Equity_Old_Pre_00 <- plm::plm(formula = Equity_old_bal,
                                  data = bal_eq_old,
                                  model = "within",
                                  subset = Year %in% year_bal_1,
                                  index = c("Country", "Year")
                                  )
Bal_Equity_Old_Robust_Pre_00 <- lmtest::coeftest(Bal_Equity_Old_Pre_00,
                                                 vcov = vcovHC(Bal_Equity_Old_Pre_00,
                                                               type = "HC0",
                                                               cluster = "group"
                                                               )
                                                 )

## Old Full Post 2000 (Table 9)

Bal_Equity_Old_Post_00 <- plm::plm(formula = Equity_old_bal,
                                   data = bal_eq_old,
                                   model = "within",
                                   subset = Year %in% year_bal_2,
                                   index = c("Country", "Year")
                                   )
Bal_Equity_Old_Robust_Post_00 <- lmtest::coeftest(Bal_Equity_Old_Post_00,
                                                  vcov = vcovHC(Bal_Equity_Old_Post_00,
                                                                type = "HC0",
                                                                cluster = "group"
                                                                )
                                                  )

## Old Emerging (Table 8)

Bal_Equity_Old_Emerging <- plm::plm(formula = Equity_old_bal,
                                    data = bal_eq_old,
                                    model = "within",
                                    subset = Country %in% name_eq_emerging,
                                    index = c("Country", "Year")
                                    )
Bal_Equity_Old_Robust_Emerging <- lmtest::coeftest(Bal_Equity_Old_Emerging,
                                                   vcov = vcovHC(Bal_Equity_Old_Emerging,
                                                                 type = "HC0",
                                                                 cluster = "group"
                                                                 )
                                                   )

## Old Developed (Table 8)

Bal_Equity_Old_Dev <- plm::plm(formula = Equity_old_bal,
                               data = bal_eq_old,
                               model = "within",
                               subset = Country %in% name_eq_developed,
                               index = c("Country", "Year")
                               )
Bal_Equity_Old_Robust_Dev <- lmtest::coeftest(Bal_Equity_Old_Dev,
                                              vcov = vcovHC(Bal_Equity_Old_Dev,
                                                            type = "HC0",
                                                            cluster = "group"
                                                            )
                                              )

detach(bal_eq_old)


### NEW MODELS  #############################################################

## Equity Panels Balanced ###################################################

temp <- Panel_equity %>%
  dplyr::select(-ERM) %>%
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

## New Full (Table 7)

Bal_Equity_New <- plm::plm(formula = Equity_new_bal,
                           data = bal_eq_new,
                           model = "within",
                           subset = Country %in% name_country_equity,
                           index = c("Country", "Year")
                           )
Bal_Equity_New_Robust <- lmtest::coeftest(Bal_Equity_New,
                                          vcov = vcovHC(Bal_Equity_New,
                                                        type = "HC0",
                                                        cluster = "group"
                                                        )
                                          )

## New Full Pre 2000 (Table 9)

Bal_Equity_New_Pre_00 <- plm::plm(formula = Equity_new_bal,
                                  data = bal_eq_new,
                                  model = "within",
                                  subset = Year %in% year_bal_1,
                                  index = c("Country", "Year")
                                  )
Bal_Equity_New_Robust_Pre_00 <- lmtest::coeftest(Bal_Equity_New_Pre_00,
                                                 vcov = vcovHC(Bal_Equity_New_Pre_00,
                                                               type = "HC0",
                                                               cluster = "group"
                                                               )
                                                 )

## New Full Post 2000 (Table 9)

Bal_Equity_New_Post_00 <- plm::plm(formula = Equity_new_bal,
                                   data = bal_eq_new,
                                   model = "within",
                                   subset = Year %in% year_bal_2,
                                   index = c("Country", "Year")
                                   )
Bal_Equity_New_Robust_Post_00 <- lmtest::coeftest(Bal_Equity_New_Post_00,
                                                  vcov = vcovHC(Bal_Equity_New_Post_00,
                                                                type = "HC0",
                                                                cluster = "group"
                                                                )
                                                  )

## New Emerging (Table 8)

Bal_Equity_New_Emerging <- plm::plm(formula = Equity_new_bal,
                                    data = bal_eq_new,
                                    model = "within",
                                    subset = Country %in% name_eq_emerging,
                                    index = c("Country", "Year")
                                    )
Bal_Equity_New_Robust_Emerging <- lmtest::coeftest(Bal_Equity_New_Emerging,
                                                   vcov = vcovHC(Bal_Equity_New_Emerging,
                                                                 type = "HC0",
                                                                 cluster = "group"
                                                                 )
                                                   )

## New Developed (Table 8)

Bal_Equity_New_Dev <- plm::plm(formula = Equity_new_bal,
                               data = bal_eq_new,
                               model = "within",
                               subset = Country %in% name_eq_developed,
                               index = c("Country", "Year")
                               )
Bal_Equity_New_Robust_Dev <- lmtest::coeftest(Bal_Equity_New_Dev,
                                              vcov = vcovHC(Bal_Equity_New_Dev,
                                                            type = "HC0",
                                                            cluster = "group"
                                                            )
                                              )

detach(bal_eq_new)

######################################################################################

## Bond Panels Balanced ##############################################################

### OLD MODELS FIRST #################################################################

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

## Old Full (Table 7)

Bal_Bond_Old <- plm::plm(formula = Bond_old_bal,
                         data = bal_b_old,
                         model = "within",
                         subset = Country %in% name_country_bond,
                         index = c("Country", "Year")
                         )
Bal_Bond_Old_Robust <- lmtest::coeftest(Bal_Bond_Old,
                                        vcov = vcovHC(Bal_Bond_Old,
                                                      type = "HC0",
                                                      cluster = "group"
                                                      )
                                        )

## Old Full Pre 2000 (Table 9)

Bal_Bond_Old_Pre_00 <- plm::plm(formula = Bond_old_bal,
                                data = bal_b_old,
                                model = "within",
                                subset = Year %in% year_bal_1,
                                index = c("Country", "Year")
                                )
Bal_Bond_Old_Robust_Pre_00 <- lmtest::coeftest(Bal_Bond_Old_Pre_00,
                                               vcov = vcovHC(Bal_Bond_Old_Pre_00,
                                                             type = "HC0",
                                                             cluster = "group"
                                                             )
                                               )

## Old Full Post 2000 (Table 9)

Bal_Bond_Old_Post_00 <- plm::plm(formula = Bond_old_bal,
                                 data = bal_b_old,
                                 model = "within",
                                 subset = Year %in% year_bal_2,
                                 index = c("Country", "Year")
                                 )
Bal_Bond_Old_Robust_Post_00 <- lmtest::coeftest(Bal_Bond_Old_Post_00,
                                                vcov = vcovHC(Bal_Bond_Old_Post_00,
                                                              type = "HC0",
                                                              cluster = "group"
                                                              )
                                                )

detach(bal_b_old)
####################################################################################

### NEW MODELS #####################################################################

## Bond Panels Balanced ############################################################

temp <- Panel_bond %>%
  dplyr::select(-ERM) %>%
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

## New Full (Table 7)

Bal_Bond_New <- plm::plm(formula = Bond_new_bal,
                         data = bal_b_new,
                         model = "within",
                         subset = Country %in% name_country_bond,
                         index = c("Country", "Year")
                         )
Bal_Bond_New_Robust <- lmtest::coeftest(Bal_Bond_New,
                                        vcov = vcovHC(Bal_Bond_New,
                                                      type = "HC0",
                                                      cluster = "group"
                                                      )
                                        )

## New Full Pre 2000 (Table 9)

Bal_Bond_New_Pre_00 <- plm::plm(formula = Bond_new_bal,
                                data = bal_b_new,
                                model = "within",
                                subset = Year %in% year_bal_1,
                                index = c("Country", "Year")
                                )
Bal_Bond_New_Robust_Pre_00 <- lmtest::coeftest(Bal_Bond_New_Pre_00,
                                               vcov = vcovHC(Bal_Bond_New_Pre_00,
                                                             type = "HC0",
                                                             cluster = "group"
                                                             )
                                               )

## New Full Post 2000 (Table 9)

Bal_Bond_New_Post_00 <- plm::plm(formula = Bond_new_bal,
                                 data = bal_b_new,
                                 model = "within",
                                 subset = Year %in% year_bal_2,
                                 index = c("Country", "Year")
                                 )
Bal_Bond_New_Robust_Post_00 <- lmtest::coeftest(Bal_Bond_New_Post_00,
                                                vcov = vcovHC(Bal_Bond_New_Post_00,
                                                              type = "HC0",
                                                              cluster = "group"
                                                              )
                                                )

detach(bal_b_new)

## REIT Panels Balanced ###############################################################

### OLD MODELS FIRST

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

## Old Full (Table 7)

Bal_REIT_Old <- plm::plm(formula = REIT_old_bal,
                         data = bal_r_old,
                         model = "within",
                         subset = Country %in% name_country_REIT,
                         index = c("Country", "Year")
                         )
Bal_REIT_Old_Robust <- lmtest::coeftest(Bal_REIT_Old,
                                        vcov = vcovHC(Bal_REIT_Old,
                                                      type = "HC0",
                                                      cluster = "group"
                                                      )
                                        )

## Old Full Pre 2000 (Table 9)

Bal_REIT_Old_Pre_00 <- plm::plm(formula = REIT_old_bal,
                                data = bal_r_old,
                                model = "within",
                                subset = Year %in% year_bal_1,
                                index = c("Country", "Year")
                                )
Bal_REIT_Old_Robust_Pre_00 <- lmtest::coeftest(Bal_REIT_Old_Pre_00,
                                               vcov = vcovHC(Bal_REIT_Old_Pre_00,
                                                             type = "HC0",
                                                             cluster = "group"
                                                             )
                                               )

## Old Full Post 2000 (Table 9)

Bal_REIT_Old_Post_00 <- plm::plm(formula = REIT_old_bal,
                                 data = bal_r_old,
                                 model = "within",
                                 subset = Year %in% year_bal_2,
                                 index = c("Country", "Year")
                                 )
Bal_REIT_Old_Robust_Post_00 <- lmtest::coeftest(Bal_REIT_Old_Post_00,
                                                vcov = vcovHC(Bal_REIT_Old_Post_00,
                                                              type = "HC0",
                                                              cluster = "group"
                                                              )
                                                )



detach(bal_r_old)

######################################################################################

### NEW MODELS ########################################################################

temp <- Panel_REIT %>%
  dplyr::select(-ERM) %>%
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

## New Full (Table 7)

Bal_REIT_New <- plm::plm(formula = REIT_new_bal,
                         data = bal_r_new,
                         model = "within",
                         subset = Country %in% name_country_REIT,
                         index = c("Country", "Year")
                         )
Bal_REIT_New_Robust <- lmtest::coeftest(Bal_REIT_New,
                                        vcov = vcovHC(Bal_REIT_New,
                                                      type = "HC0",
                                                      cluster = "group"
                                                      )
                                        )

## New Full Pre 2000 (Table 9)

Bal_REIT_New_Pre_00 <- plm::plm(formula = REIT_new_bal,
                                data = bal_r_new,
                                model = "within",
                                subset = Year %in% year_bal_1,
                                index = c("Country", "Year")
                                )
Bal_REIT_New_Robust_Pre_00 <- lmtest::coeftest(Bal_REIT_New_Pre_00,
                                               vcov = vcovHC(Bal_REIT_New_Pre_00,
                                                             type = "HC0",
                                                             cluster = "group"
                                                             )
                                               )

## New Full Post 2000 (Table 9)

Bal_REIT_New_Post_00 <- plm::plm(formula = REIT_new_bal,
                                 data = bal_r_new,
                                 model = "within",
                                 subset = Year %in% year_bal_2,
                                 index = c("Country", "Year")
                                 )
Bal_REIT_New_Robust_Post_00 <- lmtest::coeftest(Bal_REIT_New_Post_00,
                                                vcov = vcovHC(Bal_REIT_New_Post_00,
                                                              type = "HC0",
                                                              cluster = "group"
                                                              )
                                                )

detach(bal_r_new)


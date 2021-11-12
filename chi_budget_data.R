# this script retrieves and combines city of chicago budget ordinances between 2011 and 2021

library(tidyverse)
library(RSocrata)

rm(list = ls())

setwd("D:/GitHub/cpd_budget_analysis/cpd_budget_analysis")

# Full Budget Ordinances Appropriations -----------------------------------

# https://data.cityofchicago.org/browse?limitTo=datasets&q=Budget+Ordinance+-+Appropriations&sortBy=relevance

# create list of API links for city of chicago budget ordinances.
API_list_budget <- c("drv3-jzqp", # 2011
                     "8ix6-nb7q", # 2012
                     "b24i-nwag", # 2013
                     "ub6s-xy6e", # 2014
                     "qnek-cfpp", # 2015
                     "36y7-5nnf", # 2016
                     "7jem-9wyw", # 2017
                     "6g7p-xnsy", # 2018
                     "h9rt-tsn7", # 2019
                     "fyin-2vyd", # 2020
                     "6tbx-h7y2", # 2021
                     "ncj3-k47t") # 2022, recommended budget: https://data.cityofchicago.org/Administration-Finance/Budget-2022-Budget-Recommendations-Appropriations/ncj3-k47t

# define seed year
year = 2011

# create empty data frame to populate with for loop
budget_ordinances <- data.frame()

# build data.frame for all available budget ordinances
for (i in API_list_budget) {
  
  # build API call corresponding to each year
  x <- paste0("https://data.cityofchicago.org/resource/", i, ".json")
  
  # pull dataset from data.cityofchicago.org
  y <- read.socrata(x)
  
  # add year column seeding at 2011
  y$year = year
  
  # iterate years for each API call. since these are ordered in the list above, it will generate the correct value for each fiscal year of the relevant budget ordinance
  year = year + 1
  
  # combine all annual budgets into a single large data.frame
  budget_ordinances <- bind_rows(budget_ordinances, y)
}

# standardize variable names and values across years
budget_ordinances_clean <- budget_ordinances %>% 
  
  mutate(ordinance_amount = case_when(year == 2011 ~ amount,
                                      year == 2013 ~ appropriation_ordinance,
                                      TRUE ~ X_ordinance_amount_),
         ordinance_amount = as.numeric(ordinance_amount)
         )%>% 
  
  # remove redundant variables
  select(-c(department,
            amount,
            appropriation_ordinance,
            X_ordinance_amount_))

# save data
write_csv(budget_ordinances_clean, "data/budget_ordinances.csv")

# Positions and Salaries Appropriations --------------------------------------------------

# https://data.cityofchicago.org/browse?q=Budget%20Ordinance%20-%20Positions%20and%20Salaries&sortBy=relevance

# create list of API links

API_list_salaries <- c("g398-fhbm", # 2011
                       "4n2t-us8h", # 2012 
                       "78az-bt2s", # 2013
                       "etzw-ycze", # 2014
                       "f338-e9ns", # 2015
                       "ipsp-k4xh", # 2016
                       "vcfx-7p4u", # 2017
                       "9d7d-7f2b", # 2018
                       "7zkb-yr4j", # 2019
                       "txys-725h", # 2020
                       "gcwx-xm5a", # 2021
                       "kwap-s85k") # 2022 recommended budget: https://data.cityofchicago.org/Administration-Finance/Budget-2022-Budget-Recommendations-Positions-and-S/kwap-s85k

year = 2011

titles_salaries <- data.frame()

for (i in API_list_salaries){
  x <- paste0("https://data.cityofchicago.org/resource/", i, ".json")
  y <- read.socrata(x)
  y$year = year
  year = year + 1
  titles_salaries <- bind_rows(titles_salaries, y)
}

# standardize variable names and values across years
titles_salaries_clean <- titles_salaries %>% 
  
  mutate(
    
    # variable labels were different for several columns in 2011 
    schedule_grade = if_else(
      is.na(schedule_grade), schedule, schedule_grade),
    fund_description = if_else(
      is.na(fund_description), fund_name, fund_description),
    department_description = if_else(
      is.na(department_description), department_name, department_description),
    section_description = if_else(
      is.na(section_description), section_name, section_description),
    sub_section_code = if_else(
      is.na(sub_section_code), subsection_code, sub_section_code),
    sub_section_description = if_else(
      is.na(sub_section_description), subsection_name, sub_section_description),
    division_description = if_else(
      is.na(division_description), division_name, division_description),
    total_budgeted_unit = if_else(
      is.na(total_budgeted_unit), total_budgeted_units, total_budgeted_unit),
    
    # in 2012, department_code is labeled as department_number
    department_code = if_else(
      is.na(department_code), department_number, department_code)
    ) %>% 
  
  # remove redundant variables
  select(
    -c(department, # only appears in 2011
       schedule,
       fund_name,
       department_name,
       section_name,
       subsection_code,
       subsection_name,
       division_name,
       total_budgeted_units,
       department_number)
    ) %>% 
  
  # convert non-categorical variables to numeric
  mutate(total_budgeted_unit = as.numeric(total_budgeted_unit),
         budgeted_pay_rate = as.numeric(budgeted_pay_rate),
         total_budgeted_amount = as.numeric(total_budgeted_amount))

# save data
write_csv(titles_salaries_clean, "data/titles_salaries.csv")

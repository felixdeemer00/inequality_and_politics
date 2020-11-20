library(tidyverse)
library(readr)
library(readxl)

country_codes <- read_excel("final_project/raw_data/WIID_06MAY2020.xlsx") %>%
  select(c3, c2) %>%
  unique()

income_inequality <- read.csv("final_project/raw_data/WID_Data_Income.csv", sep = ";", skip = 1) %>%
  pivot_longer(cols = -c(Year, Percentile),
               names_to = "country",
               values_to = "values") %>%
  mutate(country = map_chr(country, 
                           ~ gsub(x = ., 
                                  pattern = "sptinc_z_", 
                                  replacement = ""))) %>%
  mutate(c2 = str_sub(country, 1, 2)) %>%
  select(-country) %>%
  inner_join(., country_codes) %>%
  rename(percentile = Percentile, year = Year, percent_income = values)

gdp_per_cap <- read.csv("final_project/raw_data/gdp_cap.csv", skip = 3) %>%
  pivot_longer(cols = X1960:X2019, names_to = "year", values_to = "gdppcap") %>%
  select(-c(X2020, X, Indicator.Name, Indicator.Code, Country.Name)) %>%
  mutate(year = map_chr(year, 
                           ~ gsub(x = ., 
                                  pattern = "X", 
                                  replacement = "")),
         year = map_dbl(year, as.integer)) %>%
  rename(c3 = Country.Code)

region_data <- read.csv("final_project/raw_data/gdp_cap_metadata.csv") %>%
  select(Country.Code, Region, IncomeGroup) %>%
  rename(c3 = Country.Code, region = Region, incomegroup = IncomeGroup)

political_institutions <- read_excel("final_project/raw_data/DPI_Data_Political_Institutions.xls")

pol_mod <- political_institutions %>%
  select(countryname, ifs, year, polariz, finittrm, military, prtyin, 
         execnat, maj, checks_lax, liec, eiec) %>%
  filter(year %in% 2004:2012, ifs != 0) %>%
  drop_na() %>%
  mutate(year = as.double(year)) %>%
  rename(c3 = ifs)

pol_and_ineq_mod <- left_join(pol_mod, income_inequality) %>%
  select(-countryname) %>%
  left_join(gdp_per_cap) %>%
  left_join(region_data) %>%
  drop_na()

saveRDS(pol_and_ineq_mod, file = "final_project/pol_and_ineq_mod")


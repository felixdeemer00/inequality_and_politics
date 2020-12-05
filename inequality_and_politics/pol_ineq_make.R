library(tidyverse)
library(readr)
library(readxl)
library(naniar)

country_codes <- read_excel("inequality_and_politics/raw_data/WIID_06MAY2020.xlsx") %>%
  select(c3, c2) %>%
  unique()

income_inequality <- read.csv("inequality_and_politics/raw_data/WID_Data_Income.csv", 
                              sep = ";", skip = 1) %>%
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

gdp_per_cap <- read.csv("inequality_and_politics/raw_data/gdp_cap.csv", 
                        skip = 3) %>%
  pivot_longer(cols = X1960:X2019, names_to = "year", values_to = "gdppcap") %>%
  select(-c(X2020, X, Indicator.Name, Indicator.Code, Country.Name)) %>%
  mutate(year = map_chr(year, 
                           ~ gsub(x = ., 
                                  pattern = "X", 
                                  replacement = "")),
         year = map_dbl(year, as.integer)) %>%
  rename(c3 = Country.Code)

region_data <- read.csv("inequality_and_politics/raw_data/gdp_cap_metadata.csv") %>%
  select(Country.Code, Region, IncomeGroup) %>%
  rename(c3 = Country.Code, region = Region, incomegroup = IncomeGroup)

political_institutions <- read_excel("inequality_and_politics/raw_data/DPI_Data_Political_Institutions.xls")

pol_mod <- political_institutions %>%
  select(countryname, ifs, year, polariz, finittrm, military, prtyin, 
         execnat, maj, checks_lax, liec, eiec) %>%
  filter(year %in% 2004:2012, ifs != 0) %>%
  drop_na() %>%
  mutate(year = as.double(year)) %>%
  rename(c3 = ifs)

pol_and_ineq_mod <- left_join(pol_mod, income_inequality) %>%
  select(-countryname) %>%
  drop_na() %>%
  left_join(gdp_per_cap) %>%
  left_join(region_data) %>%
  replace_with_na(replace = list(finittrm = -999,
                                 execnat = -999,
                                 prtyin = -999,
                                 military = -999,
                                 checks_lax = -999,
                                 eiec = -999,
                                 liec = -999,
                                 maj = "NA",
                                 polariz = "NA")) %>%
  mutate(region = as.factor(region),
         incomegroup = as.factor(incomegroup),
         maj = as.numeric(maj),
         polariz = as.factor(polariz),
         military = as.factor(military),
         percent_income = percent_income * 100)

saveRDS(pol_and_ineq_mod, file = "inequality_and_politics/rdsFiles/pol_and_ineq_mod")

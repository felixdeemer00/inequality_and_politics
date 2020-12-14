library(tidyverse)
library(readr)
library(readxl)
library(naniar)

# The country_codes object was created to provide a reference according to which
# the main two data sets could be joined - one only had 2-character country
# codes, while the other only had 3-character codes, so this allowed me to join
# the two regardless.

country_codes <- read_excel("inequality_and_politics/raw_data/WIID_06MAY2020.xlsx") %>%
  select(c3, c2) %>%
  unique()

# When creating this object, the column names were formatted very strangely,
# with a large amount of unnecessary text. The gsub() function was used to
# find these patterns and remove them. The columns were then joined with the
# country_code data set prior to the final join, and were renamed to match with
# the other data set.

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

# These objects were primarily used for region and income group classifications,
# as the GDP/Capita measure was not used for the final model in the end. The
# gsub() function was used to remove some strange column names formatting,
# which messed up the data after the pivot_longer() function was used.

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

# I renamed the variable names for the region_data object to aid with the
# left_join() used later, to standardize column names across objects to be
# joined.

region_data <- read.csv("inequality_and_politics/raw_data/gdp_cap_metadata.csv") %>%
  select(Country.Code, Region, IncomeGroup) %>%
  rename(c3 = Country.Code, region = Region, incomegroup = IncomeGroup)

political_institutions <- read_excel("inequality_and_politics/raw_data/DPI_Data_Political_Institutions.xls")

# I modified the data to only take values from 2000 on, so that the model will
# be more representative of the current political dynamics/effects of 
# inequality, as this could potentially have shifted across decades.

pol_mod <- political_institutions %>%
  select(countryname, ifs, year, polariz, finittrm, military, prtyin, 
         execnat, maj, checks_lax, liec, eiec) %>%
  filter(year > 1990, ifs != 0) %>%
  drop_na() %>%
  mutate(year = as.double(year)) %>%
  rename(c3 = ifs)

# The political institutions dataset had lots of values set to -999, which I 
# replaced with NAs so that this wouldn't interfere with the data. I used the
# left_join() functions to join all of the data sets into the final, combined
# data set to be used in the models. 

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

# I used the saveRDS() function to save the final output in the folder, to avoid 
# the entire code chunk having to be repeated each time.

saveRDS(pol_and_ineq_mod, file = "inequality_and_politics/rdsFiles/pol_and_ineq_mod")

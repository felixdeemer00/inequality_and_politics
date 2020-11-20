library(tidyverse)
library(readr)
library(readxl)
library(tidymodels)

pol_and_ineq_mod <- readRDS("final_project/pol_and_ineq_mod")

paim_split <- initial_split(pol_and_ineq_mod %>%
                              filter(percentile == "p99p100"), 
                            prob = 0.8)
paim_train <- training(paim_split)
paim_test <- testing(paim_split)
paim_folds <- vfold_cv(paim_train)

paim_wflow_1 <- workflow() %>%
  add_recipe(recipe(percent_income ~ maj + checks_lax + military,
             data = paim_split)) %>%
  add_model(linear_reg() %>%
              set_engine("lm"))

paim_model_1 <- paim_wflow_1 %>%
  fit(data = paim_train) %>%
  print(digits = 5)

dat_a <- pol_and_ineq_mod %>%
  filter(polariz != "NA" & percentile != "p99p100") %>%
  ggplot(aes(x = as.numeric(polariz), 
             y = percent_income, 
             color = percentile)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "How does income inequality relate to polarization?") +
  facet_wrap( ~ incomegroup)

dat_b <- pol_and_ineq_mod %>%
  filter(maj != "NA" & percentile != "p99p100") %>%
  ggplot(aes(x = as.numeric(maj), 
             y = percent_income,
             color = percentile)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "How does income inequality relate to legislative majorities?") +
  facet_wrap( ~ incomegroup)

saveRDS(dat_a, file = "final_project/dat_a")
saveRDS(dat_a, file = "final_project/dat_b")
library(tidyverse)
library(readr)
library(readxl)
library(tidymodels)
library(rstanarm)

pol_and_ineq_mod <- readRDS("final_project/pol_and_ineq_mod") %>%
  drop_na()

paim_split <- initial_split(pol_and_ineq_mod %>%
                              filter(percentile == "p99p100"), 
                            prob = 0.8)
paim_train <- training(paim_split)
paim_test <- testing(paim_split)
paim_folds <- vfold_cv(paim_train)

paim_wflow_1 <- workflow() %>%
  add_recipe(recipe(percent_income ~ maj + checks_lax + 
                    polariz + incomegroup + gdppcap + region,
             data = paim_split) %>%
               step_dummy(all_nominal()) %>%
               step_interact(~ maj:starts_with("incomegroup"))) %>%
  add_model(linear_reg() %>%
              set_engine("stan"))

paim_wflow_2 <- workflow() %>%
  add_recipe(recipe(percent_income ~ region + incomegroup,
                    data = paim_split) %>%
               step_dummy(all_nominal())) %>%
  add_model(linear_reg() %>%
              set_engine("stan"))

paim_wflow_3 <- workflow() %>%
  add_recipe(recipe(percent_income ~ maj + checks_lax + polariz,
                    data = paim_split) %>%
               step_dummy(all_nominal()) %>%
               step_interact(~ maj:polariz)) %>%
  add_model(linear_reg() %>%
              set_engine("stan"))

# metrics_1 <- paim_wflow_1 %>% 
#   fit_resamples(resamples = paim_folds) %>% 
#   collect_metrics()
# 
metrics_2 <- paim_wflow_2 %>%
  fit_resamples(resamples = paim_folds) %>%
  collect_metrics()

model_2 <- stan_glm(percent_income ~ region + gdppcap*gdppcap - 1,
                    data = paim_train,
                    refresh = 0)

print(model_2, digits = 5)
# 
# metrics_3 <- paim_wflow_3 %>% 
#   fit_resamples(resamples = paim_folds) %>% 
#   collect_metrics()





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

dat_c <- pol_and_ineq_mod %>%
  filter(maj != "NA" & percentile != "p99p100" & 
           region %in% c("Middle East & North Africa", "Sub-Saharan Africa")) %>%
  ggplot(aes(x = as.numeric(military), 
             y = percent_income,
             color = percentile)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(title = "How does income inequality relate to military governments?") +
  facet_wrap( ~ region)

saveRDS(dat_a, file = "final_project/dat_a")
saveRDS(dat_b, file = "final_project/dat_b")
saveRDS(dat_c, file = "final_project/dat_c")
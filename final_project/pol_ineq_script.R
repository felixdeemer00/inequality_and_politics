library(tidyverse)
library(readr)
library(readxl)

pol_and_ineq_mod <- readRDS("final_project/pol_and_ineq_mod")

pol_and_ineq_mod %>%
  ggplot(aes(region)) +
    geom_bar()

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
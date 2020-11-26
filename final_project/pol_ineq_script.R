library(tidyverse)
library(readr)
library(readxl)
library(skimr)
library(tidymodels)
library(rstanarm)

pol_and_ineq_mod <- readRDS("final_project/pol_and_ineq_mod") %>%
  drop_na()

paim_split <- initial_split(pol_and_ineq_mod %>%
                            filter(percentile == "p0p50"), 
                            prop = 0.8)
paim_train <- training(paim_split)
paim_test <- testing(paim_split)
paim_folds <- vfold_cv(paim_train)

paim_wflow_1 <- workflow() %>%
  add_recipe(recipe(percent_income ~ 
                      liec + maj + checks_lax + execnat + polariz + 
                      incomegroup + 
                      gdppcap + 
                      region,
             data = paim_train) %>%
             step_dummy(all_nominal())) %>%
  add_model(linear_reg() %>%
              set_engine("stan"))

paim_wflow_2 <- workflow() %>%
  add_recipe(recipe(percent_income ~ 
                      liec + maj + checks_lax + execnat + polariz + 
                      incomegroup + 
                      gdppcap + 
                      region,
                    data = paim_train) %>%
               step_dummy(all_nominal()) %>%
               step_interact(~ starts_with("polariz"):starts_with("incomegroup"))) %>%
  add_model(linear_reg() %>%
              set_engine("stan"))

metrics_1 <- paim_wflow_1 %>% 
  fit_resamples(resamples = paim_folds) %>% 
  collect_metrics()

metrics_2 <- paim_wflow_2 %>% 
  fit_resamples(resamples = paim_folds) %>% 
  collect_metrics()

met <- bind_rows(metrics_1, metrics_2) %>%
  filter(.metric == "rmse")

# stan_glm(percent_income ~ 
#            liec + maj + checks_lax + execnat + polariz + 
#            incomegroup + 
#            gdppcap + 
#            region,
#          refresh = 0,
#          data = pol_and_ineq_mod %>% filter(percentile == "p0p50")) %>%
#   print(digits = 5)

# GRAPH A

# pola.a.labs <- c("Low Polarization", "Med. Polarization", "High Polarization")
# names(pola.a.labs) <- c("0", "1", "2")
# 
# perc.a.labs <- c("Bottom 50%", "Medium 40%", "Top 10%")
# names(perc.a.labs) <- c("p0p50", "p50p90", "p90p100")
# 
# dat_a <- pol_and_ineq_mod %>%
#   filter(polariz != "NA" & percentile != "p99p100") %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#   geom_density(alpha = 0.5, position = "identity") +
#   labs(title = "How does Income Inequality relate to Polarization?",
#        subtitle = "In high-polarization environments, the top 10% tend to have a smaller share of national income",
#        x = "Fraction of National Income Owned",
#        y = "Probability") +
#   facet_grid(
#     polariz ~ percentile, 
#     labeller = labeller(polariz = pola.a.labs, 
#                         percentile = perc.a.labs),
#     scales = "free"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # GRAPH B
# 
# maj.b.labs <- c("No Majority", "Majority", "Supermajority")
# names(maj.b.labs) <- c("0", "1", "2")
# 
# dat_b <- pol_and_ineq_mod %>%
#   filter(maj != "NA" & percentile != "p99p100") %>%
#   mutate(maj = case_when(maj < 0.5 ~ 0,
#                          maj < 0.66 ~ 1,
#                          TRUE ~ 2)) %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#     geom_density(alpha = 0.5, position = "identity") +
#     labs(title = "How does Income Inequality relate to Legislative Majorities?",
#          subtitle = "In countries with supermajorities, the top 10% tend to have a larger share of national income",
#          x = "Fraction of National Income Owned",
#          y = "Probability") +
#     facet_grid(
#       maj ~ percentile, 
#       labeller = labeller(maj = maj.b.labs, 
#                           percentile = perc.a.labs),
#       scales = "free"
#     ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # GRAPH C
# 
# mil.c.labs <- c("No Military Government", "Military Government")
# names(mil.c.labs) <- c("0", "1")
# 
# dat_c <- pol_and_ineq_mod %>%
#   filter(military != "NA" & percentile != "p99p100") %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#   geom_density(alpha = 0.5, position = "identity") +
#   labs(title = "How does Income Inequality relate to Military Government?",
#        subtitle = "Military governments tend to be associated with a top 10% with more national income.",
#        x = "Fraction of National Income Owned",
#        y = "Probability") +
#   facet_grid(
#     military ~ percentile, 
#     labeller = labeller(military = mil.c.labs, 
#                         percentile = perc.a.labs),
#     scales = "free"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # GRAPH D
# 
# fin.d.labs <- c("No Term Limits", "Term Limits")
# names(fin.d.labs) <- c("0", "1")
# 
# dat_d <- pol_and_ineq_mod %>%
#   filter(finittrm != "NA" & percentile != "p99p100") %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#   geom_density(alpha = 0.5, position = "identity") +
#   labs(title = "How does Income Inequality relate to Term Limits?",
#        subtitle = "In countries without term limits, the top 10% tend to have a greater share of national income.",
#        x = "Fraction of National Income Owned",
#        y = "Probability") +
#   facet_grid(
#     finittrm ~ percentile, 
#     labeller = labeller(finittrm = fin.d.labs, 
#                         percentile = perc.a.labs),
#     scales = "free"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # GRAPH E
# 
# nat.e.labs <- c("No Nationalist Executive", "Nationalist Executive")
# names(nat.e.labs) <- c("0", "1")
# 
# dat_e <- pol_and_ineq_mod %>%
#   filter(execnat != "NA" & percentile != "p99p100") %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#   geom_density(alpha = 0.5, position = "identity") +
#   labs(title = "How does Income Inequality relate to Nationalist Executives?",
#        subtitle = "Having a nationalist executive tends to be associated with a top 10% with a greater share of income.",
#        x = "Fraction of National Income Owned",
#        y = "Probability") +
#   facet_grid(
#     execnat ~ percentile, 
#     labeller = labeller(execnat = nat.e.labs, 
#                         percentile = perc.a.labs),
#     scales = "free"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # GRAPH F
# 
# che.f.labs <- c("0-33% of Che/Bal", "33-66% of Che/Bal", "66-99% of Che/Bal")
# names(che.f.labs) <- c("0", "1", "2")
# 
# dat_f <- pol_and_ineq_mod %>%
#   filter(checks_lax != "NA" & percentile != "p99p100") %>%
#   mutate(checks_lax = case_when(checks_lax < quantile(checks_lax, 0.33) ~ 0,
#                          checks_lax < quantile(checks_lax, 0.66) ~ 1,
#                          TRUE ~ 2)) %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#   geom_density(alpha = 0.5, position = "identity") +
#   labs(title = "How does Income Inequality relate to Checks and Balances?",
#        subtitle = "In countries with more checks and balances, the top 10% tend to have a smaller income share.",
#        x = "Fraction of National Income Owned",
#        y = "Probability") +
#   facet_grid(
#     checks_lax ~ percentile, 
#     labeller = labeller(checks_lax = che.f.labs, 
#                         percentile = perc.a.labs),
#     scales = "free"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # GRAPH G
# 
# lie.g.labs <- c("0-33% of LIEC", "33-66% of LIEC", "66-99% of LIEC")
# names(lie.g.labs) <- c("0", "1", "2")
# 
# dat_g <- pol_and_ineq_mod %>%
#   filter(liec != "NA" & percentile != "p99p100") %>%
#   mutate(liec = case_when(liec < 3 ~ 0,
#                                 liec < 5 ~ 1,
#                                 TRUE ~ 2)) %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#   geom_density(alpha = 0.5, position = "identity") +
#   labs(title = "How does Income Inequality relate to Legislative Competitiveness?",
#        subtitle = "Countries with more competitive legislative elections are more divergent in their inequality levels.",
#        x = "Fraction of National Income Owned",
#        y = "Probability") +
#   facet_grid(
#     liec ~ percentile, 
#     labeller = labeller(liec = lie.g.labs, 
#                         percentile = perc.a.labs),
#     scales = "free"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # GRAPH H
# 
# eie.h.labs <- c("0-33% of EIEC", "33-66% of EIEC", "66-99% of EIEC")
# names(eie.h.labs) <- c("0", "1", "2")
# 
# dat_h <- pol_and_ineq_mod %>%
#   filter(eiec != "NA" & percentile != "p99p100") %>%
#   mutate(eiec = case_when(eiec < 3 ~ 0,
#                           eiec < 5 ~ 1,
#                           TRUE ~ 2)) %>%
#   ggplot(aes(x = percent_income,
#              y = after_stat(count/sum(count)),
#              fill = percentile)) +
#   geom_density(alpha = 0.5, position = "identity") +
#   labs(title = "How does Income Inequality relate to Executive Competitiveness?",
#        x = "Fraction of National Income Owned",
#        y = "Probability") +
#   facet_grid(
#     eiec ~ percentile, 
#     labeller = labeller(eiec = eie.h.labs, 
#                         percentile = perc.a.labs),
#     scales = "free"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# saveRDS(dat_a, file = "final_project/dat_a")
# saveRDS(dat_b, file = "final_project/dat_b")
# saveRDS(dat_c, file = "final_project/dat_c")
# saveRDS(dat_d, file = "final_project/dat_d")
# saveRDS(dat_e, file = "final_project/dat_e")
# saveRDS(dat_f, file = "final_project/dat_f")
# saveRDS(dat_g, file = "final_project/dat_g")
# saveRDS(dat_h, file = "final_project/dat_h")
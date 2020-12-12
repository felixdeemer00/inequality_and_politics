library(tidyverse)
library(readr)
library(readxl)
library(skimr)
library(tidymodels)
library(rstanarm)
library(gtsummary)
library(broom.mixed)
library(gt)

# I created two separate forms of the pol_and_ineq_mod object, one to be used 
# in the model, filtering all data except that for the income of the top 10%,
# as that was the value to be predicted by the model, and one showing all three 
# income groups, to be used in the data visualization.

pol_and_ineq_mod <- readRDS("inequality_and_politics/rdsFiles/pol_and_ineq_mod") %>%
  drop_na() %>%
  filter(percentile == "p90p100")

pol_and_ineq_mod_2 <- readRDS("inequality_and_politics/rdsFiles/pol_and_ineq_mod") %>%
  drop_na()

paim_split <- initial_split(pol_and_ineq_mod,
                            prop = 0.8)
paim_train <- training(paim_split)
paim_test <- testing(paim_split)
paim_folds <- vfold_cv(paim_train)

# The workflow was created to evaluate the different models, although it is not
# used in the final model - a stan_glm() function is used instead as it is 
# easier to display and to use to generate graphs quickly.

paim_wflow_1 <- workflow() %>%
  add_recipe(recipe(percent_income ~
                      liec + maj + checks_lax + execnat + polariz +
                      incomegroup +
                      gdppcap +
                      region,
                    data = paim_train) %>%
               step_dummy(all_nominal()) %>%
               step_interact( ~ maj:starts_with("incomegroup")) %>%
               step_interact( ~ maj:starts_with("region"))) %>%
  add_model(linear_reg() %>%
              set_engine("stan"))

model_1 <- stan_glm(percent_income ~
                      liec + maj + checks_lax + execnat + polariz +
                      incomegroup +
                      region +
                      maj * incomegroup +
                      maj * region,
                    data = pol_and_ineq_mod,
                    refresh = 0)

# GRAPH A

pola.a.labs <- c("Low Polarization", "Med. Polarization", "High Polarization")
names(pola.a.labs) <- c("0", "1", "2")

perc.a.labs <- c("Bottom 50%", "Medium 40%", "Top 10%")
names(perc.a.labs) <- c("p0p50", "p50p90", "p90p100")

dat_a <- pol_and_ineq_mod_2 %>%
  filter(polariz != "NA" & percentile != "p99p100") %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "How does Income Inequality relate to Polarization?",
       subtitle = "In high-polarization environments, the top 10% tend to have a smaller share of national income",
       x = "Fraction of National Income Owned",
       y = "Probability") +
  facet_grid(
    percentile ~ polariz,
    labeller = labeller(polariz = pola.a.labs,
                        percentile = perc.a.labs),
    scales = "free"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# GRAPH B

maj.b.labs <- c("No Majority", "Majority", "Supermajority")
names(maj.b.labs) <- c("0", "1", "2")

dat_b <- pol_and_ineq_mod_2 %>%
  filter(maj != "NA" & percentile != "p99p100") %>%
  mutate(maj = case_when(maj < 0.5 ~ 0,
                         maj < 0.66 ~ 1,
                         TRUE ~ 2)) %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
    geom_density(alpha = 0.5, position = "identity") +
    labs(title = "How does Income Inequality relate to Legislative Majorities?",
         subtitle = "In countries with supermajorities, the top 10% tend to have a larger share of national income",
         x = "Fraction of National Income Owned",
         y = "Probability") +
    facet_grid(
      percentile ~ maj,
      labeller = labeller(maj = maj.b.labs,
                          percentile = perc.a.labs),
      scales = "free"
    ) +
  theme_bw() +
  theme(legend.position = "none")

# GRAPH C

mil.c.labs <- c("No Military Government", "Military Government")
names(mil.c.labs) <- c("0", "1")

dat_c <- pol_and_ineq_mod_2 %>%
  filter(military != "NA" & percentile != "p99p100") %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "How does Income Inequality relate to Military Government?",
       subtitle = "Military governments tend to be associated with a top 10% with more national income.",
       x = "Fraction of National Income Owned",
       y = "Probability") +
  facet_grid(
    percentile ~ military,
    labeller = labeller(military = mil.c.labs,
                        percentile = perc.a.labs),
    scales = "free"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# GRAPH D

fin.d.labs <- c("No Term Limits", "Term Limits")
names(fin.d.labs) <- c("0", "1")

dat_d <- pol_and_ineq_mod_2 %>%
  filter(finittrm != "NA" & percentile != "p99p100") %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "How does Income Inequality relate to Term Limits?",
       subtitle = "In countries without term limits, the top 10% tend to have a greater share of national income.",
       x = "Fraction of National Income Owned",
       y = "Probability") +
  facet_grid(
    percentile ~ finittrm,
    labeller = labeller(finittrm = fin.d.labs,
                        percentile = perc.a.labs),
    scales = "free"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# GRAPH E

nat.e.labs <- c("No Nationalist Executive", "Nationalist Executive")
names(nat.e.labs) <- c("0", "1")

dat_e <- pol_and_ineq_mod_2 %>%
  filter(execnat != "NA" & percentile != "p99p100") %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "How does Income Inequality relate to Nationalist Executives?",
       subtitle = "Having a nationalist executive tends to be associated with a top 10% with a greater share of income.",
       x = "Fraction of National Income Owned",
       y = "Probability") +
  facet_grid(
    percentile ~ execnat,
    labeller = labeller(execnat = nat.e.labs,
                        percentile = perc.a.labs),
    scales = "free"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# GRAPH F

che.f.labs <- c("0-33% of Che/Bal", "33-66% of Che/Bal", "66-99% of Che/Bal")
names(che.f.labs) <- c("0", "1", "2")

dat_f <- pol_and_ineq_mod_2 %>%
  filter(checks_lax != "NA" & percentile != "p99p100") %>%
  mutate(checks_lax = case_when(checks_lax < quantile(checks_lax, 0.33) ~ 0,
                         checks_lax < quantile(checks_lax, 0.66) ~ 1,
                         TRUE ~ 2)) %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "How does Income Inequality relate to Checks and Balances?",
       subtitle = "In countries with more checks and balances, the top 10% tend to have a smaller income share.",
       x = "Fraction of National Income Owned",
       y = "Probability") +
  facet_grid(
    percentile ~ checks_lax,
    labeller = labeller(checks_lax = che.f.labs,
                        percentile = perc.a.labs),
    scales = "free"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# GRAPH G

lie.g.labs <- c("0-33% of LIEC", "33-66% of LIEC", "66-99% of LIEC")
names(lie.g.labs) <- c("0", "1", "2")

dat_g <- pol_and_ineq_mod_2 %>%
  filter(liec != "NA" & percentile != "p99p100") %>%
  mutate(liec = case_when(liec < 3 ~ 0,
                                liec < 5 ~ 1,
                                TRUE ~ 2)) %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "How does Income Inequality relate to Legislative Competitiveness?",
       subtitle = "Countries with more competitive legislative elections are more divergent in their inequality levels.",
       x = "Fraction of National Income Owned",
       y = "Probability") +
  facet_grid(
    percentile ~ liec,
    labeller = labeller(liec = lie.g.labs,
                        percentile = perc.a.labs),
    scales = "free"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# GRAPH H

eie.h.labs <- c("0-33% of EIEC", "33-66% of EIEC", "66-99% of EIEC")
names(eie.h.labs) <- c("0", "1", "2")

dat_h <- pol_and_ineq_mod_2 %>%
  filter(eiec != "NA" & percentile != "p99p100") %>%
  mutate(eiec = case_when(eiec < 3 ~ 0,
                          eiec < 5 ~ 1,
                          TRUE ~ 2)) %>%
  ggplot(aes(x = percent_income,
             y = after_stat(count/sum(count)),
             fill = percentile)) +
  geom_density(alpha = 0.5, position = "identity") +
  labs(title = "How does Income Inequality relate to Executive Competitiveness?",
       subtitle = "Countries with more competitive legislative elections are more divergent in their inequality levels.",
       x = "Fraction of National Income Owned",
       y = "Probability") +
  facet_grid(
    percentile ~ eiec,
    labeller = labeller(eiec = eie.h.labs,
                        percentile = perc.a.labs),
    scales = "free"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# MIA GRAPH A
newdata_a <- tibble(polariz = as.factor(c(0,1,2)),
                    maj = 0.5,
                    liec = 7,
                    checks_lax = 8,
                    execnat = 0,
                    incomegroup = as.factor("Lower middle income"),
                    region = as.factor("Middle East & North Africa"))

mia_a <- posterior_epred(model_1,
                         newdata = newdata_a) %>%
  as_tibble() %>%
  pivot_longer(cols = `1`:`3`) %>%
  ggplot(aes(value, y = after_stat(count/sum(count)),fill = name)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Polarization",
                    labels = c("1", "2", "3"),
                    values = c("yellow", "orange", "red")) +
  labs(title = "How does Polarization track with Inequality?",
       subtitle = "Higher polarization associated with lower inequality",
       x = "% of National Income held by Top 10%",
       y = "Probability")

# MIA GRAPH B
newdata_b <- tibble(polariz = as.factor(0),
                    maj = c(0.5, 0.66, 0.75),
                    liec = 7,
                    checks_lax = 8,
                    execnat = 0,
                    incomegroup = as.factor("Lower middle income"),
                    region = as.factor("Middle East & North Africa"))

mia_b <- posterior_epred(model_1,
                         newdata = newdata_b) %>%
  as_tibble() %>%
  pivot_longer(cols = `1`:`3`) %>%
  ggplot(aes(value, y = after_stat(count/sum(count)),fill = name)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Majority of Ruling Party",
                    labels = c("1/2 Majority", "2/3 Majority", "3/4 Majority"),
                    values = c("yellow", "orange", "red")) +
  labs(title = "How do Legislative Majorities track with Inequality?",
       subtitle = "Greater majorities associated with higher inequality",
       x = "% of National Income held by Top 10%",
       y = "Probability")

# MIA GRAPH C
newdata_c <- tibble(polariz = as.factor(0),
                    maj = 0.5,
                    liec = c(1,3.5,7),
                    checks_lax = 8,
                    execnat = 0,
                    incomegroup = as.factor("Lower middle income"),
                    region = as.factor("Middle East & North Africa"))

mia_c <- posterior_epred(model_1,
                         newdata = newdata_c) %>%
  as_tibble() %>%
  pivot_longer(cols = `1`:`3`) %>%
  ggplot(aes(value, y = after_stat(count/sum(count)),fill = name)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Election Competitiveness",
                    labels = c("Not Competitive", 
                               "Somewhat Competitive", 
                               "Highly Competitive"),
                    values = c("red", "orange", "yellow")) +
  labs(title = "How does Election Competitiveness track with Inequality?",
       subtitle = "Competitive Elections associated with higher inequality",
       x = "% of National Income held by Top 10%",
       y = "Probability")

# MIA GRAPH D
newdata_d <- tibble(polariz = as.factor(0),
                    maj = 0.5,
                    liec = 7,
                    checks_lax = c(1, 8, 17),
                    execnat = 0,
                    incomegroup = as.factor("Lower middle income"),
                    region = as.factor("Middle East & North Africa"))

mia_d <- posterior_epred(model_1,
                         newdata = newdata_d) %>%
  as_tibble() %>%
  pivot_longer(cols = `1`:`3`) %>%
  ggplot(aes(value, y = after_stat(count/sum(count)), fill = name)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Checks and Balances",
                    labels = c("Weak Checks & Balances", 
                               "Moderate Checks & Balances", 
                               "Strong Checks & Balances"),
                    values = c("red", "orange", "yellow")) +
  labs(title = "How do Checks & Balances track with Inequality?",
       subtitle = "Weaker Checks & Balances associated with higher inequality",
       x = "% of National Income held by Top 10%",
       y = "Probability")

# MIA GRAPH E
newdata_e <- tibble(polariz = as.factor(0),
                    maj = 0.5,
                    liec = 7,
                    checks_lax = 8,
                    execnat = c(0,1),
                    incomegroup = as.factor("Lower middle income"),
                    region = as.factor("Middle East & North Africa"))

mia_e <- posterior_epred(model_1,
                         newdata = newdata_e) %>%
  as_tibble() %>%
  pivot_longer(cols = `1`:`2`) %>%
  ggplot(aes(value, y = after_stat(count/sum(count)),fill = name)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Nationalist Executive",
                    labels = c("No Nat. Exec. in power", 
                               "Nat. Exec. in power"),
                    values = c("yellow", "red")) +
  labs(title = "How do Nationalist Executives track with Inequality?",
       subtitle = "Nationalist Executive associated with higher inequality",
       x = "% of National Income held by Top 10%",
       y = "Probability")

# MIA GRAPH F
newdata_f <- tibble(polariz = as.factor(0),
                    maj = 0.5,
                    liec = 7,
                    checks_lax = 8,
                    execnat = 0,
                    incomegroup = as.factor(c("High income",
                                            "Upper middle income",
                                            "Lower middle income",
                                            "Low income")),
                    region = as.factor("Middle East & North Africa"))

mia_f <- posterior_epred(model_1,
                         newdata = newdata_f) %>%
  as_tibble() %>%
  pivot_longer(cols = `1`:`4`) %>%
  ggplot(aes(value, y = after_stat(count/sum(count)),fill = name)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Income Group",
                    labels = c("High income",
                               "Upper middle income",
                               "Lower middle income",
                               "Low income"),
                    values = c("blue", 
                               "green", 
                               "yellow",
                               "red")) +
  labs(title = "How does Income Group track with Inequality?",
       subtitle = "Upper Middle Income group more unequal, others lower",
       x = "% of National Income held by Top 10%",
       y = "Probability")

# MIA GRAPH G
newdata_g <- tibble(polariz = as.factor(0),
                    maj = 0.5,
                    liec = 7,
                    checks_lax = 8,
                    execnat = 0,
                    incomegroup = as.factor("Low income"),
                    region = as.factor(c("Middle East & North Africa",
                                         "Europe & Central Asia",
                                         "East Asia & Pacific",
                                         "Latin America & Caribbean",
                                         "North America",
                                         "South Asia",
                                         "Sub-Saharan Africa")))

mia_g <- posterior_epred(model_1,
                         newdata = newdata_g) %>%
  as_tibble() %>%
  pivot_longer(cols = `1`:`7`) %>%
  ggplot(aes(value, y = after_stat(count/sum(count)),fill = name)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Region",
                    labels = c("Middle East & North Africa",
                               "Europe & Central Asia",
                               "East Asia & Pacific",
                               "Latin America & Caribbean",
                               "North America",
                               "South Asia",
                               "Sub-Saharan Africa"),
                    values = c("red", "orange", "yellow",
                               "green", "blue", "purple",
                               "pink")) +
  labs(title = "How does Region track with Inequality?",
       x = "% of National Income held by Top 10%",
       y = "Probability")

mod_table <- tbl_regression(model_1, intercept = TRUE, 
                            label = list(`(Intercept)` ~ "Intercept",
                                    liec ~ "Legislative Competitiveness (0-7)",
                                    maj ~ "Legislative Seat %",
                                    checks_lax ~ "Checks and Balances (0-17)",
                                    execnat ~ "Nationalist Executive (No/Yes)",
                                    polariz ~ "Polarization (0-2)",
                                    incomegroup ~ "Income Group:",
                                    region ~ "Region:")) %>%
  as_gt() %>%
  tab_header(title = "Regression of Inequality Levels", 
             subtitle = "The Effect of Political Factors on Inequality") %>%
  tab_source_note(md("Source: DPI and WID Databases"))

# After the graphs were all created, I saved them in the folder to save time
# that would be spent generating them in the main shiny app.

saveRDS(dat_a, file = "inequality_and_politics/rdsFiles/dat_a")
saveRDS(dat_b, file = "inequality_and_politics/rdsFiles/dat_b")
saveRDS(dat_c, file = "inequality_and_politics/rdsFiles/dat_c")
saveRDS(dat_d, file = "inequality_and_politics/rdsFiles/dat_d")
saveRDS(dat_e, file = "inequality_and_politics/rdsFiles/dat_e")
saveRDS(dat_f, file = "inequality_and_politics/rdsFiles/dat_f")
saveRDS(dat_g, file = "inequality_and_politics/rdsFiles/dat_g")
saveRDS(dat_h, file = "inequality_and_politics/rdsFiles/dat_h")
saveRDS(mia_a, file = "inequality_and_politics/rdsFiles/mia_a")
saveRDS(mia_b, file = "inequality_and_politics/rdsFiles/mia_b")
saveRDS(mia_c, file = "inequality_and_politics/rdsFiles/mia_c")
saveRDS(mia_d, file = "inequality_and_politics/rdsFiles/mia_d")
saveRDS(mia_e, file = "inequality_and_politics/rdsFiles/mia_e")
saveRDS(mia_f, file = "inequality_and_politics/rdsFiles/mia_f")
saveRDS(mia_g, file = "inequality_and_politics/rdsFiles/mia_g")
saveRDS(model_1, file = "inequality_and_politics/rdsFiles/model_1")
saveRDS(mod_table, file = "inequality_and_politics/rdsFiles/mod_table")

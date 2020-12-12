library(tidyverse)
library(readr)
library(readxl)
library(skimr)
library(tidymodels)
library(rstanarm)
library(gtsummary)
library(broom.mixed)
library(gt)
library(shinythemes)

dat_a <- readRDS("rdsFiles/dat_a")
dat_b <- readRDS("rdsFiles/dat_b")
dat_c <- readRDS("rdsFiles/dat_c")
dat_d <- readRDS("rdsFiles/dat_d")
dat_e <- readRDS("rdsFiles/dat_e")
dat_f <- readRDS("rdsFiles/dat_f")
dat_g <- readRDS("rdsFiles/dat_g")
dat_h <- readRDS("rdsFiles/dat_h")
mia_a <- readRDS("rdsFiles/mia_a")
mia_b <- readRDS("rdsFiles/mia_b")
mia_c <- readRDS("rdsFiles/mia_c")
mia_d <- readRDS("rdsFiles/mia_d")
mia_e <- readRDS("rdsFiles/mia_e")
mia_f <- readRDS("rdsFiles/mia_f")
mia_g <- readRDS("rdsFiles/mia_g")
model_1 <- readRDS("rdsFiles/model_1")
mod_table <- readRDS("rdsFiles/mod_table")

    ui <- navbarPage(theme = shinytheme("flatly"),
        "Economic Inequality and Political Stability",
        tabPanel("About",
                 fluidPage(
                     titlePanel("How does Income Inequality affect Politics?"),
                     h3("About the Project"),
                     p("In this project, I set out to discover the relationship 
                     between income inequality and the political environment of 
                     a country. Does an increase in income inequality weaken 
                     political institutions or increase polarization? How does 
                     the legislative majority of the ruling party correlate with 
                     income inequality? This project seeks to answer these types 
                     of questions."),
                     h3("List of Sections"),
                     p("The 'Model Discussion' tab covers construction of the 
                       model of income inequality itself, and the decision-
                       making process involved."),
                     p("The 'Observations' tab interprets the numbers in the 
                       model, using graphs to visually show the impacts of these 
                       factors."),
                     p("The 'Model in Action' tab allows one to experiment with 
                       the model, to better understand the impacts of the 
                       variables investigated."),
                     p("The 'Info' tab contains an overview of the data sources 
                       used, as well as a link to the GitHub repo for the 
                       project."),
                     )),
        tabPanel("Model Discussion",
                 titlePanel("Model Discussion"),
                 p("The factors that went into the model can be roughly grouped 
                   into political, economic, and regional factors. Economic 
                   factors are comprised of the income group of each country, 
                   and regional factors are comprised of the region a country is 
                   located in, as defined by the World Bank. The political 
                   factors are slightly more qualitative in nature, as many of 
                   them do not have a strict set of definitions. The scores were 
                   assigned to individual countries by political science 
                   researchers, as outlined under the paper ‘New Tools and New 
                   Tests in Comparative Political Economy’, published by the 
                   World Bank Development Research Group. These are the 
                   different political variables:"),
                 p("1. Legislative Competitiveness: The competitiveness of 
                   elections to the legislature, as determined by the number of 
                   parties competing, as well as the vote share of the largest 
                   party, ranging from a score of 1 (no legislature) to 7 (the 
                   largest party received less than 75% of the seats)."),
                 p("2. Legislative Majority: The legislative majority held by the 
                   ruling party, not counting coalition partners."),
                 p("3. Checks and Balances: The number of veto players in a 
                   political system, with the total number adjusted according to 
                   whether or not these veto players are independent of each 
                   other. This independence is determined according to 
                   legislative competitiveness and the political affiliations of 
                   the veto players."),
                 p("4. Nationalist Executive: If the executive’s party is 
                   nationalist, defined by a whether or not their platform 
                   discusses the creation or defense of a national or ethnic 
                   identity."),
                 p("5. Polarization: The maximum difference in political 
                   orientation between government parties, representing the 
                   political uniformity of the ruling coalition."),
                 p("Many different political and economic variables were 
                   considered for the model but did not make it into the final 
                   model, such as the presence of term limits, the length of 
                   time the ruling party had been in power, and the GDP per 
                   capita of the country. These were excluded for two reasons: 
                   either they did not improve the predictive power of the model 
                   (such as for term limits, number of years the ruling party 
                   had been in power), or because I judged they overlapped 
                   significantly with variables already factored into the model, 
                   leading to distortion of the final values (GDP per capita 
                   overlaps with income group, executive election 
                   competitiveness overlaps with legislative election 
                   competitiveness). "),
                 p("In this table, the Intercept represents the expected portion
                   of national income held by the top 10% in a High Income 
                   country in the East Asia & Pacific Region, with a 
                   polarization level of 0. To see all of the interpretations of
                   these numbers, view the 'Model Observations' tab."),
                 gt_output("mod_table")),
        tabPanel("Observations",
                 titlePanel("Observations"),
                 p("The findings of the model are discussed below, while the 
                   graphs visually display the findings of the model, applied
                   to a hypothetical country in which all variables remain
                   constant except for the one being varied."),
                 sidebarPanel(
                     selectInput(
                         "plot_type2",
                         "Plot Type",
                         c("Polarization" = "a", 
                           "Legislative Majority" = "b",
                           "Election Competitiveness" = "c",
                           "Checks and Balances" = "d",
                           "Nationalist Executive in Power" = "e",
                           "Income Group" = "f",
                           "Region" = "g")
                     )),
                 mainPanel(plotOutput("line_plot2")),
                p("HERE IS WHERE I'LL DISCUSS THE FINDINGS FROM THE MODEL AND
                  THEIR IMPLICATIONS")),
        tabPanel("The Model in Action",
                 titlePanel("The Model in Action"),
                 p("This application allows one to interact with the model, 
                 setting various parameters for two hypothetical countries and
                 observing the predicted posterior distributions for the income
                 share of the top 10%. The blue distribution corresponds to the
                 sidebar on the left, while the red corresponds to the right
                 sidebar."),
                 sidebarPanel(width = 2,
                     selectInput(
                         "incomegroup1",
                         "Income Group",
                         c("High income" = "High income",
                           "Upper middle income" = "Upper middle income",
                           "Lower middle income" = "Lower middle income",
                           "Low income" = "Low income")),
                     selectInput(
                         "region1",
                         "Region",
                         c("Middle East & North Africa" = 
                               "Middle East & North Africa",
                           "Europe & Central Asia" = "Europe & Central Asia",
                           "East Asia & Pacific" = "East Asia & Pacific",
                           "Latin America & Caribbean" = 
                               "Latin America & Caribbean",
                           "North America" = "North America",
                           "South Asia" = "South Asia",
                           "Sub-Saharan Africa" = "Sub-Saharan Africa")),
                     sliderInput("polariz1", "Polarization",
                                 min = 0, max = 2,
                                 value = 0),
                     sliderInput("maj1", "Legislative Majority",
                                 min = 0, max = 1,
                                 step = 0.01, value = 0.5),
                     sliderInput("execnat1", "Nationalist Executive",
                                 min = 0, max = 1,
                                 step = 1, value = 0),
                     sliderInput("checks_lax1", "Checks & Balances",
                                 min = 1, max = 17,
                                 value = 8),
                     sliderInput("liec1", "Legislative Competitiveness",
                                 min = 1, max = 7,
                                 value = 7)),
                 mainPanel(plotOutput("custom_plot")),
                 sidebarPanel(width = 2,
                     selectInput(
                         "incomegroup2",
                         "Income Group",
                         c("High income" = "High income",
                           "Upper middle income" = "Upper middle income",
                           "Lower middle income" = "Lower middle income",
                           "Low income" = "Low income")),
                     selectInput(
                         "region2",
                         "Region",
                         c("Middle East & North Africa" = 
                               "Middle East & North Africa",
                           "Europe & Central Asia" = "Europe & Central Asia",
                           "East Asia & Pacific" = "East Asia & Pacific",
                           "Latin America & Caribbean" = 
                               "Latin America & Caribbean",
                           "North America" = "North America",
                           "South Asia" = "South Asia",
                           "Sub-Saharan Africa" = "Sub-Saharan Africa")),
                     sliderInput("polariz2", "Polarization",
                                 min = 0, max = 2,
                                 value = 0),
                     sliderInput("maj2", "Legislative Majority",
                                 min = 0, max = 1,
                                 step = 0.01, value = 0.5),
                     sliderInput("execnat2", "Nationalist Executive",
                                 min = 0, max = 1,
                                 step = 1, value = 0),
                     sliderInput("checks_lax2", "Checks & Balances",
                                 min = 1, max = 17,
                                 value = 8),
                     sliderInput("liec2", "Legislative Competitiveness",
                                 min = 1, max = 7,
                                 value = 7))),
        tabPanel("Info", 
                 h3("Information on Sources:"),
                 
                 p("The data on political institutions in this project came from
                   the Database of Political Institutions, a database that 
                   measures variables relating to politics and political 
                   institutions, including political polarization, legislative 
                   majorities, and other things. This data can be found ",
                 a(href = "https://datacatalog.worldbank.org/dataset/wps2283-database-political-institutions",
                     "here.")),
                 
                 p("The data on income inequality comes from the World 
                   Inequality Database. Its data is reliable, as the database is 
                   extremely transparent with its data sources, as well as 
                   outlining its methodology in detail. This data can be 
                   found ",
                 a(href = "https://wid.world/data/", "here.")),
                 
                 p("The github repo for my project can be found ",
                 a(href = "https://github.com/felixdeemer00/inequality_and_politics",
                     "here.")),
                 h3("About Me"),
                 p("My name is Felix Deemer and I study Government. 
             You can reach me at felixdeemer@college.harvard.edu.")))

server <- function(input, output) {
    output$line_plot <- renderPlot({

        ifelse(input$plot_type == "a", 
                 dat <- dat_a,
               ifelse(input$plot_type == "b",
                 dat <- dat_b,
               ifelse(input$plot_type == "c",
                 dat <- dat_c,
               ifelse(input$plot_type == "d",
                 dat <- dat_d,
               ifelse(input$plot_type == "e",
                 dat <- dat_e,
               ifelse(input$plot_type == "f",
                        dat <- dat_f,
               ifelse(input$plot_type == "g",
                        dat <- dat_g,
               ifelse(input$plot_type == "h",
                        dat <- dat_h))))))))
        
        dat
    })
    output$line_plot2 <- renderPlot({
        
        ifelse(input$plot_type2 == "a", 
               mia <- mia_a,
               ifelse(input$plot_type2 == "b",
                      mia <- mia_b,
                      ifelse(input$plot_type2 == "c",
                             mia <- mia_c,
                             ifelse(input$plot_type2 == "d",
                                    mia <- mia_d,
                                    ifelse(input$plot_type2 == "e",
                                           mia <- mia_e,
                                           ifelse(input$plot_type2 == "f",
                                                  mia <- mia_f,
                                                  ifelse(input$plot_type2 == "g",
                                                         mia <- mia_g,
                                                         ifelse(input$plot_type2 == "h",
                                                                mia <- mia_h))))))))
        
        mia
    })
    output$mod_table <- render_gt({
        mod_table
        })
    output$custom_plot <- renderPlot({
        
        custom_data <- tibble(polariz = as.factor(c(input$polariz1[1], 
                                                    input$polariz2[1])),
                              maj = c(input$maj1[1], 
                                      input$maj2[1]),
                              liec = c(input$liec1[1], 
                                       input$liec2[1]),
                              checks_lax = c(input$checks_lax1[1], 
                                             input$checks_lax2[1]),
                              execnat = c(input$execnat1[1], 
                                          input$execnat2[1]),
                              incomegroup = as.factor(c(input$incomegroup1[1], 
                                                        input$incomegroup2[1])),
                              region = as.factor(c(input$region1[1], 
                                                   input$region2[1])))
        
        posterior_epred(model_1,
                        newdata = custom_data) %>%
            as_tibble() %>%
            pivot_longer(cols = `1`:`2`) %>%
            ggplot(aes(value, y = after_stat(count/sum(count)), fill = name)) +
            geom_density(alpha = 0.4) +
            theme_bw() +
            labs(x = "% of National Income held by Top 10%",
                 y = "Probability") +
            scale_x_continuous(limits = c(0,100),
                               breaks = c(seq(from = 0,
                                              to = 100,
                                              by = 10))) +
            scale_fill_manual(name = "Input",
                              labels = c("Left", 
                                         "Right"),
                              values = c("blue", "red"))
    })
}

shinyApp(ui = ui, server = server)

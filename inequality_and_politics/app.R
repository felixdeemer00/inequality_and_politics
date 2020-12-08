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
model_1 <- readRDS("rdsFiles/model_1")
mod_table <- readRDS("rdsFiles/mod_table")

    ui <- navbarPage(theme = shinytheme("flatly"),
        "Economic Inequality and Political Stability",
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("In this project, I set out to discover the relationship between income inequality
                    and the political environment of a country. Does an increase in income inequality 
                    weaken political institutions or increase polarization? How does the legislative
                    majority of the ruling party correlate with income inequality?
                    This project seeks to answer these types of questions.")),
        tabPanel("Visualizations",
                 fluidPage(
                     titlePanel("How does Income Inequality affect Politics?"),
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Polarization" = "a", 
                               "Legislative Majority" = "b",
                               "Military Government" = "c",
                               "Finite Term" = "d",
                               "Nationalist Executive in Power" = "e",
                               "Check and Balances" = "f",
                               "Legislative Competitiveness" = "g",
                               "Executive Competitiveness" = "h")
                         )),
                     mainPanel(plotOutput("line_plot"),
                               p(""))
                     )),
        tabPanel("Model",
                 titlePanel("Model"),
                 gt_output("mod_table")),
        tabPanel("The Model in Action",
                 titlePanel("The Model in Action"),
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
                         c("Middle East & North Africa" = "Middle East & North Africa",
                           "Europe & Central Asia" = "Europe & Central Asia",
                           "East Asia & Pacific" = "East Asia & Pacific",
                           "Latin America & Caribbean" = "Latin America & Caribbean",
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
                         c("Middle East & North Africa" = "Middle East & North Africa",
                           "Europe & Central Asia" = "Europe & Central Asia",
                           "East Asia & Pacific" = "East Asia & Pacific",
                           "Latin America & Caribbean" = "Latin America & Caribbean",
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
                 
                 p("The data on political institutions in this project came from the Database of 
                    Political Institutions, a database attempting to capture certain variables relating
                    to politics, including political polarization, legislative majorities, and other
                    things. This data can be found ",
                   a(href = "https://datacatalog.worldbank.org/dataset/wps2283-database-political-institutions",
                     "here.")),
                 
                 p("The data on income inequality comes from the World Inequality Database. Its data
                    seems highly reliable, as it is extremely transparent with its data sources, as
                    well as outlining its methodology in detail. This data can be found ",
                   a(href = "https://wid.world/data/", "here.")),
                 
                 p("The github repo for my project can be found at my ",
                   a(href = "https://github.com/felixdeemer00/inequality_and_politics",
                     "Github")),
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
            labs(x = "% of National Income held by Top 10%",
                 y = "Probability") +
            scale_x_continuous(limits = c(0,100)) +
            scale_fill_manual(name = "Input",
                              labels = c("Left", 
                                         "Right"),
                              values = c("blue", "red"))
    })
}

shinyApp(ui = ui, server = server)

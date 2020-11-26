#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(readr)

dat_a <- readRDS("dat_a")
dat_b <- readRDS("dat_b")
dat_c <- readRDS("dat_c")
dat_d <- readRDS("dat_d")
dat_e <- readRDS("dat_e")
dat_f <- readRDS("dat_f")
dat_g <- readRDS("dat_g")
# dat_h <- readRDS("dat_h")

# Define UI for application that draws a histogram
    ui <- navbarPage(
        "An Economy Divided: Economic Inequality and Political Stability",
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
        tabPanel("Discussion",
                 titlePanel("Discussion Title"),
                 p("This graph seeks to investigate the relationship between legislative majorities
                   and income inequality, using a linear regression model in order to find the 
                   overall trend within the data obtained. What was found is that in environments
                   where the ruling party has a larger legislative majority, there tends to be
                   greater income inequality (in that the share of national income going to the
                   bottom 50% tends to be lower.)")),
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("I am interested in discovering the impacts of various forms of economic inequality
                    (wealth and income) on political institutions and communal violence across
                    countries. Does an increase in income inequality lead to greater violence and eroded
                    political institutions? This project seeks to answer these types of questions."),
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
}

# Run the application 
shinyApp(ui = ui, server = server)

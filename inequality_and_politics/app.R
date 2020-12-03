library(tidyverse)
library(readr)
library(readxl)
library(skimr)
library(tidymodels)
library(rstanarm)
library(gtsummary)
library(broom.mixed)
library(gt)

dat_a <- readRDS("dat_a")
dat_b <- readRDS("dat_b")
dat_c <- readRDS("dat_c")
dat_d <- readRDS("dat_d")
dat_e <- readRDS("dat_e")
dat_f <- readRDS("dat_f")
dat_g <- readRDS("dat_g")
dat_h <- readRDS("dat_h")
mod_table <- readRDS("mod_table")

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
        tabPanel("Model Tab",
                 titlePanel("Model"),
                 gt_output("mod_table")),
        tabPanel("About Tab", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("I am interested in discovering the impacts of various forms of economic inequality
                    (wealth and income) on political institutions and communal violence across
                    countries. Does an increase in income inequality lead to greater violence and eroded
                    political institutions? This project seeks to answer these types of questions.
                   
                    The data on political institutions in this project came from the Database of 
                    Political Institutions, a database attempting to capture certain variables relating
                    to politics, including political polarization, legislative majorities, and other
                    things.
                    
                    The data on income inequality comes from the World Inequality Database. Its data
                    seems highly reliable, as it is extremely transparent with its data sources, as
                    well as outlining its methodology in detail.
                   
                    The github repo for my project can be found at:
                    https://github.com/felixdeemer00/inequality_and_politics"),
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
}

shinyApp(ui = ui, server = server)

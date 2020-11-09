#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(PPBDS.data)
library(ggplot2)
library(tidyverse)
library(readr)
library(readxl)

nonstate_violence <- read_csv("raw_data/UCDP_Data_Nonstate_Violence.csv",
                              col_types = cols(
                                  .default = col_double(),
                                  side_a_name = col_character(),
                                  side_a_name_fulltext = col_logical(),
                                  side_a_name_mothertongue = col_character(),
                                  side_a_components = col_character(),
                                  side_b_name = col_character(),
                                  side_b_name_fulltext = col_logical(),
                                  side_b_name_mothertongue = col_character(),
                                  side_b_components = col_character(),
                                  start_date = col_date(format = ""),
                                  start_date2 = col_date(format = ""),
                                  ep_end_date = col_date(format = ""),
                                  ep_end_prec = col_logical(),
                                  location = col_character(),
                                  gwno_location = col_character()
                              ))

one_sided_violence <- read_csv("raw_data/UCDP_Data_Onesided_Violence.csv",
                               col_types = cols(
                                   conflict_id = col_double(),
                                   dyad_id = col_double(),
                                   actor_id = col_double(),
                                   coalition_components = col_character(),
                                   actor_name = col_character(),
                                   actor_name_fulltext = col_character(),
                                   actor_name_mothertongue = col_character(),
                                   year = col_double(),
                                   best_fatality_estimate = col_double(),
                                   low_fatality_estimate = col_double(),
                                   high_fatality_estimate = col_double(),
                                   is_government_actor = col_double(),
                                   location = col_character(),
                                   gwno_location = col_character(),
                                   gwnoa = col_double(),
                                   region = col_character(),
                                   version = col_double()
                               ))

income_inequality <- read_excel("raw_data/WIID_06MAY2020.xlsx") %>%
    select(c(c3, c2)) %>%
    distinct()

income_inequality_2 <- read.csv("raw_data/WID_Data_Income.csv", sep = ";", skip = 1) %>%
    pivot_longer(cols = -c(Year, Percentile),
                 names_to = "country",
                 values_to = "values") %>%
    mutate(country = map_chr(country, 
                             ~ gsub(x = ., 
                                    pattern = "sptinc_z_", 
                                    replacement = ""))) %>%
    mutate(c2 = str_sub(country, 1, 2)) %>%
    select(-country) %>%
    inner_join(., income_inequality)

political_institutions <- read_excel("raw_data/DPI_Data_Political_Institutions.xls")

pol_mod <- political_institutions %>%
    select(countryname, ifs, year, polariz, finittrm, military, prtyin, 
           execnat, maj, checks_lax, liec, eiec) %>%
    filter(year %in% 2004:2012, ifs != 0) %>%
    drop_na() %>%
    mutate(year = as.double(year)) %>%
    rename(c3 = ifs, Year = year)

pol_and_ineq_mod <- left_join(pol_mod, income_inequality_2) %>%
    drop_na()

# Define UI for application that draws a histogram
    ui <- navbarPage(
        "An Economy Divided: Economic Inequality and Political Stability",
        tabPanel("Visualizations",
                 fluidPage(
                     titlePanel("How does Income Inequality affect Politics?"),
                         mainPanel(plotOutput("line_plot"),
                                   p(""))
                     )),
        tabPanel("Discussion",
                 titlePanel("Discussion Title"),
                 p("I chose to display 3 separate graphs on top of one another, as I felt this better communicates
                    the central point. The violin graph ensures that the reader has a sense of the overall density, 
                    while the boxplot allows for a comparison of the quartiles. The scatter plot displayed on top
                    of this data shows the distribution slightly more intuitively, as well as showing the constituent
                    data points that went into producing the graph.")),
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui

        pol_and_ineq_mod %>%
            filter(Percentile == "p0p50") %>%
            filter(maj != "NA") %>%
            ggplot(aes(x = as.numeric(maj), y = values)) +
              geom_point(alpha = 0.5) +
              geom_smooth(method = "lm") +
              labs(title = "How does the income of the bottom 50% vary according to legislative majorities?",
                   subtitle = "On average, greater majorities mean greater inequality",
                   x = "Legislative Majority of Ruling Party",
                   y = "Proportion of Income to bottom 50%")

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

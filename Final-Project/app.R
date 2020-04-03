#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(stringr)
library(tidycensus)
census_api_key("85884debe86c4a15bb8d549f40ad1f1c9c415045")
library(forcats)
library(ggthemes)

data_2018 <- read_csv("raw-data//2018_college_data.csv") %>%
    clean_names()
names(data_2018) <- str_replace_all(names(data_2018), "^[^_]*_", "")
names(data_2018) <- str_replace_all(names(data_2018), "_of_undergraduate_enrollment_that_are", "")

census_data_2 <- get_acs(geography = "us", 
                         variables = c(
                             total_pop = "B01001_001",
                             pop_white = "B01001A_001",
                             pop_african_american = "B01001B_001",
                             pop_native_american_alaskan = "B01001C_001",
                             pop_asian = "B01001D_001",
                             pop_native_hawaiian_pacific_islander = "B01001E_001",
                             pop_other = "B01001F_001",
                             pop_two_or_more_races = "B01001G_001"),
                         year = 2010) 

census_data_2 <- census_data_2 %>%
    select(variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(pct_white = (pop_white / total_pop) * 100) %>%
    mutate(pct_african_american = (pop_african_american / total_pop) * 100) %>%
    mutate(pct_native_american_alaskan = (pop_native_american_alaskan / total_pop) * 100) %>%
    mutate(pct_asian = (pop_asian / total_pop) * 100) %>%
    mutate(pct_native_hawaiian_pacific_islander = (pop_native_hawaiian_pacific_islander / total_pop) * 100) %>%
    mutate(pct_other = (pop_other / total_pop) * 100) %>%
    mutate(pct_two_or_more_races = (pop_two_or_more_races / total_pop) * 100)

size_2018 <- data_2018 %>%
    
    # The institution size variable must be a factor, so that they can be used as
    # the variable on the x-axis of a box plot
    
    mutate(institution_size_category = factor(institution_size_category,
                                              levels = c("Under 1,000",
                                                         "1,000 - 4,999",
                                                         "5,000 - 4,999", 
                                                         "10,000 - 19,999",
                                                         "20,000 and above",
                                                         "Not applicable",
                                                         "Not reported"))) %>%
    filter(institution_size_category != "Not applicable" & institution_size_category != "Not reported") 


# Define UI for application that draws a histogram
ui <- fluidPage(
    ui <- navbarPage(
        "Diversity of Upper Level Education",
        tabPanel("Model",
                 fluidPage(
                     titlePanel("Institution Size"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(
                                 "plot_type",
                                 "Plot Type",
                                 c("Percent Black or African American" = "a", "Percent Asian" = "b")
                             )),
                         mainPanel(plotOutput("line_plot")))
                 )),
        tabPanel("Discussion",
                 titlePanel("Discussion Title"),
                 p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("This project aims to determine the factors that contribute to 
                 the racial and ethnic diversity of college populations. I 
                 picked several variables that I wanted to explore: state in 
                 which a college is located, size of the institution, level of 
                 the institution, degree of urbanization, the admissions rate, 
                 and the gender diversity. I hope to use these variables and 
                 determine the trends that contribute to highly, racially 
                 diverse universities and racially uniform universities. This 
                 topic is very central to conversation today with the lawsuit 
                 regarding Harvard's admissions practices and conversations 
                 regarding affirmative action in news media today. Furthermore, 
                 in additional study of the data, I hope to be able to isolate 
                 specific institutions (specifically Ivy League schools) and 
                 evaluate the diversity trends especially in response to the recent 
                 lawsuit. "),
                 h3("Data"),
                 p("To accomplish the goal of this project, I have amassed data 
                 sets from each year that contain information about almost 7,000
                 institutions in the continental United States. Each data set 
                 contains the information about these institutions in a specific 
                 year, and I have collected the data sets from 2009 to 2018. 
                 These data sets all come from nces.ed.gov (the National Center 
                 for Education Statistics). 
                 In addition to these data sets, I have loaded race data from 
                 the tidycensus r package in order to compare the mean racial 
                 diversity of a set of institutions in a particular state to 
                 the general degree of diversity in that state."),
                 h3("About Me"),
                 p("My name is Emma Freeman and I study Computer Science at Harvard. 
                    You can reach me at efreeman@college.harvard.edu.")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$line_plot <- renderPlot({
        
        if(input$plot_type == "a") {
            size_2018 %>%
                
                # This removes any NA observations in order to remove the message about the
                # removal of NA observations that occurs when the geom_boxplot() is used
                
                filter(!is.na(percent_black_or_african_american)) %>%
                ggplot(aes(x = as_factor(institution_size_category), y = percent_black_or_african_american)) +
                
                # This shows the decreasing number of items toward the extremes 
                
                geom_boxplot(alpha = 0.25) +
                xlab("Size of Institution") + 
                ylab("Percent Black or African American") +
                
                # I will use them_tufte going forward, so that my final project appears 
                # consistent
                
                theme_tufte() + 
                labs(title = "Percent of Black or African Students by Size of Institution",
                     subtitle = "With comparison at national population estimate") +
                theme(axis.line = element_line()) +
                
                # This gives the national estimate for the percentage of the population that 
                # is African American, offering a comparison for the level of representation 
                # in schools 
                
                geom_hline(yintercept = census_data_2$pct_african_american, color = "red")
        }
        else {
            if(input$plot_type == "b") {
                size_2018 %>%
                    
                    
                    filter(!is.na(percent_asian)) %>%
                    ggplot(aes(x = as_factor(institution_size_category), y = percent_asian)) +
                    geom_boxplot(alpha = 0.25) +
                    xlab("Size of Institution") + 
                    ylab("Percent Asian") +
                    theme_tufte() + 
                    labs(title = "Percent of Asian Students by Size of Institution",
                         subtitle = "With comparison at national population estimate") +
                    theme(axis.line = element_line()) +
                    geom_hline(yintercept = census_data_2$pct_asian, color = "red")
            }
            
        }
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

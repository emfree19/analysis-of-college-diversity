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
library(forcats)
library(ggthemes)
library(dotwhisker)
library(broom)
library(patchwork) 

mean_data <- read_csv("mean_data.csv", col_types = cols())
mean_data <- mean_data %>%
    mutate(institution_size_category = factor(institution_size_category,
                                              levels = c("Under 1,000",
                                                         "1,000 - 4,999",
                                                         "5,000 - 9,999", 
                                                         "10,000 - 19,999",
                                                         "20,000 and above"))) %>%
    mutate(degree_of_urbanization_urban_centric_locale = 
               factor(degree_of_urbanization_urban_centric_locale, 
                      levels = c("Rural", "Town", "Suburb", "City")))

census_data_2 <- read_csv("census.csv", col_types = cols())

ui <- fluidPage(
    ui <- navbarPage(
        "Diversity of Upper Level Education",
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("This project aims to determine the factors that correlate 
                 with racial and ethnic diversity of college populations. I 
                 picked several variables that I wanted to explore: size of 
                 institution, percent of women, admissions rate, and locale. 
                 Furthermore, I chose the top three minorities (latinx, african 
                 american, and asian) to consider. Note, I have the information for other
                 race data. I found the correlations between these independent variables 
                 and the dependent variables defined above. I found the correlation 
                 coefficients and defined the independent variables categorically 
                 in order to more clearly show the relationship in the data. I 
                 used these variables to help determine what may to racially 
                 diverse universities and racially uniform universities.
                 This was a central topic (before the pandemic) due to the recent 
                 lawsuit regarding Harvard's admissions practices."),
                 h3("Data"),
                 p("To accomplish the goal of this project, I have amassed data 
                 sets from each year (between 2010 to 2018) that contain information 
                 about almost 7,000 institutions in the continental United States. 
                 Each data set had the pertinent variables about these institutions for 
                 a given year. The data was taken from the", 
                 a(href = "https://nces.ed.gov/ipeds/use-the-data", "National Center 
                 for Education Statistics"), 
                 "In addition, I have loaded race data from 
                 the tidycensus r package in order to compare the mean racial population
                 of the country as a whole to determine the either overrepresentation or 
                 underrepresentation of a minority"),
                 h3("About Me"),
                 p("My name is Emma Freeman and I study Computer Science at Harvard. 
                    You can reach me at efreeman@college.harvard.edu."),
                 p("The code for this project can be found at",
                   a(href = "https://github.com/emfree19/final_project", "github repo"), ".")),
        tabPanel("Latinx Student Population",
                 fluidPage(
                     sidebarPanel(
                         width = 6,
                         selectInput(
                             "plot_type_h",
                             "Independent Variables",
                             c("Size of Institution, Percent Women, Admissions Rate" = "a",
                               "Locale" = "b")),
                         p("Here are the coefficients."),
                         plotOutput("h_coefficients")
                         ),
                     mainPanel(width = 6,
                               plotOutput("h_institution"),
                               plotOutput("h_women"),
                               plotOutput("h_admissions"))
                 )),
        tabPanel("African American Student Population",
                 fluidPage(
                     sidebarPanel(
                         width = 6,
                         selectInput(
                             "plot_type_aa",
                             "Independent Variables",
                             c("Size of Institution, Percent Women, Admissions Rate" = "a", 
                               "Locale" = "b")),
                         p("Here are the coefficients."),
                         plotOutput("aa_coefficients")
                     ),
                     mainPanel(width = 6, 
                               plotOutput("aa_institution"),
                               plotOutput("aa_women"),
                               plotOutput("aa_admissions"))
                 )),
        tabPanel("Asian Student Population",
                 fluidPage(
                     sidebarPanel(
                         width = 6,
                         selectInput(
                             "plot_type_a",
                             "Independent Variables",
                             c("Size of Institution, Percent Women, Admissions Rate" = "a",
                               "Locale" = "b")),
                             p("Here are the coefficients."),
                             plotOutput("a_coefficients")
                     ),
                     mainPanel(width = 6,
                               plotOutput("a_institution"),
                               plotOutput("a_women"),
                               plotOutput("a_admissions"))
                 ))
        )
)
        


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$aa_coefficients <- renderPlot({
        if(input$plot_type_aa == "a") {
            model_african_american <- lm(mean_african_american ~ mean_enrollment + 
                                             mean_women + 
                                             mean_admitted,
                                         data = mean_data) %>%
                tidy(conf.int = TRUE)
            dwplot(model_african_american) %>%
                relabel_predictors(mean_enrollment = "Undergraduate Enrollment",
                                   mean_women = "Percentage of Women",
                                   mean_admitted = "Admissions Rate") +
                theme_classic() +
                ggtitle("Correlation between Independent Variables \n & African American Student Population") +
                theme(legend.position = "none")
        }
        else{
            if(input$plot_type_aa == "b") {
                model_african_american_2 <- lm(mean_african_american ~ degree_of_urbanization_urban_centric_locale,
                                               data = mean_data) %>%
                    tidy(conf.int = TRUE)
                
                dwplot(model_african_american_2, show_intercept = TRUE) %>%
                    relabel_predictors(`(Intercept)` = "Baseline: Rural",
                                       degree_of_urbanization_urban_centric_localeTown = "Town",
                                       degree_of_urbanization_urban_centric_localeSuburb = "Suburb",
                                       degree_of_urbanization_urban_centric_localeCity = "City") +
                    theme_classic() +
                    ggtitle("Correlation between Locale \n & African American Student Population") +
                    theme(legend.position = "none")
            }
        }
         
    })
    output$aa_institution <- renderPlot({
        if(input$plot_type_aa == "a") {
            ggplot(mean_data, aes(x = institution_size_category, y = mean_african_american)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Size of Institution") + 
                ylab("Percent Black or African American") +
                theme_classic() + 
                labs(title = "Percent of Black or African Students by Size of Institution",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_african_american, color = "red")
        }
        else{
            if(input$plot_type_aa == "b") {
                ggplot(mean_data, aes(x = as.factor(degree_of_urbanization_urban_centric_locale), y = mean_african_american)) +
                    geom_boxplot(alpha = 0.25) +
                    xlab("Level of Urbanization of the Institution") + 
                    ylab("Percent Black or African American") +
                    theme_classic() + 
                    labs(title = "Percent of Black or African Students by Level of Urbanization",
                         subtitle = "With comparison at national population estimate") +
                    geom_hline(yintercept = census_data_2$pct_african_american, color = "red")
            }
        }
        
    })
    output$aa_women <- renderPlot({
        if(input$plot_type_aa == "a") {
            ggplot(mean_data, aes(x = as.factor(quantiles_women), y = mean_african_american)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Percent of Female Students") + 
                ylab("Percent Black or African American Students") +
                theme_classic() + 
                labs(title = "Percent of Black or African Students by Percent of Female Students",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_african_american, color = "red")
        }
        else{}
    })
    
    output$aa_admissions <- renderPlot ({ 
        if(input$plot_type_aa == "a") {
            ggplot(mean_data, aes(x = as.factor(quantiles_admission), y = mean_african_american)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Admissions Rate") + 
                ylab("Percent Black or African American Students") +
                theme_classic() + 
                labs(title = "Percent of Black or African Students by Admissions Rate",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_african_american, color = "red")
        }
        else{}
    })
    
    output$h_coefficients <- renderPlot({
        if(input$plot_type_h == "a") {
            model_hispanic <- lm(mean_hispanic ~ mean_enrollment + 
                                     mean_women + 
                                     mean_admitted,
                                 data = mean_data) %>%
                tidy(conf.int = TRUE)
            dwplot(model_hispanic) %>%
                relabel_predictors(mean_enrollment = "Undergraduate Enrollment",
                                   mean_women = "Percentage of Women",
                                   mean_admitted = "Admissions Rate") +
                theme_classic() +
                ggtitle("Correlation between Independent Variables and Latinx Student Population") +
                theme(legend.position = "none") 
        }
        else{
            if(input$plot_type_h == "b") {
                model_hispanic_2 <- lm(mean_hispanic ~ degree_of_urbanization_urban_centric_locale,
                                       data = mean_data) %>%
                    tidy(conf.int = TRUE)
                
                dwplot(model_hispanic_2, show_intercept = TRUE) %>%
                    relabel_predictors(`(Intercept)` = "Baseline: Rural",
                                       degree_of_urbanization_urban_centric_localeTown = "Town",
                                       degree_of_urbanization_urban_centric_localeSuburb = "Suburb",
                                       degree_of_urbanization_urban_centric_localeCity = "City") +
                    theme_classic() +
                    ggtitle("Correlation between Locale and Latinx Student Population") +
                    theme(legend.position = "none")
            }
        }
    })
    output$h_institution <- renderPlot({
        if(input$plot_type_h == "a") {
            ggplot(mean_data, aes(x = as.factor(institution_size_category), y = mean_hispanic)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Size of Institution") + 
                ylab("Percent Latinx Students") +
                theme_classic() + 
                labs(title = "Percent of Latinx Students by Size of Institution",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_hispanic, color = "red") 
        }
        else{
            if(input$plot_type_h == "b"){
                ggplot(mean_data, aes(x = as.factor(degree_of_urbanization_urban_centric_locale), y = mean_hispanic)) +
                    geom_boxplot(alpha = 0.25) +
                    xlab("Level of Urbanization of the Institution") + 
                    ylab("Percent Latinx Students") +
                    theme_classic() + 
                    labs(title = "Percent of Latinx Students by Level of Urbanization",
                         subtitle = "With comparison at national population estimate") +
                    geom_hline(yintercept = census_data_2$pct_hispanic, color = "red")
            }
        }
    })
    output$h_women <- renderPlot({
        if(input$plot_type_h == "a"){
            ggplot(mean_data, aes(x = as.factor(quantiles_women), y = mean_hispanic)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Percent Women") + 
                ylab("Percent Latinx Students") +
                theme_classic() + 
                labs(title = "Percent of Latinx Students by Percent of Female Students",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_hispanic, color = "red")
        }
        else{}
    })
    output$h_admissions <- renderPlot({
        if(input$plot_type_h == "a"){
            ggplot(mean_data, aes(x = as.factor(quantiles_admission), y = mean_hispanic)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Admissions Rate") + 
                ylab("Percent Latinx Students") +
                theme_classic() + 
                labs(title = "Percent of Latinx Students by Admissions Rate",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_hispanic, color = "red")
        }
        else{}
    })
    
    output$a_coefficients <- renderPlot({
        if(input$plot_type_a == "a") {
            model_asian <- lm(mean_asian ~ mean_enrollment + 
                                  mean_women + 
                                  mean_admitted,
                              data = mean_data) %>%
                tidy(conf.int = TRUE)
            dwplot(model_asian) %>%
                relabel_predictors(mean_enrollment = "Undergraduate Enrollment",
                                   mean_women = "Percentage of Women",
                                   mean_admitted = "Admissions Rate") +
                theme_classic() +
                ggtitle("Correlation between Independent Variables \n & Asian Student Population") +
                theme(legend.position = "none") 
        }
        else{
            if(input$plot_type_a == "b") {
                model_asian_2 <- lm(mean_asian ~ degree_of_urbanization_urban_centric_locale,
                                    data = mean_data) %>%
                    tidy(conf.int = TRUE)
                
                dwplot(model_asian_2, show_intercept = TRUE) %>%
                    relabel_predictors(`(Intercept)` = "Baseline: Rural",
                                       degree_of_urbanization_urban_centric_localeTown = "Town",
                                       degree_of_urbanization_urban_centric_localeSuburb = "Suburb",
                                       degree_of_urbanization_urban_centric_localeCity = "City") +
                    theme_classic() +
                    ggtitle("Correlation between Locale \n & Asian Student Population") +
                    theme(legend.position = "none")
            }
        }
        
    })
    output$a_institution <- renderPlot({
        if(input$plot_type_a == "a") {
            ggplot(mean_data, aes(x = as.factor(institution_size_category), y = mean_asian)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Size of Institution") + 
                ylab("Percent Asian Students") +
                theme_classic() + 
                labs(title = "Percent of Asian Students by Size of Institution",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_asian, color = "red")
            
        }
        else{
            if(input$plot_type_a == "b") {
                ggplot(mean_data, aes(x = as.factor(degree_of_urbanization_urban_centric_locale), y = mean_asian)) +
                    geom_boxplot(alpha = 0.25) +
                    xlab("Level of Urbanization of the Institution") + 
                    ylab("Percent Asian Students") +
                    theme_classic() + 
                    labs(title = "Percent of Asian Students by Level of Urbanization",
                         subtitle = "With comparison at national population estimate") +
                    geom_hline(yintercept = census_data_2$pct_asian, color = "red")
            }
        }
        
    })
    output$a_women <- renderPlot({
        if(input$plot_type_a == "a") {
            ggplot(mean_data, aes(x = as.factor(quantiles_women), y = mean_asian)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Percent Women") + 
                ylab("Percent Asian Students") +
                theme_classic() + 
                labs(title = "Percent of Asian Students by Percent of Female Students",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_asian, color = "red")
        }
        else{}
    })
    
    output$a_admissions <- renderPlot ({ 
        if(input$plot_type_a == "a") {
            ggplot(mean_data, aes(x = as.factor(quantiles_admission), y = mean_asian)) +
                geom_boxplot(alpha = 0.25) +
                xlab("Admissions Rate") + 
                ylab("Percent Asian Students") +
                theme_classic() + 
                labs(title = "Percent of Asian Students by Admissions Rate",
                     subtitle = "With comparison at national population estimate") +
                geom_hline(yintercept = census_data_2$pct_asian, color = "red")
        }
        else{}
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

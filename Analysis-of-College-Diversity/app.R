#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# These are the libraries required for the code below to run and for the app
# to be published

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
library(shinythemes)

# When mean_data.csv was originally saved, the levels for the factors were
# defined but they needed to be redefined after redownload.
# This .csv file is actually the wrangled version of 10 joined education data 
# sets from earlier iterations of the project

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

# This another data set from which I pulled information

census_data_2 <- read_csv("census.csv", col_types = cols())


# I used the shinytheme() package to create a nice format for the app

ui <- fluidPage(theme = shinytheme("flatly"),
                
    # I started with the page containing the Latinx population at colleges 
    # because they are the largest minority group in the United States 
                
    ui <- navbarPage(
        "Diversity of Upper Level Education",
        tabPanel("Latinx Student Population",
                 fluidPage(
                     sidebarPanel(
                         width = 6,
                         
                         # This is the code for the pull down menu
                         
                         selectInput(
                             "plot_type_h",
                             "Independent Variables",
                             c("Size of Institution, Percent Women, Admissions Rate" = "a",
                               "Locale" = "b")),
                         h4("Correlation Coefficients"),
                         
                         # These correspond to the plot and text on the left 
                         # side of the page 
                         
                         plotOutput("h_coefficients"),
                         textOutput("h_output_1")
                         ),
                     mainPanel(width = 6,
                               
                               # These correspond to the graphics on the right
                               # side of the page. I chose to summarize the data
                               # shown in the graphics here instead of on a 
                               # separate page 
                               
                               plotOutput("h_institution"),
                               plotOutput("h_women"),
                               plotOutput("h_admissions"))
                 )),
        tabPanel("African American Student Population",
                 fluidPage(
                     sidebarPanel(
                         
                         # I tried a couple of different values for the width
                         # of the sidebar. 6 seemed to be the best option 
                         # because the graphic needed adequate space 
                         
                         width = 6,
                         selectInput(
                             "plot_type_aa",
                             "Independent Variables",
                             
                             # These are the two options in the pull down menu
                             # This is the same for the three population pages
                             
                             c("Size of Institution, Percent Women, Admissions Rate" = "a", 
                               "Locale" = "b")),
                         h4("Correlation Coefficients"),
                         
                         # This corresponds to the graphic and text on the left
                         # side of the page. I chose to speak about the results 
                         # of my project here instead of on a separate page 
                         # because I thought it made more sense and viewers 
                         # could easily refer to the graphics 
                         
                         plotOutput("aa_coefficients"),
                         textOutput("aa_output_1")
                     ),
                     
                     # These correspond to the plots on the right side of the
                     # pages
                     
                     mainPanel(width = 6, 
                               plotOutput("aa_institution"),
                               plotOutput("aa_women"),
                               plotOutput("aa_admissions"))
                 )),
        
        # Here is the third page. The design decision for this page was the same
        # as the two previous pages 
        
        tabPanel("Asian Student Population",
                 fluidPage(
                     sidebarPanel(
                         width = 6,
                         selectInput(
                             "plot_type_a",
                             "Independent Variables",
                             c("Size of Institution, Percent Women, Admissions Rate" = "a",
                               "Locale" = "b")),
                             h4("Correlation Coefficients"),
                             plotOutput("a_coefficients"),
                             textOutput("a_output_1")
                     ),
                     mainPanel(width = 6,
                               plotOutput("a_institution"),
                               plotOutput("a_women"),
                               plotOutput("a_admissions"))
                 )),
        
        # Here is the About Page. It contains the background information for the 
        # project 
        
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("Following the Harvard Admissions lawsuit (regarding the 
                   allegedly unfair admission practices for Asian Americans), I 
                   thought it would be interesting to determine what factors of 
                   upper level educational institutions correlate with racial 
                   and ethnic diversity of these college populations. At first, 
                   I planned to look at the admissions data for ivy league 
                   schools but soon realized that there would be very limited 
                   data, and the admissions practices of these top tier schools 
                   are already under severe scrutiny. Instead, I thought it 
                   would be more broadly applicable and interesting to consider 
                   the greater college sphere of the United States."),
                 p("There were approximately 7,000 schools that had provided 
                   their admissions data. From these schools, I chose the 
                   admissions data for the top three minority groups in the 
                   United States (Latinx, African American, and Asian 
                   individuals) to study. Note, I also have the information for 
                   other minorities for future consideration. Then, I chose 
                   several independent variables with which I could compare the 
                   three race variables. These independent variables are: size 
                   of the institution, percent of the university population that
                   are women, admissions rate, and location of the campus. 
                   I used these variables to determine what may contribute to 
                   racially diverse universities vs. racially uniform 
                   universities using linear models."),
                 h3("Data"),
                 p("The", 
                   
                   # Here is a link to where you can create the original data
                   # sets before wrangling and joining 
                   
                   a(href = "https://nces.ed.gov/ipeds/use-the-data", "National Center 
                 for Education Statistics"), 
                   "has  data admissions data for almost 7,000 upper level 
                 educational institutions from 1980 to 2018. I used their 
                 survey data to create custom data files with the variables I 
                 wanted to investigate. I downloaded 9 datasets from 2010 to 
                 2018, which enabled me to take mean admissions data for each 
                 institution over the time period. I chose to use the mean data 
                 (instead of data from individual years) in order lessen the 
                 effects of any anomalous data. In addition to the National 
                 Center for Education Statistics Data, I used", 
                   
                   # Here is information regarding the tidycensus package which
                   # I used to create the census_data_2 file 
                   
                   a(href = "https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf", 
                     "tidycensus"),
                   "package which has population data for the United States for 
                 many years and down to the county level. I chose 2010 data 
                 at the country level with which to compare college racial data, 
                 so I could determine overrepresentation or underrepresentation 
                 of minorities."),
                 h3("About Me"),
                 p("My name is Emma Freeman. I am a first year at Harvard 
                 planning to study computer science. I'm a member of Harvard's
                 Open Data Project and enjoy data science. For more information 
                 about the project, you can reach me at 
                 efreeman@college.harvard.edu."),
                 p("The code for this project can be found at this",
                   
                   # Here's the repo containing this code. It is sufficiently
                   # cleaned up and should only contain the code for this graph
                   # Original milestones and data wrangling for this project are 
                   # no longer present
                   
                   a(href = "https://github.com/emfree19/analysis-of-college-diversity", "github repo"), "."))
        )
)
        


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$aa_output_1 <- renderText({
        
        # This is the summary of the data on the African American page when
        # the menu option is the first one. It summarizes the result of the data
        # here (and not on a separate page). The reasoning for this is in the 
        # code above 
        
        if(input$plot_type_aa == "a") {
            "Above, the plot shows the correlation coefficients between the mean 
            percent of African American students and the variables undergraduate enrollment 
            (this represents the continuous scale for size of institution), the 
            percent of female students, and the admissions rate of colleges. The 
            correlation coefficient for the linear model comparing mean percent 
            of African American students and undergraduate enrollment is very close to zero,
            which indicates that there is not a significant correlation between
            the two variables. Similarly, the correlation coefficient for admissions
            rate is close to 0, suggesting a negligible relationship between
            to variables. The correlation coefficent for the percentage of women
            is 0.15, which suggests that a 1% increase in the percent of female
            students at an institution corresponds to a 0.15% increase in 
            African American Students. 
            The graphs to the right show the boxplots for the data represented
            by the correlation coefficients but on a categorical scale instead of 
            a continuous scale. I decided to represent this data with boxplots
            instead of scatterplots because the scatterplots were prone to 
            overplotting."
        }
        
        # This is the summary of the results of the locale data 
        
        else{
            if(input$plot_type_aa == "b"){
                "On this page, we have another independent variable, locale of 
                the institutions. There are four possible options: Rural, Town,
                Suburb, and City. The coefficient plot above, shows the intercept
                (Rural) at ~8%. Furthermore, the correlation coefficients for
                institutions located in towns, suburbs, and cities are positive (to
                varying degrees) suggesting a tendency to a greater percentage of 
                African American students as level of urbanity increases."
            }
        }
        
    })
    
    # These are the plots shown on the African American page for both options
    # in the pull down menu
    
    output$aa_coefficients <- renderPlot({
        if(input$plot_type_aa == "a") {
            model_african_american <- lm(mean_african_american ~ mean_enrollment + 
                                             mean_women + 
                                             mean_admitted,
                                         data = mean_data) %>%
                tidy(conf.int = TRUE)
            dwplot(model_african_american, color = "red") %>%
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
                
                dwplot(model_african_american_2, show_intercept = TRUE, color = "red") %>%
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
    
    # This is the output for the Latinx page. The design decisions for the 
    # individuals graphs were defined in earlier iterations/milestones for the
    # project 
    
    output$h_output_1 <- renderText({
        if(input$plot_type_h == "a") {
            "Above, the plot shows the correlation coefficients between the mean 
            percent of Latinx students and the variables undergraduate enrollment 
            (this represents the continuous scale for size of institution), the 
            percent of female students, and the admissions rate of colleges. The 
            correlation coefficient for the linear model comparing mean percent 
            of Latinx students and undergraduate enrollment is very close to zero,
            which indicates that there is not a significant correlation between
            the two variables. Percentage of Female Students and Admissions Rate 
            have a slightly more negative relationship with percent of Latinx students
            but both are very slight negative corrleations. 
            The graphs to the right show the boxplots for the data represented
            by the correlation coefficients but on a categorical scale instead of 
            a continuous scale. I decided to represent this data with boxplots
            instead of scatterplots because the scatterplots were prone to 
            overplotting. Nonetheless, the boxplots also have their cons: to viewers,
            they may seem to show a greater relationship between the variables 
            than is given. That is why is showed both the correlation coefficients
            and the box plots."
        }
        else{
            if(input$plot_type_h == "b"){
                "On this page, we have another independent variable, locale of 
                the institutions. There are four possible options: Rural, Town,
                Suburb, and City. The coefficient plot above, shows the intercept
                (Rural) at ~8%. Furthermore, the correlation coefficient for institutions
                located in town is negative, while the correlation coefficient for suburb
                and city are positive. These represent the respective mean decrease
                and increase in percent of Latinx students the is represented in the boxplot."
            }
        }

    })
    output$h_coefficients <- renderPlot({
        if(input$plot_type_h == "a") {
            model_hispanic <- lm(mean_hispanic ~ mean_enrollment + 
                                     mean_women + 
                                     mean_admitted,
                                 data = mean_data) %>%
                tidy(conf.int = TRUE)
            dwplot(model_hispanic, dot_args = list(color = "dark blue"), whisker_args = list(color = "dark blue")) %>%
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
                
                dwplot(model_hispanic_2, show_intercept = TRUE, dot_args = list(color = " dark blue"), whisker_args = list(color = " dark blue")) %>%
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
                geom_hline(yintercept = census_data_2$pct_hispanic, color = "dark blue") 
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
                    geom_hline(yintercept = census_data_2$pct_hispanic, color = " dark blue")
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
                geom_hline(yintercept = census_data_2$pct_hispanic, color = "dark blue")
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
                geom_hline(yintercept = census_data_2$pct_hispanic, color = "dark blue")
        }
        else{}
    })
    
    
    # Here is the code for the text and the graphics on the Asian page 
    
    output$a_output_1 <- renderText({
        if(input$plot_type_a == "a") {
            "Above, the plot shows the correlation coefficients between the mean 
            percent of Asian students and the variables undergraduate enrollment 
            (this represents the continuous scale for size of institution), the 
            percent of female students, and the admissions rate of colleges. The 
            correlation coefficient for the linear model comparing mean percent 
            of Asian students and undergraduate enrollment is very close to zero,
            which indicates that there is not a significant correlation between
            the two variables. Percentage of Female Students and Admissions Rate 
            have a slightly more negative relationship with percent of Asian students.
            In fact, the correlation between percent of Asian students and admissions
            rate is -0.15, suggesting that an increase of one percent of admissions 
            rate corresponds to a 0.15% decrease in the percent of Asian students.
            The graphs to the right show the boxplots for the data represented
            by the correlation coefficients but on a categorical scale instead of 
            a continuous scale. I decided to represent this data with boxplots
            instead of scatterplots because the scatterplots were prone to 
            overplotting."
        }
        else{
            if(input$plot_type_a == "b"){
                "On this page, we have another independent variable, locale of 
                the institutions. There are four possible options: Rural, Town,
                Suburb, and City. The coefficient plot above, shows the intercept
                (Rural) at ~3.5%. Furthermore, the correlation coefficient for institutions
                located in town is negative, while the correlation coefficient for suburb
                and city are positive. These represent the respective mean decrease
                and increase in percent of Latinx students the is represented in the boxplot."
            }
        }
        
    })
    output$a_coefficients <- renderPlot({
        if(input$plot_type_a == "a") {
            model_asian <- lm(mean_asian ~ mean_enrollment + 
                                  mean_women + 
                                  mean_admitted,
                              data = mean_data) %>%
                tidy(conf.int = TRUE)
            dwplot(model_asian, dot_args = list(color = " dark green"), whisker_args = list(color = " dark green")) %>%
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
                
                dwplot(model_asian_2, show_intercept = TRUE, dot_args = list(color = "dark green"), whisker_args = list(color = " dark green")) %>%
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
                geom_hline(yintercept = census_data_2$pct_asian, color = "dark green")
            
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
                    geom_hline(yintercept = census_data_2$pct_asian, color = "dark green")
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
                geom_hline(yintercept = census_data_2$pct_asian, color = "dark green")
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
                geom_hline(yintercept = census_data_2$pct_asian, color = "dark green")
        }
        else{
            element_blank()
        }
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

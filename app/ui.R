if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
if (!require("leaflet")) { install.packages("leaflet", repos="http://cran.us.r-project.org")}
library(leaflet)
library(highcharter)

ui <- dashboardPage(
  skin="yellow",
  dashboardHeader(title = "NYC COVID-19 & Vaccine tracker",titleWidth= 250),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Time Series", tabName = "Time Series", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("chart-bar"),
               startExpanded = TRUE,
               menuSubItem("By Rates",tabName = "Rates"),
               menuSubItem("Age",tabName = "Age"),
               menuSubItem("Sex",tabName = "Sex"),
               menuSubItem("Race",tabName = "Race"),
               menuSubItem("Antibody",tabName = "Antibody")),
      menuItem("Vaccine Info", tabName = "Vaccine_info", icon = icon("syringe"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      #Homepage
      # First tab content
      tabItem(tabName = "Home",
              fluidPage(
              h2("NYC COVID-19 Tracker & Vaccine Site Information",align = "center"),
              fluidRow(width = 20, 
                       br(),
                       h3("Chuanchuan Liu, Daizy Lam, Hao Hu, Yiwen Fang, Zhihang Xia",align = "center"),
                       br(),
                       h4("New York City reported the first COVID-19 cases on March 1,2020 and our lives have changed drastically onwards. New York State has the highest numbers of confirmed cases in the United States till mid-July and most cases were in New York City where half of the population lives. NYC took control of the virus with the leadership of governor Cuomo implementing a complete shutdown, masks mandates, stopping all business and restricting social gatherings. Although cases have been reduced in NYC, the pandemic is far from over as the virus mutates and cold weather and holiday season leads to resurgence.",align = "left"),
                       br(),
                       h4("In effort to stop the pandemic, The U.S. Department of Health & Human Services expedited the vaccine development program with Operation Warp Speed. In early November, both Moderna and Pfizer released promising results on vaccine trials and by mid December 2020, both Moderna and Pfizer vaccines were approved by the FDA.",align = "Left"),
                       br(),
                       h4("Knowing that COVID-19 vaccine is a vital tool to stop the pandemic, we designed this website to help New Yorkers get vaccine information and track virus updates in different areas.",align = "Left"),
                       br(),
                       style = "background-image: url('../nyc_bg.jpeg');
                                    background-repeat:no-repeat;background-size:cover;
                                      opacity: 0.7;
                                    background-position:center"
                       
                       
                       
                       
                       ),
              
              fluidRow(
                         width = 12,
                         h2("Overall COVID-19 Case & Death",align = "center"),
                      
                         valueBoxOutput("total"),
                         valueBoxOutput("death")),
              fluidRow(
                         valueBoxOutput("max_case"),
                         valueBoxOutput("max_death"),
                         )
                       
      )),
      
      #Rates
      tabItem(tabName = "Rates",
              fluidPage(
                fluidRow(titlePanel("Interactive COVID-19 rate data by borough"),
                         fluidRow(
                           valueBoxOutput('max_case_rate'),
                           valueBoxOutput('max_death_rate'),
                           valueBoxOutput('max_hos_rate')
                         ),
                         h4("Let's look into the COVID-19 data by the five boroughs in New York to compare the different situations in case rate, death rate and hospitalization by age group, sex and race/ethnicity."),
                         br(),
                         h3("Findings"),
                         br(),
                         h4(""),
                         
                         
                         
                ),

                fluidRow(
                  
                tabsetPanel(
                  type="tabs",
                  tabPanel("Age",
                          fluidRow(
                            selectizeInput('select_borough_a','Select the Borough',
                                     choices = c('Boroughs' = '','Bronx','Brooklyn',
                                                 'Manhattan','Queens','StatenIsland'), multiple = TRUE),
                      highchartOutput('boro_age_cr_bar',width = "100%"),
                      splitLayout(cellWidths = c("50%", "50%"),
                                  highchartOutput('boro_age_dr_bar',width = "80%"),
                                  highchartOutput('boro_age_hr_bar',width = "80%")),
                    )
                    ),
                    tabPanel("Sex",
                             h4("Let's look into the COVID-19 data by the five boroughs in New York to understand the different situations in the area"),
                             fluidRow(
                               selectizeInput('select_borough_s','Select the Borough',
                                              choices = c('Boroughs' = '','Bronx','Brooklyn',
                                                          'Manhattan','Queens','StatenIsland'), multiple = TRUE),
                               br(),
                               highchartOutput('boro_sex_cr_bar',width = "100%"),
                               splitLayout(cellWidths = c("50%", "50%"),
                                           highchartOutput('boro_sex_dr_bar',width = "80%"),
                                           highchartOutput('boro_sex_hr_bar',width = "80%")),
                             )
                             ),
                    tabPanel("Race/ethnicity",                          
                             h4("Let's look into the COVID-19 data by the five boroughs in New York to understand the different situations in the area"),
                             fluidRow(
                               selectizeInput('select_borough_r','Select the Borough',
                                              choices = c('Boroughs' = '','Bronx','Brooklyn',
                                                          'Manhattan','Queens','StatenIsland'), multiple = TRUE),
                               highchartOutput('boro_race_cr_bar',width = "100%"),
                               splitLayout(cellWidths = c("50%", "50%"),
                                           highchartOutput('boro_race_dr_bar',width = "80%"),
                                           highchartOutput('boro_race_hr_bar',width = "80%")),
                             ))
                  ))
                  
                
              )),
                
                  
      
      # Second tab content
      tabItem(tabName = "map",
              box(
                width=2,
                title = "Control Panel", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                checkboxGroupInput("borough", label = h3("Borough"), 
                                   choices = list("Manhattan" = "Manhattan", "Staten Island" = "Staten Island", "Bronx" = "Bronx", "Queens" = "Queens", "Brooklyn" = "Brooklyn"),
                                   selected = list("Manhattan", "Staten Island", "Bronx", "Queens", "Brooklyn")),
                radioButtons("data_type", label = h3("Data Type"),
                             choices = list("Number of confirmed cases" = "COVID_CASE_COUNT", 
                                            "Number of confirmed deaths" = "COVID_DEATH_COUNT",
                                            "Number of people tested for COVID-19" = "TOTAL_COVID_TESTS",
                                            "Rate of confirmed cases per 100,000 people" = "COVID_CASE_RATE",
                                            "Rate of confirmed deaths per 100,000 people" = "COVID_DEATH_RATE",
                                            "Percentage of people with positive COVID-19 test results" = "PERCENT_POSITIVE"
                                            ),
                             selected = "COVID_CASE_COUNT")
              ),
              box(
                width=10,
                h2("Map tab content"),
                tags$style(type = "text/css", "#map {height: calc(100vh - 150px) !important;}"),
                leafletOutput("map")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      ),
      
      tabItem(tabName = "Vaccine_info",
              h2("Vaccine Info tab content"),
              leafletOutput("vaccine_map")
      )
    )
  )
  
)

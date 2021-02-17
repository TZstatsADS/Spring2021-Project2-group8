if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
if (!require("leaflet")) { install.packages("leaflet", repos="http://cran.us.r-project.org")}
library(leaflet)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)
if (!require("highcharter")) { install.packages("highcharter")}
library(highcharter)

ui <- dashboardPage(
  
  skin = "purple",
  dashboardHeader(title = "NYC COVID-19 & Vaccine tracker", titleWidth = 250),
  
  # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Time Series", tabName = "Time_series", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("chart-bar"),
               startExpanded = TRUE,
               menuSubItem("By Rates",tabName = "Rates"),
               menuSubItem("By Counts",tabName = "Counts")),
      menuItem("Vaccine Info", tabName = "Vaccine_info", icon = icon("syringe")),
      menuItem("About", tabName="About", icon = icon("list-alt"))
    )
  ),
  
  # Body content
  dashboardBody(
    tabItems(
      # Home tab content
      tabItem(tabName = "Home",
              fluidPage(
                h2(strong("Let's get vaccinated, NYC!"),align = "center"),
                h3("Chuanchuan Liu, Daizy Lam, Hao Hu, Yiwen Fang, Zhihang Xia",align = "center",style="color:olive"),
                h4("2021Spring GR5243 Project2 Group8 - M.A. Statistics program at Columbia University",align = "center",style="color:olive"),
                
                fluidRow(width = 20, 
                         br(),
                         h3("New York City reported the first COVID-19 cases on March 1,2020 and our lives have changed drastically onwards. New York State has the highest numbers of confirmed cases in the United States till mid-July and most cases were in New York City where half of the population lives. NYC took control of the virus with the 
                          leadership of governor Cuomo implementing a complete shutdown, masks mandates, stopping all business and restricting social gatherings. Although cases have been reduced in NYC, the pandemic is far from over as the virus mutates and cold weather and holiday season leads to resurgence.",align = "left"),
                         br(),
                         h3("In effort to stop the pandemic, The U.S. Department of Health & Human Services expedited the vaccine development program with Operation Warp Speed. In early November, both Moderna and Pfizer released promising results on vaccine trials and by mid December 2020, both Moderna and Pfizer vaccines were approved by the FDA.
                          Knowing that COVID-19 vaccine is a vital tool to stop the pandemic, we designed this website to help New Yorkers get vaccine information and track virus updates in different areas.",align = "Left"),
                         br(),
                         style = "background-image: url('../nyc_bg.jpeg');
                                    background-repeat:no-repeat;background-size:cover;
                                      opacity: 0.9;
                                    background-position:center"
                ),
                
                fluidRow(hr(),
                         column(width = 5,img(src="covid_vaccine.jpg",width = "100%", height = "35%"),align = "center",
                                p(strong("Current Vaccine Eligibility:")," Phase 1b - Beginning February 15",
                                  br(),
                                  a(href="https://www1.nyc.gov/site/doh/covid/covid-19-vaccine-eligibility.page", "Check Eligibility",target="_blank"),style="text-align:center;color:black")),
                         
                         box(width = 5, height = "10%", h2(strong("NYC Vaccine App"),align = "center"),
                             background = "purple",
                             h4("Welcome to our NYC Vaccine App,
                            We want to provide New Yorkers with updated COVID-19 information and help locate the closest vaccine site."),
                             br(),
                             tags$div(
                               "1. Click the Map tab to understand the COVID situation in your target area", 
                               tags$br(),
                               tags$br(),
                               "2. Click the vaccine info tab to search for the closest vaccine site with a particular zip code, location, and vaccine type",
                               tags$br(),
                               tags$br(),
                               "3. Click the Time Series tab to understand the timeline of the COVID development in NYC",
                               tags$br(),
                               tags$br(),
                               "4. Click the analysis tab to deep dive into the data by borough in different age group, sex and race."
                             ) ),
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
      
      # Rates
      tabItem(tabName = "Rates",
              fluidPage(
                fluidRow(titlePanel("Interactive COVID-19 Rates Dashboard"),
                         fluidRow(
                           valueBoxOutput('max_case_rate'),
                           valueBoxOutput('max_death_rate'),
                           valueBoxOutput('max_hos_rate')
                         ),
                         h4("Let's look into the COVID-19 data by the five boroughs in New York to compare the different situations in case rate, death rate and hospitalization by age group, sex and race/ethnicity."),
                         br(),
                         h3("Data by borough")
                ),
                
                fluidRow(
                  tabsetPanel(
                    type="tabs",
                    tabPanel("Age",
                             h4("The following charts compares the data by age in the five boroughs"),
                             fluidRow(
                               selectizeInput('select_borough_a','Select the Borough',
                                              choices = c('Boroughs' = '','Bronx','Brooklyn',
                                                          'Manhattan','Queens','StatenIsland'), selected = list("Bronx", "Brooklyn", "Manhattan", "Queens", "StatenIsland"), multiple = TRUE),
                               highchartOutput('boro_age_cr_bar',width = "100%"),
                               splitLayout(cellWidths = c("50%", "50%"),
                                           highchartOutput('boro_age_dr_bar',width = "80%"),
                                           highchartOutput('boro_age_hr_bar',width = "80%")),
                             )
                    ),
                    tabPanel("Sex",
                             h4("The following charts compares the data by sex in the five boroughs"),
                             fluidRow(
                               selectizeInput('select_borough_s','Select the Borough',
                                              choices = c('Boroughs' = '','Bronx','Brooklyn',
                                                          'Manhattan','Queens','StatenIsland'), selected = list("Bronx", "Brooklyn", "Manhattan", "Queens", "StatenIsland"), multiple = TRUE),
                               br(),
                               highchartOutput('boro_sex_cr_bar',width = "100%"),
                               splitLayout(cellWidths = c("50%", "50%"),
                                           highchartOutput('boro_sex_dr_bar',width = "80%"),
                                           highchartOutput('boro_sex_hr_bar',width = "80%")),
                             )
                    ),
                    tabPanel("Race/ethnicity",                          
                             h4("The following charts compares the race/ethnicity in the five borough"),
                             fluidRow(
                               selectizeInput('select_borough_r','Select the Borough',
                                              choices = c('Boroughs' = '','Bronx','Brooklyn',
                                                          'Manhattan','Queens','StatenIsland'), selected = list("Bronx", "Brooklyn", "Manhattan", "Queens", "StatenIsland"), multiple = TRUE),
                               highchartOutput('boro_race_cr_bar',width = "100%"),
                               splitLayout(cellWidths = c("50%", "50%"),
                                           highchartOutput('boro_race_dr_bar',width = "80%"),
                                           highchartOutput('boro_race_hr_bar',width = "80%")),
                             ))
                  ))
              )),
      
      # Counts
      tabItem(tabName = "Counts",
              box(
                width=6,
                h4("The bar chart below shows that Brooklyn and Queens in NYC are more vulnerable to COVID-19."),
                h4("Please click on the select box to see more specific distribution by sex, race, and age."),
                br(),
                
                sidebarPanel(width = 2.5,
                  selectInput("count","Please Select Count Type",
                              choices=c("case count","death count","hospitalized count")),
                  selectInput("group","Please Select Group Type",
                              choices=c("overview","sex","race","age"))
                ),
                
                mainPanel(
                  plotOutput(width = '150%', "myPlot")
                )), 
              
              box(
                width=6,
                h4("Please click on the select box to see more specific distribution of positive tests number & rate and confirmed death & probable death."),
                br(),
                
                mainPanel(
                  fluidRow(
                    tabsetPanel(id = "tpanel",
                                type = "tabs",
                                tabPanel("number of test positive", plotOutput("plot1")),
                                tabPanel("percentage of test positive", plotOutput("plot2")),
                                tabPanel("comfirmed death",  plotOutput("plot3")),
                                tabPanel("probable death", plotOutput("plot4")))
                  )
                ))
      ),
      
      # Map tab content
      tabItem(tabName = "map",
              box(
                width=2,
                title = "Control Panel", status = "primary",
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
                tags$style(type = "text/css", "#map {height: calc(100vh - 150px) !important;}"),
                leafletOutput("map")
              )
      ),
      
      # Time Series tab content
      tabItem(tabName = "Time_series",
              titlePanel("Daily Increase"),
              box(width=15, sidebarPanel(selectInput("select", label = h4("DATA"), 
                                                     choices = list("Cases" = "CASE_COUNT", "Deaths" = "DEATH_COUNT", "Hospitalizations" = "HOSPITALIZED_COUNT"), 
                                                     selected = "CASE_COUNT"),
                                         selectInput("select2", label = h4("RANGE"), 
                                                     choices = list("Citywise" = "CT", "Manhattan" = "MN_", "Staten Island" = "SI_", "Bronx" = "BX_", "Queens" = "QN_","Brooklyn" = "BK_"), 
                                                     selected = "CT"),
                                         radioButtons("lines", label = h4("LINE TYPE"), 
                                                      choices = list("Connect" = "Line", "Smooth" = "Trend"),
                                                      selected = "Line")
              ),
              mainPanel(plotlyOutput("TSplot", width="100%", height="500px"))
              )),
      
      # Vaccine Info tab content
      tabItem(tabName = "Vaccine_info",
              h2("Vaccine info map"),
              leafletOutput("vaccine_map"),
              h2("The Vaccine data"),
              DT::dataTableOutput("vaccine_table"),
              h2("More information about vaccine_offered type"),
              uiOutput("tab"),
              h2("Prerequisite for vaccination"),
              uiOutput("vac")
      ),
      
      tabItem(tabName = "About",
              box(width=12, h1(strong("ABOUT"), align="center")),     
              mainPanel(width=12, h2(strong("Contributors"), align="center")),
              h4("Yiwen Fang, yf2560@columbia.edu", align="center"),
              h4("Zhihang Xia, zx2338@columbia.edu", align="center"),
              h4("Daizy Lam, wl2744@columbia.edu", align="center"),
              h4("Chuanchuan Liu, cl4048@columbia.edu", align="center"),
              h4("Hao Hu, hh2874@columbia.edu", align="center"),
              br(),
              
              mainPanel(width=12, h2(strong("Data Source"), align="center"),
                        h4("The data source used is from", a("NYC Coronavirus Disease 2019 Data", 
                                                             href="https://github.com/nychealth/coronavirus-data"), 
                           " and ", a("NYC COVID-19 Vaccine Finder",
                                      href="https://vaccinefinder.nyc.gov/locations"),
                           ". ",align="center")
              ),
              br(),
              mainPanel(width=12, h2(strong("Code"), align="center"),
                        h4("More detailed codes are shared at", a("Github", 
                                                                  href="https://github.com/TZstatsADS/Spring2021-Project2-group8"), ". ", align="center"))
      )
    )
  )
)

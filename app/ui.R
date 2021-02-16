if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
if (!require("leaflet")) { install.packages("leaflet", repos="http://cran.us.r-project.org")}
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Vaccine Info", tabName = "Vaccine_info", icon = icon("syringe")),
      ##(added)
      menuItem("Analysis", tabName="Analysis", icon = icon("bar-chart-o")),
      menuItem("About", tabName="About", icon = icon("list-alt"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content")
      ),
      
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
      ),
      
      ##(added) 
      tabItem(tabName = "Analysis",
             
              mainPanel(
                h4("The bar chart below shows that Brooklyn and Queens in NYC are more vulnerable to COVID-19."),
                h4("Please click on the select box to see more specific distribution by sex, race, and sex."),
                br(),
              ), 
              
               box(
                width=10,
                
                sidebarPanel(
                  selectInput("count","Please Select Count Type",
                              choices=c("case count","death count","hospitalized count")),
                  selectInput("group","Please Select Group Type",
                              choices=c("overview","sex","race","age"))
                ),
                
                mainPanel(
                  plotOutput("myPlot")
                )), 
              
              br(),
          
                box(
                  width=10,
                  
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


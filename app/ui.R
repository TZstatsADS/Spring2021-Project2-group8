if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
if (!require("leaflet")) { install.packages("leaflet", repos="http://cran.us.r-project.org")}
library(leaflet)
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
      menuItem("Vaccine Info", tabName = "Vaccine_info", icon = icon("syringe"))
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
              leafletOutput("vaccine_map"),
              h2("The Vaccine data"),
              DT::dataTableOutput("vaccine_table"),
              h2("More information about vaccine_offered type"),
              uiOutput("tab"),
              h2("Prerequisite for vaccination"),
              uiOutput("vac")

      )

    )
  )
  
)

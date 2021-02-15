if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("leaflet")) { install.packages("leaflet", repos="http://cran.us.r-project.org")}
library(leaflet)
if (!require("dplyr")) { install.packages("dplyr")}
library(dplyr)
if (!require("tigris")) { install.packages("tigris")}
library(tigris)
if (!require("tidyverse")) { install.packages("tidyverse")}
library(tidyverse)
if (!require("DT")) { install.packages("DT")}
library(DT)


sum.formula = JS("function (cluster) {    
    var markers = cluster.getAllChildMarkers();
    var sum = 0; 
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.mag);
    }
      var size = sum/10000;
      var mFormat = ' marker-cluster-';
      if(sum < 3000) {
      mFormat += 'small'
      } else if (sum > 13000) {
      mFormat += 'large'
      } else {
      mFormat += 'medium'};
      return L.divIcon({ html: '<div><span>' + sum + '</span></div>', className: 'marker-cluster'+mFormat, iconSize: L.point(40, 40) });
  }")


server <- function(input, output) {
    data_by_modzcta=read.csv(file="../data/data-by-modzcta.csv")
    zipcode_latitude_longitude=read.csv(file="../data/zipcode_latitude_longitude.csv")
    data_by_lat_lng = data_by_modzcta %>%
        inner_join(zipcode_latitude_longitude, by = "MODIFIED_ZCTA") %>%
        mutate(zipcode=as.character(MODIFIED_ZCTA))
    
    data_by_lat_lng_input <- reactive({
        data_by_lat_lng %>%
            filter(BOROUGH_GROUP %in% input$borough)
    })
    
    data_type_input <- reactive({
        input$data_type
    })
    
    # cache zip boundaries that are download via tigris package
    options(tigris_use_cache = TRUE)
    
    # get zip boundaries that start with 282 (outdated example)
    char_zips <- zctas(cb = TRUE)
    
    # Map tab: Yiwen Fang --------------------------------------------------------
    
    # https://api.rpubs.com/insight/leaflet
    output$map <- renderLeaflet({
        
        # join zip boundaries and income data 
        char_zips <- geo_join(char_zips, 
                              data_by_lat_lng_input(), 
                              by_sp = "GEOID10", 
                              by_df = "zipcode",
                              how = "inner")
        
        # create color palette 
        pal <- colorNumeric(
            palette = "Blues",
            domain = char_zips[[data_type_input()]])
        
        # create labels for zipcodes
        labels <- 
            paste0(
                "<b>", "Infomation", "</b><br/>",
                "COVID_CASE_COUNT: ", as.character(char_zips$COVID_CASE_COUNT), "<br/>",
                "COVID_CASE_RATE: ", as.character(char_zips$COVID_CASE_RATE), "<br/>",
                "COVID_DEATH_COUNT: ", as.character(char_zips$COVID_DEATH_COUNT), "<br/>",
                "COVID_DEATH_RATE: ", as.character(char_zips$COVID_DEATH_RATE), "<br/>",
                "PERCENT_POSITIVE: ", as.character(char_zips$PERCENT_POSITIVE), "<br/>",
                "TOTAL_COVID_TESTS: ", as.character(char_zips$TOTAL_COVID_TESTS)) %>%
            lapply(htmltools::HTML)
        
        # This if controls for no circles showing if rates are selected
        if (data_type_input() %in% list("COVID_CASE_COUNT", "COVID_DEATH_COUNT", "TOTAL_COVID_TESTS")) {
            map <- leaflet(char_zips) %>%
                # set view to New York City
                setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
                addProviderTiles("CartoDB.DarkMatter", options = providerTileOptions(noWrap = TRUE)) %>%
                addCircleMarkers(
                    lng=~longitude,
                    lat=~latitude,
                    color = 'red',
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    options = markerOptions(mag = char_zips[[data_type_input()]]),
                    clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula)),
                    popup=~paste(
                        "<b>", "Infomation", "</b><br/>",
                        "COVID_CASE_COUNT: ", as.character(COVID_CASE_COUNT), "<br/>",
                        "COVID_CASE_RATE: ", as.character(COVID_CASE_RATE), "<br/>",
                        "COVID_DEATH_COUNT: ", as.character(COVID_DEATH_COUNT), "<br/>",
                        "COVID_DEATH_RATE: ", as.character(COVID_DEATH_RATE), "<br/>",
                        "PERCENT_POSITIVE: ", as.character(PERCENT_POSITIVE), "<br/>",
                        "TOTAL_COVID_TESTS: ", as.character(TOTAL_COVID_TESTS)
                    )
                ) %>%
                addLabelOnlyMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    options = markerOptions(mag = char_zips[[data_type_input()]]),
                    label = char_zips[[data_type_input()]],
                    labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T),
                    clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula))) %>%
                # add zip codes
                addPolygons(fillColor = ~pal(char_zips[[data_type_input()]]),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.7,
                            highlight = highlightOptions(weight = 2,
                                                         color = "#FF0000",
                                                         dashArray = "",
                                                         fillOpacity = 0.7,
                                                         bringToFront = TRUE),
                            label = labels) %>%
                # add legend
                addLegend(pal = pal, 
                          values = char_zips[[data_type_input()]], 
                          opacity = 0.7, 
                          title = htmltools::HTML(data_type_input()),
                          position = "topright")
        } else {
            map <- leaflet(char_zips) %>%
                # set view to New York City
                setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
                addProviderTiles("CartoDB.DarkMatter", options = providerTileOptions(noWrap = TRUE)) %>%
                # add zip codes
                addPolygons(fillColor = ~pal(char_zips[[data_type_input()]]),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.7,
                            highlight = highlightOptions(weight = 2,
                                                         color = "#FF0000",
                                                         dashArray = "",
                                                         fillOpacity = 0.7,
                                                         bringToFront = TRUE),
                            label = labels) %>%
                # add legend
                addLegend(pal = pal, 
                          values = char_zips[[data_type_input()]], 
                          opacity = 0.7, 
                          title = htmltools::HTML(data_type_input()),
                          position = "topright")
        }
        
    })
    
    # Vaccine tab: Yiwen Fang HAO HU----------------------------------------------------------------
    
    Covid_Vaccine=read.csv(file="../data/Covid_Vaccine.csv")
    output$vaccine_map <- renderLeaflet({
        map <- leaflet(Covid_Vaccine) %>%
            # set view to New York City
            setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
            addProviderTiles("CartoDB.DarkMatter", options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(
                lng=~Longitude,
                lat=~Latitude,
                color = 'red',
                stroke = FALSE,
                fillOpacity = 0.5,
                clusterOptions = markerClusterOptions(),
                popup=~paste(
                    "<b>", "Infomation", "</b><br/>",
                    "Site Name: ", as.character(Name), "<br/>",
                    "Type: ", as.character(Type), "<br/>",
                    "Address: ", as.character(Location), "<br/>",
                    "Zip Code: ", as.character(Zip_code), "<br/>",
                    "Vaccine Offered: ", as.character(Vaccine_offered), "<br/>"
                )
            )
    })
    
    output$vaccine_table = DT::renderDataTable({
        Covid_Vaccine[1:5]})

    url1 <- a("Moderna/Pfizer-BioNTech COVID-19 vaccine", href="https://www.cdc.gov/coronavirus/2019-ncov/vaccines/different-vaccines.html")
    output$tab <- renderUI({
      tagList("URL link :", url1)
    })
    
    url2 <- a("Vaccine Eligibility", href="https://www1.nyc.gov/site/doh/covid/covid-19-vaccine-eligibility.page")
    output$vac <- renderUI({
      tagList("URL link :", url2)
    })

    
    
}

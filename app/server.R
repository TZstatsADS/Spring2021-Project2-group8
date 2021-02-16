library(highcharter)
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
  
  # Home tab: Daizy Lam --------------------------------------------------------
  
  output$total <- renderValueBox({
    valueBox(
      h4("Total Case Count"),
      h3(sum(data_by_modzcta$COVID_CASE_COUNT)),
      icon = icon("list"),
      color = "aqua"
    )
  })
  
  output$death <- renderValueBox({
    valueBox(
      h4("Total Death Count"),
      h3(sum(data_by_modzcta$COVID_DEATH_COUNT)),
      icon = icon("user"),
      color = "olive"
    )
  })
  
  output$max_case <- renderValueBox({
    valueBox(
      h4("Max Case Count: Queens"),
      h3(max(by_borough$CASE_COUNT)),
      icon = icon("list"),
      color = "aqua"
    )
  })
  
  output$max_death <- renderValueBox({
    valueBox(
      h4("Max Death Count: Queens"),
      h3(max(by_borough$DEATH_COUNT)),
      icon = icon("user"),
      color = "olive"
    )
  })
  
  # Analysis tab: 1) Rates: Daizy Lam --------------------------------------------------------
  data_borough=read.csv("../data/group-data-by-boro-edit.csv")
  data_borough_selection_a <- reactive({
    if(is.null(input$select_borough_a)){selected_boro = levels(data_borough$Borough)}
    else{selected_boro = input$select_borough_a}  
    data_borough %>%
      filter(Borough %in% selected_boro)
    })
  
  data_borough_selection_s <- reactive({
    if(is.null(input$select_borough_s)){selected_boro = levels(data_borough$Borough)}
    else{selected_boro = input$select_borough_s}  
    data_borough %>%
      filter(Borough %in% selected_boro)
  })
  data_borough_selection_r <- reactive({
    if(is.null(input$select_borough_r)){selected_boro = levels(data_borough$Borough)}
    else{selected_boro = input$select_borough_r}  
    data_borough %>%
      filter(Borough %in% selected_boro)
  })
  #Max Rate:
  by_borough=read.csv("../data/by-boro.csv")
  output$max_case_rate <- renderValueBox({
    valueBox(
      h4("Highest Case Rate : StatenIsland"),
      h3(max(by_borough$CASE_RATE)),
      icon = icon("percentage"),
      color = "maroon"
    )
  })
  output$max_death_rate <- renderValueBox({
    valueBox(
      h4("Highest Death Rate : Bronx"),
      h3(max(by_borough$DEATH_RATE)),
      icon = icon("percentage"),
      color = "fuchsia"
    )
  })
  output$max_hos_rate <- renderValueBox({
    valueBox(
      h4("Highest Hospitalized Rate : Bronx"),
      h3(max(by_borough$HOSPITALIZED_RATE)),
      icon = icon("percentage"),
      color = "purple"
    )
  })
  
  # Rate bar chart group by Age
  output$boro_age_cr_bar <- renderHighchart({
    data_borough_selection_a() %>%
      filter(group=='Age') %>%
      group_by(Borough,subgroup)%>%
      select(CASE_RATE)  %>%
      hchart('column', hcaes(x=subgroup, y=CASE_RATE, group=Borough))
  })
  
  output$boro_age_dr_bar <- renderHighchart({
    data_borough_selection_a() %>%
      filter(group=='Age') %>%
      group_by(Borough,subgroup)%>%
      select(DEATH_RATE)  %>%
      hchart('column', hcaes(x=subgroup, y=DEATH_RATE, fill=Borough, group=Borough))
  }) 
  
  output$boro_age_hr_bar <- renderHighchart({
    data_borough_selection_a() %>%
      filter(group=='Age') %>%
      group_by(Borough,subgroup)%>%
      select(HOSPITALIZED_RATE)  %>%
      hchart('column', hcaes(x=subgroup, y=HOSPITALIZED_RATE, group=Borough))
  }) 
  
  # Rate bar chart group by Sex

  output$boro_sex_cr_bar <- renderHighchart({
    data_borough_selection_s() %>%
      filter(group=='Sex') %>%
      group_by(Borough,subgroup)%>%
      select(CASE_RATE)  %>%
      hchart('column', hcaes(x=subgroup, y=CASE_RATE,group=Borough)) 
  })
  
  output$boro_sex_dr_bar <- renderHighchart({
    data_borough_selection_s() %>%
      filter(group=='Sex') %>%
      group_by(Borough,subgroup)%>%
      select(DEATH_RATE)  %>%
      hchart('column', hcaes(x=subgroup, y=DEATH_RATE,group=Borough)) 
  })
  
  output$boro_sex_hr_bar <- renderHighchart({
    data_borough_selection_s() %>%
      filter(group=='Sex') %>%
      group_by(Borough,subgroup)%>%
      select(HOSPITALIZED_RATE)  %>%
      hchart('column', hcaes(x=subgroup, y=HOSPITALIZED_RATE,group=Borough)) 
  })

  # Rate bar chart group by Race
  
  output$boro_race_cr_bar<- renderHighchart({
    data_borough_selection_r() %>%
    filter(group=='Race/ethnicity') %>%
    group_by(Borough,subgroup)%>%
    select(CASE_RATE,Borough,subgroup)  %>%
    hchart('column', hcaes(x=subgroup, y=CASE_RATE,group=Borough))
  })
  
  output$boro_race_dr_bar<- renderHighchart({
    data_borough_selection_r() %>%
      filter(group=='Race/ethnicity') %>%
      group_by(Borough,subgroup)%>%
      select(CASE_RATE,Borough,subgroup)  %>%
      hchart('column', hcaes(x=subgroup, y=CASE_RATE,group=Borough))
  })


  output$boro_race_hr_bar<- renderHighchart({
    data_borough_selection_r() %>%
      filter(group=='Race/ethnicity') %>%
      group_by(Borough,subgroup)%>%
      select(CASE_RATE,Borough,subgroup)  %>%
      hchart('column', hcaes(x=subgroup, y=CASE_RATE,group=Borough))
  })

  
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
  
  # Vaccine tab: Yiwen Fang ----------------------------------------------------------------
  
  Covid_Vaccinne=read.csv(file="../data/Covid_Vaccinne.csv")
  output$vaccine_map <- renderLeaflet({
    map <- leaflet(Covid_Vaccinne) %>%
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
}

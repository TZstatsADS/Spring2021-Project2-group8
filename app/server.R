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
if (!require("ggplot2")) { install.packages("ggplot2")}
library(ggplot2)
if (!require("lubridate")) { install.packages("lubridate")}
library(lubridate)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)
if (!require("hrbrthemes")) { install.packages("hrbrthemes")}
library(hrbrthemes)
if (!require("highcharter")) { install.packages("highcharter")}
library(highcharter)
if (!require("RColorBrewer")) { install.packages("RColorBrewer")}
library(RColorBrewer)

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
  data_by_modzcta=read.csv(file="./output/data-by-modzcta.csv")
  zipcode_latitude_longitude=read.csv(file="./output/zipcode_latitude_longitude.csv")
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
      palette = "Purples",
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
  
  # Analysis tab: 1) Rates: Daizy Lam ------------------------------------------
  
  data_borough=read.csv("./output/group-data-by-boro-edit.csv")
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
  by_borough=read.csv("./output/by-boro.csv")
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
      h4("Highest Death Rate : Brooklyn"),
      h3(max(by_borough$DEATH_RATE)),
      icon = icon("percentage"),
      color = "fuchsia"
    )
  })
  output$max_hos_rate <- renderValueBox({
    valueBox(
      h4("Highest Hospitalized Rate : Brooklyn"),
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
  
  # Vaccine tab: Hao Hu---------------------------------------------------------
  
  Covid_Vaccine=read.csv(file="./output/Covid_Vaccine.csv")
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
  
  # Time series tab: Zhihang Xia -----------------------------------------------
  
  data_by_date <- read.csv("./output/now-data-by-day.csv")
  data_by_date <- data_by_date%>%
    select(-contains(c("PROB","AVG","INCOMPLETE")))%>%
    mutate(date_of_interest=parse_date_time(date_of_interest,orders="mdy"))%>%
    mutate(date_of_interest=as.Date(date_of_interest))
  
  
  output$TSplot <- renderPlotly({
    if(input$select2=="CT"){
      p <- ggplot(mapping=aes(x=data_by_date[["date_of_interest"]], y=data_by_date[[input$select]]))+
        geom_point(size=0.7, colour="red")+
        scale_x_date(date_labels = "%Y %b %d")+
        theme_ipsum()+
        xlab("")+
        ylab(input$select)+
        labs(title=paste("Citywise_", input$select, sep=""))
      if (input$lines==c("Trend")){
        return(ggplotly(p+geom_smooth(colour="red")))
      }
      else if (input$lines==c("Line")){
        return(ggplotly(p+geom_line(colour="red")))
      }
    }
    else{
      p <- ggplot(mapping=aes(x=data_by_date[["date_of_interest"]], y=data_by_date[[paste(input$select2,input$select,sep="")]]))+
        geom_point(size=0.7, colour="orange")+
        scale_x_date(date_labels = "%Y %b %d")+
        theme_ipsum()+
        xlab("")+
        ylab(input$select)+
        labs(title=paste(input$select2, input$select, sep=""))
      if (input$lines==c("Trend")){
        return(ggplotly(p+geom_smooth(colour="orange")))
      }
      else if (input$lines==c("Line")){
        return(ggplotly(p+geom_line(colour="orange")))
      }
    }
  })
  
  # Counts tab: Chuanchuan Liu -------------------------------------------------
  
  output$myPlot <- renderPlot({
    
    countType <- input$count
    groupType <- input$group
    
    by_boro = read.csv("./output/by-boro_origin.csv",header=TRUE,sep=",")
    
    if (countType == "case count" && groupType == "overview"){
      barplot(by_boro$CASE_COUNT, main='case count by borough', 
              names.arg=c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland','Citywide'), 
              xlab="borough", ylab="case count", col=brewer.pal(6,"Set3"))
    }
    
    group_cases_by_boro = read.csv("./output/group-cases-by-boro.csv",header=TRUE,sep=",")
    df1 <- cbind(group_cases_by_boro$BK_CASE_COUNT, group_cases_by_boro$BX_CASE_COUNT, group_cases_by_boro$MN_CASE_COUNT, group_cases_by_boro$QN_CASE_COUNT, group_cases_by_boro$SI_CASE_COUNT)
    colnames(df1) <- c('BK_CASE_COUNT', 'BX_CASE_COUNT', 'MN_CASE_COUNT', 'QN_CASE_COUNT', 'SI_CASE_COUNT')
    rownames(df1) <- c('boroughwide','0-4','5-12','13-17','18-24','25-34','35-44','45-54','55-64','65-74','75+','asian','black','hispanic','white','female','male')
    
    if (countType == "case count" && groupType == "sex"){
      barplot(df1[16:17,1:5], main='case count by borough and sex', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="case count", 
              legend=rownames(df1[16:17,1:5]), beside=TRUE, col=c("lightblue","lavender"))
    }
    if (countType == "case count" && groupType == "race"){
      barplot(df1[12:15,1:5], main='case count by borough and race', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="case count",
              legend=rownames(df1[12:15,1:5]), beside=TRUE, col=brewer.pal(4,"Set3"))
    }
    if (countType == "case count" && groupType == "age"){
      barplot(df1[2:11,1:5], main='case count by borough and age', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="case count",
              legend=rownames(df1[2:11,1:5]), beside=TRUE, col=brewer.pal(10,"Set3"))
    }
    
    if (countType == "death count" && groupType == "overview"){
      barplot(by_boro$DEATH_COUNT, main='death count by borough', 
              names.arg=c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland','Citywide'), 
              xlab="borough", ylab="death count", col=brewer.pal(6,"Set3"))
    }
    
    group_death_by_boro = read.csv("./output/group-death-by-boro.csv",header=TRUE,sep=",")
    df2 <- cbind(group_death_by_boro$BK_DEATH_COUNT, group_death_by_boro$BX_DEATH_COUNT, group_death_by_boro$MN_DEATH_COUNT, group_death_by_boro$QN_DEATH_COUNT, group_death_by_boro$SI_DEATH_COUNT)
    colnames(df2) <- c('BK_DEATH_COUNT', 'BX_DEATH_COUNT', 'MN_DEATH_COUNT', 'QN_DEATH_COUNT', 'SI_DEATH_COUNT')
    rownames(df2) <- c('boroughwide','0-17','18-24','25-34','35-44','45-54','55-64','65-74','75+','asian','black','hispanic','white','female','male')
    
    if (countType == "death count" && groupType == "sex"){
      barplot(df2[14:15,1:5], main='death count by borough and sex', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="death count", 
              legend=rownames(df2[14:15,1:5]), beside=TRUE, col=c("lightblue","lavender"))
    }
    if (countType == "death count" && groupType == "race"){
      barplot(df2[10:13,1:5], main='death count by borough and race', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="death count",
              legend=rownames(df2[10:13,1:5]), beside=TRUE, col=brewer.pal(4,"Set3"))
    }
    if (countType == "death count" && groupType == "age"){
      barplot(df2[2:9,1:5], main='death count by borough and age', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="death count",
              legend=rownames(df2[2:9,1:5]), beside=TRUE, col=brewer.pal(8,"Set3"))
    }
    
    if (countType == "hospitalized count" && groupType == "overview"){
      barplot(by_boro$HOSPITALIZED_COUNT, main='hospitalized count by borough', 
              names.arg=c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland','Citywide'), 
              xlab="borough", ylab="hospitalized count", col=brewer.pal(6,"Set3"))
    }
    
    group_hosp_by_boro = read.csv("./output/group-hosp-by-boro.csv",header=TRUE,sep=",")
    df3 <- cbind(group_hosp_by_boro$BK_HOSPITALIZED_COUNT, group_hosp_by_boro$BX_HOSPITALIZED_COUNT, group_hosp_by_boro$MN_HOSPITALIZED_COUNT, group_hosp_by_boro$QN_HOSPITALIZED_COUNT, group_hosp_by_boro$SI_HOSPITALIZED_COUNT)
    colnames(df3) <- c('BK_HOSPITALIZED_COUNT', 'BX_HOSPITALIZED_COUNT', 'MN_HOSPITALIZED_COUNT', 'QN_HOSPITALIZED_COUNT', 'SI_HOSPITALIZED_COUNT')
    rownames(df3) <- c('boroughwide','0-4','5-12','13-17','18-24','25-34','35-44','45-54','55-64','65-74','75+','asian','black','hispanic','white','female','male')
    
    if (countType == "hospitalized count" && groupType == "sex"){
      barplot(df3[16:17,1:5], main='hospitalized count by borough and sex', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="hospitalized count", 
              legend=rownames(df1[16:17,1:5]), beside=TRUE, col=c("lightblue","lavender"))
    }
    if (countType == "hospitalized count" && groupType == "race"){
      barplot(df3[12:15,1:5], main='hospitalized count by borough and race', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="hospitalized count",
              legend=rownames(df1[12:15,1:5]), beside=TRUE, col=brewer.pal(4,"Set3"))
    }
    if (countType == "hospitalized count" && groupType == "age"){
      barplot(df3[2:11,1:5], main='hospitalized count by borough and age', 
              names.arg=c('Brooklyn','Bronx','Manhattan','Queens','Statenlsland'), 
              xlab="borough", ylab="hospitalized count",
              legend=rownames(df1[2:11,1:5]), beside=TRUE, col=brewer.pal(10,"Set3"))
    }
    
  })
  
  # Positive tests number & rate and confirmed death & probable death: Chuanchuan Liu ---------------------------
  
  antibody_by_boro = read.csv("./output/antibody-by-boro.csv",header=TRUE,sep=",")
  
  output$plot1 <- renderPlot({
    if (input$tpanel == "number of test positive"){
      slices <- antibody_by_boro$NUM_PEOP_POS
      lbls <- c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland')
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct)
      lbls <- paste(lbls,"%",sep="")
      pie(slices,labels = lbls, col=brewer.pal(5,"Set2"),
          main="number of people test positive by borough")
    }
  })
  
  output$plot2 <- renderPlot({
    if (input$tpanel == "percentage of test positive"){
      slices <- antibody_by_boro$PERCENT_POSITIVE
      lbls <- c('Bronx','Brooklyn','Manhattan','Queens','Statenlsland')
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) 
      lbls <- paste(lbls,"%",sep="") 
      pie(slices,labels = lbls, col=brewer.pal(5,"Set2"),
          main="percentage of people test positive by borough")
    }
  })
  
  prob_comf_by_boro = read.csv("./output/probable-confirmed-by-boro.csv",header=TRUE,sep=",")
  
  output$plot3 <- renderPlot({
    if (input$tpanel == "comfirmed death"){
      slices <- prob_comf_by_boro$CONFIRMED_DEATH
      lbls <- c('Queens','Brooklyn','Bronx','Manhattan','Statenlsland','Unknown','Data Pending')
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) 
      lbls <- paste(lbls,"%",sep="") 
      pie(slices,labels = lbls, col=brewer.pal(7,"Set2"),
          main="comfirmed death by borough")
    }
  })
  
  output$plot4 <- renderPlot({
    if (input$tpanel == "probable death"){
      slices <- prob_comf_by_boro$PROBABLE_DEATH
      lbls <- c('Queens','Brooklyn','Bronx','Manhattan','Statenlsland','Unknown','Data Pending')
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) 
      lbls <- paste(lbls,"%",sep="") 
      pie(slices,labels = lbls, col=brewer.pal(7,"Set2"),
          main="probable death by borough")
    }
  })
  
}

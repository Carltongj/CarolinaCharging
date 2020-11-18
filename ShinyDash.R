library(shiny) #For input/output
library(shinydashboard) #For dashboard structure
library(dplyr) #For data manipulation 
library(leaflet) #For making interactive geovisualizations
library(rgdal) #For working with spatial data
library(rsconnect) #For connecting to shinyapps.io
library(ggplot2) #For making attractive plots
library(plotly) #For enhancing plot output

#Some Quick Data Prep Before Working on the Actual Dashboard


#These are the two data sources I'll be working with, one is a non-spatial csv the other is a spatial shapefile.
Carolinas <- readOGR(dsn ="NC_EV/NC_EV", layer="NC_EV_SHP")
EV_Stations <- read.csv("EV_Stations_NCSC.csv", as.is = TRUE)


EV_Stations <- rename(EV_Stations, c("lat"="Latitude", "lng"="Longitude")) #This rename is essential for the leaflet map, which reckognizes 'lat' and 'lng' for geocoding
EV_Stations <- select(EV_Stations, -7:-12, -21) #Remove unecessary columns from my data frame.
EV_Icon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/7/77/Circle-icons-bolt.svg",
  iconWidth = 30,
  iconHeight = 30,
  iconAnchorX = 30,
  iconAnchorY = 30
) #Icon for use in leaflet map, basic height and width parameters established. Freely licensed image selected.

#Subsetting non-spatial data for individual state analysis.
EV_Stations_NC <- subset(EV_Stations, State == "NC")
EV_Stations_SC <- subset(EV_Stations, State == "SC")
EV_Net_NC<-count(EV_Stations_NC, EV_Network)
EV_Net_SC<-count(EV_Stations_SC, EV_Network)

#Subsetting spatial data for individual state analysis.
Carolinas_NC <- subset(Carolinas, STATEFP == "37")
Carolinas_SC <- subset(Carolinas, STATEFP == "45")
Carolinas_NS <- as.data.frame(Carolinas)
Carolinas_NS_NC <- as.data.frame(Carolinas_NC)
Carolinas_NS_SC <- as.data.frame(Carolinas_SC)


#Subsetting for regional barplots.
Triad <- subset(Carolinas_NS_NC, NAME %in% c("Guilford", "Forsyth", "Alamance", "Rockingham", "Randolph",
                                 "Stokes", "Davidson", "Davie", "Yadkin", "Surry"))
Triangle <- subset(Carolinas_NS_NC, NAME %in% c("Orange", "Chatham", "Sanford", "Person", "Harnett",
                                             "Durham", "Oxford", "Henderson", "Franklin", "Raleigh",
                                             "Johnston"))
Charlotte <- subset(Carolinas_NS, NAME %in% c("Iredell", "Rowan", "Cleveland", "Lincoln", "Gaston",
                                                "Mecklenburg", "Cabarrus", "Albemarle", "York", "Lancaster",
                                                "Chester", "Union"))
Charleston <- subset(Carolinas_NS_SC, NAME %in% c("Charleston", "Berkeley", "Dorchester"))
Columbia <- subset(Carolinas_NS_SC, NAME %in% c("Orangeburg", "Calhoun", "Lexington", "Saluda", "Richland",
                                                "Newberry", "Fairfield", "Kershaw"))
Greenville <- subset(Carolinas_NS_SC, NAME %in% c("Oconee", "Pickens", "Greenville", "Spartanburg", "Cherokee",
                                                "Anderson", "Laurens", "Abbeville", "Greenwood"))

#Creation of classes for leaflet geovisualizations
bins <- c(0, 1, 2, 5, 10)
PCO_10000 <- colorBin("Blues", Carolinas$PCO10000, bins = bins)
PCO_10000_2 <- colorBin("Blues", Carolinas_NC$PCO10000, bins = bins)
PCO_10000_3 <- colorBin("Blues", Carolinas_SC$PCO10000, bins = bins)

#Creation of interactive labels for leaflet geovisualizations
labels <- sprintf(
  "<strong>%s</strong><br/>%g EV Stations / 10,000 ppl",
  Carolinas$NAME, Carolinas$PCO10000
) %>% lapply(htmltools::HTML)
labels2 <- sprintf(
  "<strong>%s</strong><br/>%g EV Stations / 10,000 ppl",
  Carolinas_NC$NAME, Carolinas_NC$PCO10000
) %>% lapply(htmltools::HTML)
labels3 <- sprintf(
  "<strong>%s</strong><br/>%g EV Stations / 10,000 ppl",
  Carolinas_SC$NAME, Carolinas_SC$PCO10000
) %>% lapply(htmltools::HTML)

#Basic population estimates for value box
NC_Population <- 10049000 #2019 Census Estimate
SC_Population <- 5149000 #2019 Census Estimate
Carolina_Population <- NC_Population + SC_Population

#Start of User Interface
ui <- dashboardPage(
#Header title  
  dashboardHeader(title = "Carolina Charging"),
#Sidebar, including menu items leading to subpages
    sidebar <-  dashboardSidebar(
    sidebarMenu(
      menuItem("Carolina Dashboard", tabName = "home_dashboard", icon = icon("home")),
      menuItem("North Carolina", tabName = "nc_dashboard", icon = icon("dashboard")),
      menuItem("Charlotte", icon = icon("ellipsis-h"), tabName = "Charlotte",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Triangle Region", icon = icon("ellipsis-h"), tabName = "Triangle",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Triad Region", icon = icon("ellipsis-h"), tabName = "Triad",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("South Carolina", icon = icon("dashboard"), tabName = "sc_dashboard"),
      menuItem("Charleston", icon = icon("ellipsis-h"), tabName = "Charleston",
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Columbia", icon = icon("ellipsis-h"), tabName = "Columbia",
             badgeLabel = "new", badgeColor = "green"),
      menuItem("Greenville", icon = icon("ellipsis-h"), tabName = "Greenville",
             badgeLabel = "new", badgeColor = "green"),
      menuItem("Data Source", icon = icon("atlas"), href = "https://afdc.energy.gov/fuels/electricity_locations.html#/find/nearest?fuel=ELEC"
             ))
    
  ),

#Dashboard Body Renders

  dashboardBody(
    tabItems(
#First Dashboard, Home -- Includes 3 value boxes, 2 leaflet maps, a stacked barplot and a basic i/o dropdown.
      tabItem(tabName = "home_dashboard",
       fluidRow(valueBox("Charging Stations", count(EV_Stations), icon = icon("charging-station")),
                valueBox("Charging Outlets", sum(EV_Stations$Charging_Outlets), icon = icon("plug"), color = "orange"),
                valueBox("Outlets/10,000 People", sum(EV_Stations$Charging_Outlets)/Carolina_Population*10000, icon = icon("user-friends"), color = "purple")),
       fluidRow(
                box(title = "Map of Carolina Charging Stations",
                solidHeader = TRUE,
                status= "warning",
                width = 8,
                leafletOutput("EV_Map")),
                box(title = "Charging Facilities by Type",
                solidHeader = TRUE,
                status = "warning",
                width = 4,
                selectInput("Facility_Type", "Choose the Type of Facility", sort(unique(EV_Stations$Facility_Type)), selected = "AIRPORT")),
                box(title = "Charging Facilities by Type",
                solidHeader = TRUE,
                status = "warning",
                width = 4,
                height = 110,
                textOutput("Facility"),
                tags$head(tags$style("#Facility{color: black;
                                 font-size: 30px;
                                 font-style: strong;
                                 text-align: center;
                                 }"))
        )
    ),
        fluidRow( 
                box(title = "Charging Station Density by County",
                solidHeader = TRUE,
                status= "primary",
                width = 8,
                leafletOutput("EV_Map_Density")),
                box(title = "Composition of Charging Stations by Network",
                solidHeader = TRUE,
                status = "primary",
                width = 4,
                plotlyOutput("Network"),
                )
                ),

  ),

#Second Dashboard, North Carolina -- Includes 3 value boxes, 2 refined leaflet maps, a lolipop plot and a refined basic i/o dropdown.

  tabItem(tabName = "nc_dashboard",
          fluidRow(valueBox("Charging Stations", count(EV_Stations_NC), icon = icon("charging-station")),
                   valueBox("Charging Outlets", sum(EV_Stations_NC$Charging_Outlets), icon = icon("plug"), color = "orange"),
                   valueBox("Outlets/10,000 People", sum(EV_Stations_NC$Charging_Outlets)/NC_Population*10000, icon = icon("user-friends"), color = "purple")),
          fluidRow(
            box(title = "Map of North Carolina Charging Stations",
                solidHeader = TRUE,
                status= "warning",
                width = 8,
                leafletOutput("EV_Map_NC")),
            box(title = "Charging Facilities by Type",
                solidHeader = TRUE,
                status = "warning",
                width = 4,
                selectInput("Facility_Type_NC", "Choose the Type of Facility", sort(unique(EV_Stations$Facility_Type)), selected = "AIRPORT")),
            box(title = "Charging Facilities by Type",
                solidHeader = TRUE,
                status = "warning",
                width = 4,
                height = 110,
                textOutput("Facility_NC"),
                tags$head(tags$style("#Facility_NC{color: black;
                                 font-size: 30px;
                                 font-style: strong;
                                 text-align: center;
                                 }"))
            )
  ),
          fluidRow( 
            box(title = "Charging Station Density by County",
            solidHeader = TRUE,
            status= "primary",
            width = 8,
            leafletOutput("EV_Map_Density_NC")),
          box(title = "Composition of Charging Stations by Network",
            solidHeader = TRUE,
            status = "primary",
            width = 4,
            plotlyOutput("Network_NC"),
    )
  ),
),

#Third Dashboard, South Carolina -- Includes 3 value boxes, 2 refined leaflet maps, a lolipop plot and a refined basic i/o dropdown.

tabItem(tabName = "sc_dashboard",
        fluidRow(valueBox("Charging Stations", count(EV_Stations_SC), icon = icon("charging-station")),
                 valueBox("Charging Outlets", sum(EV_Stations_SC$Charging_Outlets), icon = icon("plug"), color = "orange"),
                 valueBox("Outlets/10,000 People", sum(EV_Stations_SC$Charging_Outlets)/SC_Population*10000, icon = icon("user-friends"), color = "purple")),
        fluidRow(
          box(title = "Map of South Carolina Charging Stations",
              solidHeader = TRUE,
              status= "warning",
              width = 8,
              leafletOutput("EV_Map_SC")),
          box(title = "Charging Facilities by Type",
              solidHeader = TRUE,
              status = "warning",
              width = 4,
              selectInput("Facility_Type_SC", "Choose the Type of Facility", sort(unique(EV_Stations_SC$Facility_Type)), selected = "AIRPORT")),
          box(title = "Charging Facilities by Type",
              solidHeader = TRUE,
              status = "warning",
              width = 4,
              height = 110,
              textOutput("Facility_SC"),
              tags$head(tags$style("#Facility_SC{color: black;
                                 font-size: 30px;
                                 font-style: strong;
                                 text-align: center;
                                 }"))
          )
        ),
        fluidRow( 
          box(title = "Charging Station Density by County",
              solidHeader = TRUE,
              status= "primary",
              width = 8,
              leafletOutput("EV_Map_Density_SC")),
          box(title = "Composition of Charging Stations by Network",
              solidHeader = TRUE,
              status = "primary",
              width = 4,
              plotlyOutput("Network_SC"),
          )
        ),
),

#Fourth Dashboard, Triad Region -- Includes 2 barplots, one standardized by population, one not.

tabItem(tabName = "Triad",
        fluidRow( 
          box(title = "Total # of Charging Outlets by County",
              solidHeader = TRUE,
              status= "primary",
              width = 12,
              plotlyOutput("Outlets_Triad")),
          box(title = "Charging Outlets per 10,000 People by County",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              plotlyOutput("PCO_Triad")),
          )
        ),

#Fifth Dashboard, Triangle Region -- Includes 2 barplots, one standardized by population, one not.

tabItem(tabName = "Triangle",
        fluidRow( 
          box(title = "Total # of Charging Outlets by County",
              solidHeader = TRUE,
              status= "primary",
              width = 12,
              plotlyOutput("Outlets_Triangle")),
          box(title = "Charging Outlets per 10,000 People by County",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              plotlyOutput("PCO_Triangle")),
        )
),

#Sixth Dashboard, Charlotte Region -- Includes 2 barplots, one standardized by population, one not.

tabItem(tabName = "Charlotte",
        fluidRow( 
          box(title = "Total # of Charging Outlets by County",
              solidHeader = TRUE,
              status= "primary",
              width = 12,
              plotlyOutput("Outlets_Charlotte")),
          box(title = "Charging Outlets per 10,000 People by County",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              plotlyOutput("PCO_Charlotte")),
        )
  ),

#Seventh Dashboard, Charleston Region -- Includes 2 barplots, one standardized by population, one not.

tabItem(tabName = "Charleston",
        fluidRow( 
          box(title = "Total # of Charging Outlets by County",
              solidHeader = TRUE,
              status= "primary",
              width = 12,
              plotlyOutput("Outlets_Charleston")),
          box(title = "Charging Outlets per 10,000 People by County",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              plotlyOutput("PCO_Charleston")),
        )
  ),

#Eighth Dashboard, Columbia Region -- Includes 2 barplots, one standardized by population, one not.

tabItem(tabName = "Columbia",
        fluidRow( 
          box(title = "Total # of Charging Outlets by County",
              solidHeader = TRUE,
              status= "primary",
              width = 12,
              plotlyOutput("Outlets_Columbia")),
          box(title = "Charging Outlets per 10,000 People by County",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              plotlyOutput("PCO_Columbia")),
        )
    ),

#Ninth Dashboard, Greenville Region -- Includes 2 barplots, one standardized by population, one not.

tabItem(tabName = "Greenville",
        fluidRow( 
          box(title = "Total # of Charging Outlets by County",
              solidHeader = TRUE,
              status= "primary",
              width = 12,
              plotlyOutput("Outlets_Greenville")),
          box(title = "Charging Outlets per 10,000 People by County",
              solidHeader = TRUE,
              status = "primary",
              width = 12,
              plotlyOutput("PCO_Greenville")),
        )
)

)
)
)

#I/O Server Section

server <- function(input, output){

#Leaflet Map, Point Based (with clusters on zoomout) for all of the Carolinas  
output$EV_Map <- renderLeaflet({
EV_Stations %>%
leaflet() %>%
addTiles() %>%
addMarkers(icon = EV_Icon , popup = EV_Stations$Station_Name, clusterOptions = markerClusterOptions())
  })

#Leaflet Map, Point Based (with clusters on zoomout) for only North Carolina 
output$EV_Map_NC <- renderLeaflet({
  EV_Stations_NC %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(icon = EV_Icon , popup = EV_Stations_NC$Station_Name, clusterOptions = markerClusterOptions())
})

#Leaflet Map, Point Based (with clusters on zoomout) for only South Carolina 
output$EV_Map_SC <- renderLeaflet({
  EV_Stations_SC %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(icon = EV_Icon , popup = EV_Stations_SC$Station_Name, clusterOptions = markerClusterOptions())
})

#Leaflet Map, Polygon Based for all the Carolinas based on Density
output$EV_Map_Density <- renderLeaflet({
Carolinas %>%
leaflet() %>%
addTiles() %>%
addPolygons(fillColor = ~PCO_10000(PCO10000),
                weight = 1,
                opacity = 0.7,
                color = "gray",
                dashArray = "0",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                weight = 3,
                color = "black",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
addLegend(PCO_10000, values = ~PCO_10000, opacity = 0.7, title = "Charging Outlets per 10,000 People",
                  position = "bottomright")
})

#Leaflet Map, Polygon Based for only North Carolina based on Density

output$EV_Map_Density_NC <- renderLeaflet({
  Carolinas_NC %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(fillColor = ~PCO_10000_2(PCO10000),
                weight = 1,
                opacity = 0.7,
                color = "gray",
                dashArray = "0",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 3,
                  color = "black",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels2,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(PCO_10000_2, values = ~PCO_10000_2, opacity = 0.7, title = "Charging Outlets per 10,000 People",
              position = "bottomright")
})

#Leaflet Map, Polygon Based for only South Carolina based on Density

output$EV_Map_Density_SC <- renderLeaflet({
  Carolinas_SC %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(fillColor = ~PCO_10000_3(PCO10000),
                weight = 1,
                opacity = 0.7,
                color = "gray",
                dashArray = "0",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 3,
                  color = "black",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels3,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(PCO_10000_3, values = ~PCO_10000_3, opacity = 0.7, title = "Charging Outlets per 10,000 People",
              position = "bottomright")
})

#Based on user input, calculates the sum of each type of charging facility for all the Carolinas

output$Facility <- renderText({
if(input$Facility_Type == "AIRPORT"){
  x1 <- sum(EV_Stations$Facility_Type == "AIRPORT")
  print(paste("Airports: ", x1))
}else if (input$Facility_Type == "AUTO_REPAIR"){
  x2 <- sum(EV_Stations$Facility_Type == "AUTO_REPAIR")
  print(paste("Auto Repair Shops: ", x2))
}else if (input$Facility_Type == "B_AND_B"){
  x3 <- sum(EV_Stations$Facility_Type == "B_AND_B")
  print(paste("Bed and Breakfasts: ", x3))
}else if (input$Facility_Type == "BANK"){
  x4 <- sum(EV_Stations$Facility_Type == "BANK")
  print(paste("Banks: ", x4))
}else if (input$Facility_Type == "BREWERY_DISTILLERY_WINERY"){
  x5 <- sum(EV_Stations$Facility_Type == "BREWERY_DISTILLERY_WINERY")
  print(paste("Breweries: ", x5))
}else if (input$Facility_Type == "CAMPGROUND"){
  x6 <- sum(EV_Stations$Facility_Type == "CAMPGROUND")
  print(paste("Campgrounds: ", x6))
}else if (input$Facility_Type == "CAR_DEALER"){
  x7 <- sum(EV_Stations$Facility_Type == "CAR_DEALER")
  print(paste("Car Dealerships: ", x7))
}else if (input$Facility_Type == "COLLEGE_CAMPUS"){
  x8 <- sum(EV_Stations$Facility_Type == "COLLEGE_CAMPUS")
  print(paste("College Campuses: ", x8))
}else if (input$Facility_Type == "CONVENIENCE_STORE"){
  x9 <- sum(EV_Stations$Facility_Type == "CONVENIENCE_STORE")
  print(paste("Convenience Stores: ", x9))
}else if (input$Facility_Type == "CONVENTION_CENTER"){
  x10 <- sum(EV_Stations$Facility_Type == "CONVENTION_CENTER")
  print(paste("Convention Centers: ", x10))
}else if (input$Facility_Type == "COOP"){
  x11 <- sum(EV_Stations$Facility_Type == "COOP")
  print(paste("Cooperative: ", x11))
}else if (input$Facility_Type == "GAS_STATION"){
  x12 <- sum(EV_Stations$Facility_Type == "GAS_STATION")
  print(paste("Gas Stations: ", x12))
}else if (input$Facility_Type == "GROCERY"){
  x13 <- sum(EV_Stations$Facility_Type == "GROCERY")
  print(paste("Grocery Stores: ", x13))
}else if (input$Facility_Type == "HOSPITAL"){
  x14 <- sum(EV_Stations$Facility_Type == "HOSPITALS")
  print(paste("Hospitals: ", x14))
}else if (input$Facility_Type == "HOTEL"){
  x15 <- sum(EV_Stations$Facility_Type == "HOTEL")
  print(paste("Hotel: ", x15))
}else if (input$Facility_Type == "INN"){
  x16 <- sum(EV_Stations$Facility_Type == "INN")
  print(paste("Inns: ", x16))
}else if (input$Facility_Type == "LIBRARY"){
  x17 <- sum(EV_Stations$Facility_Type == "LIBRARY")
  print(paste("Libraries: ", x17))
}else if (input$Facility_Type == "MUNI_GOV"){
  x18 <- sum(EV_Stations$Facility_Type == "MUNI_GOV")
  print(paste("Municipal Government Buildings: ", x18))
}else if (input$Facility_Type == "MUSEUM"){
  x19 <- sum(EV_Stations$Facility_Type == "MUSEUM")
  print(paste("Museums: ", x19))
}else if (input$Facility_Type == "OFFICE_BLDG"){
  x20 <- sum(EV_Stations$Facility_Type == "OFFICE_BLDG")
  print(paste("Office Buildings: ", x20))
}else if (input$Facility_Type == "OTHER"){
  x21 <- sum(EV_Stations$Facility_Type == "OTHER")
  print(paste("Other Facilities: ", x21))
}else if (input$Facility_Type == "OTHER_ENTERTAINMENT"){
  x22 <- sum(EV_Stations$Facility_Type == "OTHER_ENTERTAINMENT")
  print(paste("Other Entertainment Venues: ", x22))
}else if (input$Facility_Type == "PARK" ){
  x23 <- sum(EV_Stations$Facility_Type == "PARK")
  print(paste("Parks: ", x23))
}else if (input$Facility_Type == "PARKING_GARAGE"){
  x24 <- sum(EV_Stations$Facility_Type == "PARKING_GARAGE")
  print(paste("Parking Garages: ", x24))
}else if (input$Facility_Type == "PARKING_LOT"){
  x25 <- sum(EV_Stations$Facility_Type == "PARKING_LOT")
  print(paste("Parking Lots: ", x25))
}else if (input$Facility_Type == "PAY_GARAGE"){
  x26 <- sum(EV_Stations$Facility_Type == "PAY_GARAGE")
  print(paste("Paid Parking Garages: ", x26))
}else if (input$Facility_Type == "PAY_LOT"){
  x27 <- sum(EV_Stations$Facility_Type == "PAY_LOT")
  print(paste("Paid Parking Lots: ", x27))
}else if (input$Facility_Type == "REC_SPORTS_FACILITY"){
  x28 <- sum(EV_Stations$Facility_Type == "REC_SPORTS_FACILITY")
  print(paste("Sports Facilities: ", x28))
}else if (input$Facility_Type == "RESEARCH_FACILITY"){
  x29 <- sum(EV_Stations$Facility_Type == "RESEARCH_FACILITY")
  print(paste("Research Facilities: ", x29))
}else if (input$Facility_Type == "RESTAURANT"){
  x30 <- sum(EV_Stations$Facility_Type == "RESTAURANT")
  print(paste("Restaurants: ", x30))
}else if (input$Facility_Type == "RV_PARK"){
  x31 <- sum(EV_Stations$Facility_Type == "RV_PARK")
  print(paste("RV Parks: ", x31))
}else if (input$Facility_Type == "SHOPPING_CENTER"){
  x32 <- sum(EV_Stations$Facility_Type == "SHOPPING_CENTER")
  print(paste("Shopping Centers: ", x32))
}else if (input$Facility_Type == "STANDALONE_STATION"){
  x33 <- sum(EV_Stations$Facility_Type == "STANDALONE_STATION")
  print(paste("Standalone: ", x33))
}else if (input$Facility_Type == "STREET_PARKING"){
  x34 <- sum(EV_Stations$Facility_Type == "STREET_PARKING")
  print(paste("Street Parking: ", x34))
}else if (input$Facility_Type == "TRAVEL_CENTER"){
  x35 <- sum(EV_Stations$Facility_Type == "TRAVEL_CENTER")
  print(paste("Travel Centers: ", x35))
}else if (input$Facility_Type == "UTILITY"){
  x36 <- sum(EV_Stations$Facility_Type == "UTILITY")
  print(paste("Utility Centers: ", x36))
}else print("Error, try again.")
})

#Based on user input, calculates the sum of each type of charging facility for only North Carolina

output$Facility_NC <- renderText({
  if(input$Facility_Type_NC == "AIRPORT"){
    x1 <- sum(EV_Stations_NC$Facility_Type == "AIRPORT")
    print(paste("Airports: ", x1))
  }else if (input$Facility_Type_NC == "AUTO_REPAIR"){
    x2 <- sum(EV_Stations_NC$Facility_Type == "AUTO_REPAIR")
    print(paste("Auto Repair Shops: ", x2))
  }else if (input$Facility_Type_NC == "B_AND_B"){
    x3 <- sum(EV_Stations_NC$Facility_Type == "B_AND_B")
    print(paste("Bed and Breakfasts: ", x3))
  }else if (input$Facility_Type_NC == "BANK"){
    x4 <- sum(EV_Stations_NC$Facility_Type == "BANK")
    print(paste("Banks: ", x4))
  }else if (input$Facility_Type_NC == "BREWERY_DISTILLERY_WINERY"){
    x5 <- sum(EV_Stations_NC$Facility_Type == "BREWERY_DISTILLERY_WINERY")
    print(paste("Breweries: ", x5))
  }else if (input$Facility_Type_NC == "CAMPGROUND"){
    x6 <- sum(EV_Stations_NC$Facility_Type == "CAMPGROUND")
    print(paste("Campgrounds: ", x6))
  }else if (input$Facility_Type_NC == "CAR_DEALER"){
    x7 <- sum(EV_Stations_NC$Facility_Type == "CAR_DEALER")
    print(paste("Car Dealerships: ", x7))
  }else if (input$Facility_Type_NC == "COLLEGE_CAMPUS"){
    x8 <- sum(EV_Stations_NC$Facility_Type == "COLLEGE_CAMPUS")
    print(paste("College Campuses: ", x8))
  }else if (input$Facility_Type_NC == "CONVENIENCE_STORE"){
    x9 <- sum(EV_Stations_NC$Facility_Type == "CONVENIENCE_STORE")
    print(paste("Convenience Stores: ", x9))
  }else if (input$Facility_Type_NC == "CONVENTION_CENTER"){
    x10 <- sum(EV_Stations_NC$Facility_Type == "CONVENTION_CENTER")
    print(paste("Convention Centers: ", x10))
  }else if (input$Facility_Type_NC == "COOP"){
    x11 <- sum(EV_Stations_NC$Facility_Type == "COOP")
    print(paste("Cooperative: ", x11))
  }else if (input$Facility_Type_NC == "GAS_STATION"){
    x12 <- sum(EV_Stations_NC$Facility_Type == "GAS_STATION")
    print(paste("Gas Stations: ", x12))
  }else if (input$Facility_Type_NC == "GROCERY"){
    x13 <- sum(EV_Stations_NC$Facility_Type == "GROCERY")
    print(paste("Grocery Stores: ", x13))
  }else if (input$Facility_Type_NC == "HOSPITAL"){
    x14 <- sum(EV_Stations_NC$Facility_Type == "HOSPITALS")
    print(paste("Hospitals: ", x14))
  }else if (input$Facility_Type_NC == "HOTEL"){
    x15 <- sum(EV_Stations_NC$Facility_Type == "HOTEL")
    print(paste("Hotel: ", x15))
  }else if (input$Facility_Type_NC == "INN"){
    x16 <- sum(EV_Stations_NC$Facility_Type == "INN")
    print(paste("Inns: ", x16))
  }else if (input$Facility_Type_NC == "LIBRARY"){
    x17 <- sum(EV_Stations_NC$Facility_Type == "LIBRARY")
    print(paste("Libraries: ", x17))
  }else if (input$Facility_Type_NC == "MUNI_GOV"){
    x18 <- sum(EV_Stations_NC$Facility_Type == "MUNI_GOV")
    print(paste("Municipal Government Buildings: ", x18))
  }else if (input$Facility_Type_NC == "MUSEUM"){
    x19 <- sum(EV_Stations_NC$Facility_Type == "MUSEUM")
    print(paste("Museums: ", x19))
  }else if (input$Facility_Type_NC == "OFFICE_BLDG"){
    x20 <- sum(EV_Stations_NC$Facility_Type == "OFFICE_BLDG")
    print(paste("Office Buildings: ", x20))
  }else if (input$Facility_Type_NC == "OTHER"){
    x21 <- sum(EV_Stations_NC$Facility_Type == "OTHER")
    print(paste("Other Facilities: ", x21))
  }else if (input$Facility_Type_NC == "OTHER_ENTERTAINMENT"){
    x22 <- sum(EV_Stations_NC$Facility_Type == "OTHER_ENTERTAINMENT")
    print(paste("Other Entertainment Venues: ", x22))
  }else if (input$Facility_Type_NC == "PARK" ){
    x23 <- sum(EV_Stations_NC$Facility_Type == "PARK")
    print(paste("Parks: ", x23))
  }else if (input$Facility_Type_NC == "PARKING_GARAGE"){
    x24 <- sum(EV_Stations_NC$Facility_Type == "PARKING_GARAGE")
    print(paste("Parking Garages: ", x24))
  }else if (input$Facility_Type_NC == "PARKING_LOT"){
    x25 <- sum(EV_Stations_NC$Facility_Type == "PARKING_LOT")
    print(paste("Parking Lots: ", x25))
  }else if (input$Facility_Type_NC == "PAY_GARAGE"){
    x26 <- sum(EV_Stations_NC$Facility_Type == "PAY_GARAGE")
    print(paste("Paid Parking Garages: ", x26))
  }else if (input$Facility_Type_NC == "PAY_LOT"){
    x27 <- sum(EV_Stations_NC$Facility_Type == "PAY_LOT")
    print(paste("Paid Parking Lots: ", x27))
  }else if (input$Facility_Type_NC == "REC_SPORTS_FACILITY"){
    x28 <- sum(EV_Stations_NC$Facility_Type == "REC_SPORTS_FACILITY")
    print(paste("Sports Facilities: ", x28))
  }else if (input$Facility_Type_NC == "RESEARCH_FACILITY"){
    x29 <- sum(EV_Stations_NC$Facility_Type == "RESEARCH_FACILITY")
    print(paste("Research Facilities: ", x29))
  }else if (input$Facility_Type_NC == "RESTAURANT"){
    x30 <- sum(EV_Stations_NC$Facility_Type == "RESTAURANT")
    print(paste("Restaurants: ", x30))
  }else if (input$Facility_Type_NC == "RV_PARK"){
    x31 <- sum(EV_Stations_NC$Facility_Type == "RV_PARK")
    print(paste("RV Parks: ", x31))
  }else if (input$Facility_Type_NC == "SHOPPING_CENTER"){
    x32 <- sum(EV_Stations_NC$Facility_Type == "SHOPPING_CENTER")
    print(paste("Shopping Centers: ", x32))
  }else if (input$Facility_Type_NC == "STANDALONE_STATION"){
    x33 <- sum(EV_Stations_NC$Facility_Type == "STANDALONE_STATION")
    print(paste("Standalone: ", x33))
  }else if (input$Facility_Type_NC == "STREET_PARKING"){
    x34 <- sum(EV_Stations_NC$Facility_Type == "STREET_PARKING")
    print(paste("Street Parking: ", x34))
  }else if (input$Facility_Type_NC == "TRAVEL_CENTER"){
    x35 <- sum(EV_Stations_NC$Facility_Type == "TRAVEL_CENTER")
    print(paste("Travel Centers: ", x35))
  }else if (input$Facility_Type_NC == "UTILITY"){
    x36 <- sum(EV_Stations_NC$Facility_Type == "UTILITY")
    print(paste("Utility Centers: ", x36))
  }else print("Error, try again.")
})

#Based on user input, calculates the sum of each type of charging facility for only South Carolina

output$Facility_SC <- renderText({
  if(input$Facility_Type_SC == "AIRPORT"){
    x1 <- sum(EV_Stations_SC$Facility_Type == "AIRPORT")
    print(paste("Airports: ", x1))
  }else if (input$Facility_Type_SC == "AUTO_REPAIR"){
    x2 <- sum(EV_Stations_SC$Facility_Type == "AUTO_REPAIR")
    print(paste("Auto Repair Shops: ", x2))
  }else if (input$Facility_Type_SC == "B_AND_B"){
    x3 <- sum(EV_Stations_SC$Facility_Type == "B_AND_B")
    print(paste("Bed and Breakfasts: ", x3))
  }else if (input$Facility_Type_SC == "BANK"){
    x4 <- sum(EV_Stations_SC$Facility_Type == "BANK")
    print(paste("Banks: ", x4))
  }else if (input$Facility_Type_SC == "BREWERY_DISTILLERY_WINERY"){
    x5 <- sum(EV_Stations_SC$Facility_Type == "BREWERY_DISTILLERY_WINERY")
    print(paste("Breweries: ", x5))
  }else if (input$Facility_Type_SC == "CAMPGROUND"){
    x6 <- sum(EV_Stations_SC$Facility_Type == "CAMPGROUND")
    print(paste("Campgrounds: ", x6))
  }else if (input$Facility_Type_SC == "CAR_DEALER"){
    x7 <- sum(EV_Stations_SC$Facility_Type == "CAR_DEALER")
    print(paste("Car Dealerships: ", x7))
  }else if (input$Facility_Type_SC == "COLLEGE_CAMPUS"){
    x8 <- sum(EV_Stations_SC$Facility_Type == "COLLEGE_CAMPUS")
    print(paste("College Campuses: ", x8))
  }else if (input$Facility_Type_SC == "CONVENIENCE_STORE"){
    x9 <- sum(EV_Stations_SC$Facility_Type == "CONVENIENCE_STORE")
    print(paste("Convenience Stores: ", x9))
  }else if (input$Facility_Type_SC == "CONVENTION_CENTER"){
    x10 <- sum(EV_Stations_SC$Facility_Type == "CONVENTION_CENTER")
    print(paste("Convention Centers: ", x10))
  }else if (input$Facility_Type_SC == "COOP"){
    x11 <- sum(EV_Stations_SC$Facility_Type == "COOP")
    print(paste("Cooperative: ", x11))
  }else if (input$Facility_Type_SC == "GAS_STATION"){
    x12 <- sum(EV_Stations_SC$Facility_Type == "GAS_STATION")
    print(paste("Gas Stations: ", x12))
  }else if (input$Facility_Type_SC == "GROCERY"){
    x13 <- sum(EV_Stations_SC$Facility_Type == "GROCERY")
    print(paste("Grocery Stores: ", x13))
  }else if (input$Facility_Type_SC == "HOSPITAL"){
    x14 <- sum(EV_Stations_SC$Facility_Type == "HOSPITALS")
    print(paste("Hospitals: ", x14))
  }else if (input$Facility_Type_SC == "HOTEL"){
    x15 <- sum(EV_Stations_SC$Facility_Type == "HOTEL")
    print(paste("Hotel: ", x15))
  }else if (input$Facility_Type_SC == "INN"){
    x16 <- sum(EV_Stations_SC$Facility_Type == "INN")
    print(paste("Inns: ", x16))
  }else if (input$Facility_Type_SC == "LIBRARY"){
    x17 <- sum(EV_Stations_SC$Facility_Type == "LIBRARY")
    print(paste("Libraries: ", x17))
  }else if (input$Facility_Type_SC == "MUNI_GOV"){
    x18 <- sum(EV_Stations_SC$Facility_Type == "MUNI_GOV")
    print(paste("Municipal Government Buildings: ", x18))
  }else if (input$Facility_Type_SC == "MUSEUM"){
    x19 <- sum(EV_Stations_SC$Facility_Type == "MUSEUM")
    print(paste("Museums: ", x19))
  }else if (input$Facility_Type_SC == "OFFICE_BLDG"){
    x20 <- sum(EV_Stations_SC$Facility_Type == "OFFICE_BLDG")
    print(paste("Office Buildings: ", x20))
  }else if (input$Facility_Type_SC == "OTHER"){
    x21 <- sum(EV_Stations_SC$Facility_Type == "OTHER")
    print(paste("Other Facilities: ", x21))
  }else if (input$Facility_Type_SC == "OTHER_ENTERTAINMENT"){
    x22 <- sum(EV_Stations_SC$Facility_Type == "OTHER_ENTERTAINMENT")
    print(paste("Other Entertainment Venues: ", x22))
  }else if (input$Facility_Type_SC == "PARK" ){
    x23 <- sum(EV_Stations_SC$Facility_Type == "PARK")
    print(paste("Parks: ", x23))
  }else if (input$Facility_Type_SC == "PARKING_GARAGE"){
    x24 <- sum(EV_Stations_SC$Facility_Type == "PARKING_GARAGE")
    print(paste("Parking Garages: ", x24))
  }else if (input$Facility_Type_SC == "PARKING_LOT"){
    x25 <- sum(EV_Stations_SC$Facility_Type == "PARKING_LOT")
    print(paste("Parking Lots: ", x25))
  }else if (input$Facility_Type_SC == "PAY_GARAGE"){
    x26 <- sum(EV_Stations_SC$Facility_Type == "PAY_GARAGE")
    print(paste("Paid Parking Garages: ", x26))
  }else if (input$Facility_Type_SC == "PAY_LOT"){
    x27 <- sum(EV_Stations_SC$Facility_Type == "PAY_LOT")
    print(paste("Paid Parking Lots: ", x27))
  }else if (input$Facility_Type_SC == "REC_SPORTS_FACILITY"){
    x28 <- sum(EV_Stations_SC$Facility_Type == "REC_SPORTS_FACILITY")
    print(paste("Sports Facilities: ", x28))
  }else if (input$Facility_Type_SC == "RESEARCH_FACILITY"){
    x29 <- sum(EV_Stations_SC$Facility_Type == "RESEARCH_FACILITY")
    print(paste("Research Facilities: ", x29))
  }else if (input$Facility_Type_SC == "RESTAURANT"){
    x30 <- sum(EV_Stations_SC$Facility_Type == "RESTAURANT")
    print(paste("Restaurants: ", x30))
  }else if (input$Facility_Type_SC == "RV_PARK"){
    x31 <- sum(EV_Stations_SC$Facility_Type == "RV_PARK")
    print(paste("RV Parks: ", x31))
  }else if (input$Facility_Type_SC == "SHOPPING_CENTER"){
    x32 <- sum(EV_Stations_SC$Facility_Type == "SHOPPING_CENTER")
    print(paste("Shopping Centers: ", x32))
  }else if (input$Facility_Type_SC == "STANDALONE_STATION"){
    x33 <- sum(EV_Stations_SC$Facility_Type == "STANDALONE_STATION")
    print(paste("Standalone: ", x33))
  }else if (input$Facility_Type_SC == "STREET_PARKING"){
    x34 <- sum(EV_Stations_SC$Facility_Type == "STREET_PARKING")
    print(paste("Street Parking: ", x34))
  }else if (input$Facility_Type_SC == "TRAVEL_CENTER"){
    x35 <- sum(EV_Stations_SC$Facility_Type == "TRAVEL_CENTER")
    print(paste("Travel Centers: ", x35))
  }else if (input$Facility_Type_SC == "UTILITY"){
    x36 <- sum(EV_Stations_SC$Facility_Type == "UTILITY")
    print(paste("Utility Centers: ", x36))
  }else print("Error, try again.")
})

#GGPLOT stacked barplot of Charging Networks in both Carolinas, compared side by side, nested in plotly.

output$Network <- renderPlotly({
  ggplotly(
  ggplot(EV_Stations, aes(factor(State), fill = factor(EV_Network))) +
  geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent)  + 
  theme_bw() +  theme(plot.background = element_blank(), legend.title =  element_blank() ) +
  xlab("State") + ylab("Percent")
  )
})

#GGPLOT lollipop chart of Charging Networks in only North Carolina nested in plotly.

output$Network_NC <- renderPlotly({
  ggplot(EV_Net_NC, tooltip="EV_Network", aes(factor(EV_Network), n)) +
    geom_point(color="steelblue", size = 3.5, stat = "identity") + 
    geom_segment( aes(x=factor(EV_Network), xend=factor(EV_Network), y=0, yend=n), color="blue")+
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) + 
    xlab("Count")+
    ylab("EV Network")
})

#GGPLOT lollipop chart of Charging Networks in only South Carolina nested in plotly.

output$Network_SC <- renderPlotly({
  ggplot(EV_Net_SC, aes(factor(EV_Network), n)) +
    geom_point(color="steelblue", size = 3.5, stat = "identity") + 
    geom_segment( aes(x=factor(EV_Network), xend=factor(EV_Network), y=0, yend=n), color="blue")+
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) + 
    xlab("Count")+
    ylab("EV Network")
})

#Regional Bar Plots

output$PCO_Triad <- renderPlotly({
  ggplot(Triad, aes(x=factor(NAME), y= PCO10000, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("Charging Outlets per 10,000 ppl") + 
    xlab("Regional Counties")
})

output$Outlets_Triad <- renderPlotly({
  ggplot(Triad, aes(x=factor(NAME), y= SUM_Chargi, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("# of Charging Outlets") + 
    xlab("Regional Counties")
  })

output$PCO_Triangle <- renderPlotly({
  ggplot(Triangle, aes(x=factor(NAME), y= PCO10000, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("Charging Outlets per 10,000 ppl") + 
    xlab("Regional Counties")
})

output$Outlets_Triangle <- renderPlotly({
  ggplot(Triangle, aes(x=factor(NAME), y= SUM_Chargi, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("# of Charging Outlets") + 
    xlab("Regional Counties")
})

output$PCO_Charlotte <- renderPlotly({
  ggplot(Charlotte, aes(x=factor(NAME), y= PCO10000, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("Charging Outlets per 10,000 ppl") + 
    xlab("Regional Counties")
})

output$Outlets_Charlotte <- renderPlotly({
  ggplot(Charlotte, aes(x=factor(NAME), y= SUM_Chargi, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("# of Charging Outlets") + 
    xlab("Regional Counties")
})

output$PCO_Charleston <- renderPlotly({
  ggplot(Charleston, aes(x=factor(NAME), y= PCO10000, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("Charging Outlets per 10,000 ppl") + 
    xlab("Regional Counties")
})

output$Outlets_Charleston <- renderPlotly({
  ggplot(Charleston, aes(x=factor(NAME), y= SUM_Chargi, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("# of Charging Outlets") + 
    xlab("Regional Counties")
})

output$PCO_Columbia <- renderPlotly({
  ggplot(Columbia, aes(x=factor(NAME), y= PCO10000, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("Charging Outlets per 10,000 ppl") + 
    xlab("Regional Counties")
})

output$Outlets_Columbia <- renderPlotly({
  ggplot(Columbia, aes(x=factor(NAME), y= SUM_Chargi, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("# of Charging Outlets") + 
    xlab("Regional Counties")
})

output$PCO_Greenville <- renderPlotly({
  ggplot(Greenville, aes(x=factor(NAME), y= PCO10000, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("Charging Outlets per 10,000 ppl") + 
    xlab("Regional Counties")
})

output$Outlets_Greenville <- renderPlotly({
  ggplot(Greenville, aes(x=factor(NAME), y= SUM_Chargi, fill=factor(NAME)))+
    geom_bar(stat = "identity", width = 0.8)+
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold", angle = 50),
      axis.ticks.x=element_blank(),
      legend.position = "none") +
    scale_fill_discrete(name = "Counties") +
    ylab("# of Charging Outlets") + 
    xlab("Regional Counties")
})

}

#Server Call

shinyApp(ui, server)

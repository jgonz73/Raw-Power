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
library(sf)
library(readxl)
library(leaflet)
library(ggplot2)
library(geojsonio)

df18 <- read_excel("egrid2018_data_v2.xlsx", sheet="PLNT18")

df18 <- subset(df18, select = c("Data Year", "Plant name", "Plant state abbreviation", "Plant latitude", 
                                "Plant longitude", "Plant annual coal net generation (MWh)", 
                                "Plant annual oil net generation (MWh)",
                                "Plant annual gas net generation (MWh)",
                                "Plant annual nuclear net generation (MWh)",
                                "Plant annual hydro net generation (MWh)",
                                "Plant annual biomass net generation (MWh)",
                                "Plant annual wind net generation (MWh)",
                                "Plant annual solar net generation (MWh)",
                                "Plant annual geothermal net generation (MWh)",
                                "Plant annual other fossil net generation (MWh)",
                                "Plant annual other unknown/ purchased fuel net generation (MWh)"))


# Renames columns
df18 <- df18 %>%
  rename(
    "YEAR" = "Data Year",
    "NAME" = "Plant name",
    "STATE" = "Plant state abbreviation",
    "LAT" = "Plant latitude",
    "LON" = "Plant longitude",
    "COAL" = "Plant annual coal net generation (MWh)",
    "OIL" = "Plant annual oil net generation (MWh)",
    "GAS" = "Plant annual gas net generation (MWh)",
    "NUCLEAR" = "Plant annual nuclear net generation (MWh)",
    "HYDRO" = "Plant annual hydro net generation (MWh)",
    "BIOMASS" = "Plant annual biomass net generation (MWh)",
    "WIND" = "Plant annual wind net generation (MWh)",
    "SOLAR" = "Plant annual solar net generation (MWh)",
    "GEOTHERMAL" = "Plant annual geothermal net generation (MWh)",
    "OTHER1" = "Plant annual other fossil net generation (MWh)",
    "OTHER2" = "Plant annual other unknown/ purchased fuel net generation (MWh)"
  )

df18 <- df18[-c(1),]
df18$NAME = toupper(df18$NAME)

# convert generation to numbers from strings
df18$LAT <- as.numeric(gsub(",", "", df18$LAT))
df18$LON <- as.numeric(gsub(",", "", df18$LON))
df18$COAL <- as.numeric(gsub(",", "", df18$COAL))
df18$OIL <- as.numeric(gsub(",", "", df18$OIL))
df18$GAS <- as.numeric(gsub(",", "", df18$GAS))
df18$NUCLEAR <- as.numeric(gsub(",", "", df18$NUCLEAR))
df18$HYDRO <- as.numeric(gsub(",", "", df18$HYDRO))
df18$BIOMASS <- as.numeric(gsub(",", "", df18$BIOMASS))
df18$WIND <- as.numeric(gsub(",", "", df18$WIND))
df18$SOLAR <- as.numeric(gsub(",", "", df18$SOLAR))
df18$GEOTHERMAL <- as.numeric(gsub(",", "", df18$GEOTHERMAL))
df18$OTHER1 <- as.numeric(gsub(",", "", df18$OTHER1))
df18$OTHER2 <- as.numeric(gsub(",", "", df18$OTHER2))

# Combine last 2 generations to one column: OTHER
df18$OTHER <- df18$OTHER1 + df18$OTHER2
df18 <- df18[ , !names(df18) %in% c("OTHER1", "OTHER2")]

# Removes locations that don't have coordinates
df18 <- subset(df18, !is.na(df18$LON))

df18$STATE <- factor(df18$STATE)

# need to compute
# 1.) Total generation
df18$TOTAL <- df18$COAL + df18$OIL + df18$GAS + df18$NUCLEAR + df18$HYDRO + df18$BIOMASS + df18$WIND + df18$WIND + df18$SOLAR + df18$OTHER

# 2.) % of total for each of 10 types -> reactive
# 3.) total renewable (HYDRO, BIOMASS, WIND, SOLAR, GEOTHERMAL) 
df18$TOTAL_R <- df18$HYDRO + df18$BIOMASS  + df18$WIND + df18$SOLAR + df18$GEOTHERMAL

# 4.) total non-renewable (COAL, OIL, GAS, NUCLEAR, OTHER)
df18$TOTAL_NR <- df18$COAL + df18$OIL + df18$GAS + df18$NUCLEAR + df18$OTHER

# 5.) % of total that is renewable
df18$PERCENT_R <- round((df18$TOTAL_R / df18$TOTAL), 3) * 100
# df18$PERCENT_R <- ifelse(!d18$TOTAL, 0, round((df18$TOTAL_R / df18$TOTAL), 3) * 100)

# 6.) % of total that is non-renewable
df18$PERCENT_NR <- round((df18$TOTAL_NR / df18$TOTAL), 3) * 100
# df18$PERCENT_NR <- ifelse(!d18$TOTAL, 0, round((df18$TOTAL_NR / df18$TOTAL), 3) * 100)

#=====================================================================================================================================
df10 <- read_excel("eGRID2010_Data.xls", sheet = "PLNT10")

df10 <- subset(df10, select = c("Plant name", "Plant state abbreviation", "Plant latitude", 
                                "Plant longitude", "Plant annual coal net generation (MWh)", 
                                "Plant annual oil net generation (MWh)",
                                "Plant annual gas net generation (MWh)",
                                "Plant annual nuclear net generation (MWh)",
                                "Plant annual hydro net generation (MWh)",
                                "Plant annual biomass net generation (MWh)",
                                "Plant annual wind net generation (MWh)",
                                "Plant annual solar net generation (MWh)",
                                "Plant annual geothermal net generation (MWh)",
                                "Plant annual other fossil net generation (MWh)",
                                "Plant annual other unknown/ purchased fuel net generation (MWh)",
                                "Plant annual total nonrenewables net generation (MWh)",
                                "Plant annual total renewables net generation (MWh)"))

# Renames columns
df10 <- df10 %>%
  rename(
    "NAME" = "Plant name",
    "STATE" = "Plant state abbreviation",
    "LAT" = "Plant latitude",
    "LON" = "Plant longitude",
    "COAL" = "Plant annual coal net generation (MWh)",
    "OIL" = "Plant annual oil net generation (MWh)",
    "GAS" = "Plant annual gas net generation (MWh)",
    "NUCLEAR" = "Plant annual nuclear net generation (MWh)",
    "HYDRO" = "Plant annual hydro net generation (MWh)",
    "BIOMASS" = "Plant annual biomass net generation (MWh)",
    "WIND" = "Plant annual wind net generation (MWh)",
    "SOLAR" = "Plant annual solar net generation (MWh)",
    "GEOTHERMAL" = "Plant annual geothermal net generation (MWh)",
    "OTHER1" = "Plant annual other fossil net generation (MWh)",
    "OTHER2" = "Plant annual other unknown/ purchased fuel net generation (MWh)",
    "TOTAL_R" = "Plant annual total nonrenewables net generation (MWh)",
    "TOTAL_NR" = "Plant annual total renewables net generation (MWh)"
  )

df10$NAME = toupper(df10$NAME)

# convert generation to numbers from strings
df10$LAT <- as.numeric(gsub(",", "", df10$LAT))
df10$LON <- as.numeric(gsub(",", "", df10$LON))
df10$COAL <- as.numeric(gsub(",", "", df10$COAL))
df10$OIL <- as.numeric(gsub(",", "", df10$OIL))
df10$GAS <- as.numeric(gsub(",", "", df10$GAS))
df10$NUCLEAR <- as.numeric(gsub(",", "", df10$NUCLEAR))
df10$HYDRO <- as.numeric(gsub(",", "", df10$HYDRO))
df10$BIOMASS <- as.numeric(gsub(",", "", df10$BIOMASS))
df10$WIND <- as.numeric(gsub(",", "", df10$WIND))
df10$SOLAR <- as.numeric(gsub(",", "", df10$SOLAR))
df10$GEOTHERMAL <- as.numeric(gsub(",", "", df10$GEOTHERMAL))
df10$OTHER1 <- as.numeric(gsub(",", "", df10$OTHER1))
df10$OTHER2 <- as.numeric(gsub(",", "", df10$OTHER2))
df10$TOTAL_R <- as.numeric(gsub(",", "", df10$TOTAL_R))
df10$TOTAL_NR <- as.numeric(gsub(",", "", df10$TOTAL_NR))


# combine last 2 generations to one column: OTHER
df10$OTHER <- df10$OTHER1 + df10$OTHER2
df10 <- df10[ , !names(df10) %in% c("OTHER1", "OTHER2")]

df10$STATE <- factor(df10$STATE)

# need to compute
# 1.) Total generation
df10$TOTAL <- df10$COAL + df10$OIL + df10$GAS + df10$NUCLEAR + df10$HYDRO + df10$BIOMASS + df10$WIND + df10$WIND + df10$SOLAR + df10$OTHER

# 2.) % of total for each of 10 types -> reactive
# 3.) total renewable (HYDRO, BIOMASS, WIND, SOLAR, GEOTHERMAL) -> Already in df
# 4.) total non-renewable (COAL, OIL, GAS, NUCLEAR, OTHER) -> Already in df

# 5.) % of total that is renewable
df10$PERCENT_R <- round((df10$TOTAL_R / df10$TOTAL), 3) * 100
# df18$PERCENT_R <- ifelse(!d18$TOTAL, 0, round((df18$TOTAL_R / df18$TOTAL), 3) * 100)

# 6.) % of total that is non-renewable
df10$PERCENT_NR <- round((df10$TOTAL_NR / df10$TOTAL), 3) * 100
# df18$PERCENT_NR <- ifelse(!d18$TOTAL, 0, round((df18$TOTAL_NR / df18$TOTAL), 3) * 100)

#=====================================================================================================================================
df00 <- read_excel("eGRID2000_plant.xls", sheet = "EGRDPLNT00")

df00 <- subset(df00, select = c("Plant Name", "Plant state abbreviation", "Plant latitude", 
                                "Plant longitude", "Plant annual coal net generation (MWh)", 
                                "Plant annual oil net generation (MWh)",
                                "Plant annual gas net generation (MWh)",
                                "Plant annual nuclear net generation (MWh)",
                                "Plant annual hydro net generation (MWh)",
                                "Plant annual biomass/wood net generation (MWh)",
                                "Plant annual wind net generation (MWh)",
                                "Plant annual solar net generation (MWh)",
                                "Plant annual geothermal net generation (MWh)",
                                "Plant annual other fossil net generation (MWh)",
                                "Plant annual solid waste net generation (MWh)",
                                "Plant annual total nonrenewables net generation (MWh)",
                                "Plant annual total renewables net generation (MWh)"))

# Renames columns
df00 <- df00 %>%
  rename(
    "NAME" = "Plant Name",
    "STATE" = "Plant state abbreviation",
    "LAT" = "Plant latitude",
    "LON" = "Plant longitude",
    "COAL" = "Plant annual coal net generation (MWh)",
    "OIL" = "Plant annual oil net generation (MWh)",
    "GAS" = "Plant annual gas net generation (MWh)",
    "NUCLEAR" = "Plant annual nuclear net generation (MWh)",
    "HYDRO" = "Plant annual hydro net generation (MWh)",
    "BIOMASS" = "Plant annual biomass/wood net generation (MWh)",
    "WIND" = "Plant annual wind net generation (MWh)",
    "SOLAR" = "Plant annual solar net generation (MWh)",
    "GEOTHERMAL" = "Plant annual geothermal net generation (MWh)",
    "OTHER1" = "Plant annual other fossil net generation (MWh)",
    "OTHER2" = "Plant annual solid waste net generation (MWh)",
    "TOTAL_R" = "Plant annual total nonrenewables net generation (MWh)",
    "TOTAL_NR" = "Plant annual total renewables net generation (MWh)"
  )

# convert generation to numbers from strings
df00$LAT <- as.numeric(gsub(",", "", df00$LAT))
df00$LON <- as.numeric(gsub(",", "", df00$LON))
df00$COAL <- as.numeric(gsub(",", "", df00$COAL))
df00$OIL <- as.numeric(gsub(",", "", df00$OIL))
df00$GAS <- as.numeric(gsub(",", "", df00$GAS))
df00$NUCLEAR <- as.numeric(gsub(",", "", df00$NUCLEAR))
df00$HYDRO <- as.numeric(gsub(",", "", df00$HYDRO))
df00$BIOMASS <- as.numeric(gsub(",", "", df00$BIOMASS))
df00$WIND <- as.numeric(gsub(",", "", df00$WIND))
df00$SOLAR <- as.numeric(gsub(",", "", df00$SOLAR))
df00$GEOTHERMAL <- as.numeric(gsub(",", "", df00$GEOTHERMAL))
df00$OTHER1 <- as.numeric(gsub(",", "", df00$OTHER1))
df00$OTHER2 <- as.numeric(gsub(",", "", df00$OTHER2))
df00$TOTAL_R <- as.numeric(gsub(",", "", df00$TOTAL_R))
df00$TOTAL_NR <- as.numeric(gsub(",", "", df00$TOTAL_NR))

# combine last 2 generations to one column: OTHER
df00$OTHER <- df00$OTHER1 + df00$OTHER2
df00 <- df00[ , !names(df00) %in% c("OTHER1", "OTHER2")]

df00$STATE <- factor(df00$STATE)

# List of sources to display and select from in checklist
sources <- c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar",
             "Geothermal", "Other", "Renewables", "Non-renewables", "All")

# need to compute
# 1.) Total generation
df00$TOTAL <- df00$COAL + df00$OIL + df00$GAS + df00$NUCLEAR + df00$HYDRO + df00$BIOMASS + df00$WIND + df00$WIND + df00$SOLAR + df00$OTHER

# 2.) % of total for each of 10 types -> reactive
# 3.) total renewable (HYDRO, BIOMASS, WIND, SOLAR, GEOTHERMAL) -> Already in df
# 4.) total non-renewable (COAL, OIL, GAS, NUCLEAR, OTHER) -> Already in df

# 5.) % of total that is renewable
df00$PERCENT_R <- round((df00$TOTAL_R / df00$TOTAL), 3) * 100
# df10$PERCENT_R <- ifelse(!d10$TOTAL, 0, round((df10$TOTAL_R / df10$TOTAL), 3) * 100)

# 6.) % of total that is non-renewable
df00$PERCENT_NR <- round((df00$TOTAL_NR / df00$TOTAL), 3) * 100
# df10$PERCENT_NR <- ifelse(!d10$TOTAL, 0, round((df10$TOTAL_NR / df10$TOTAL), 3) * 100)

#=====================================================================================================================================

power_plants <- read_sf("PowerPlants_US_202004.shp")
df18 %>% st_as_sf(coords = c("LON", "LAT"), crs=4326)

df18IL <- subset(df18, df18$STATE == 'IL')

#=====================================================================================================================================

# Define UI for application that draws a histogram
ui <- navbarPage("CS424 Spring 2021 Project 2",
                 tabPanel("Illinois 2018 Data",
                          
                          # Checklist to filter sources
                          fluidRow(
                            #column(12,
                            leafletOutput("test")
                            #)
                          ),
                          fluidRow(
                            column(3),
                            column(6,
                                   checkboxGroupInput("icons", "Select energy sources to display:",
                                                      choiceNames = sources,
                                                      choiceValues = sources,
                                                      selected = 'All',
                                                      inline=TRUE),
                            ),
                            column(3)
                          )
                 ),
                 tabPanel("About"
                          
                 )
                 
)

#=====================================================================================================================================
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  all18 <- reactive({
    df <- NULL
    if ("All" %in% input$icons) {
      df <- df18IL
    }
    return (df)
  }) 
  
  #renew18 <- reactive({
  #  df <- NULL
  #  if ("Renewables" %in% input$icons) {
  #    df <- subset(df18IL, df18IL$TOTAL_R > 0)
  #  }
  #  return (df)
  #})
  
  #nonrenew18 <- reactive({
  #  df <- NULL
  #  if ("Non-renewables" %in% input$icons) {
  #    df <- subset(df18IL, df18IL$TOTAL_NR > 0)
  #  }
  #  return (df)
  #})
  
  # Get reactive data from selected items in checklist based on category and source
  coal18 <- reactive({
    coaldf <- NULL
    if ("Coal" %in% input$icons || "All" %in% input$icons || "Non-renewables" %in% input$icons) {
      coaldf <- subset(df18IL, df18IL$COAL > 0)
    } 
    
    return(coaldf)
  })
  
  oil18 <- reactive({
    oildf <- NULL
    if ("Oil" %in% input$icons || "All" %in% input$icons || "Non-renewables" %in% input$icons) {
      oildf <- subset(df18IL, df18IL$OIL > 0) 
    }
    return (oildf)
  })
  
  gas18 <- reactive({
    gasdf <- NULL
    if ("Gas" %in% input$icons || "All" %in% input$icons || "Non-renewables" %in% input$icons) {
      gasdf <- subset(df18IL, df18IL$GAS > 0) 
    }
    return (gasdf)
  })
  
  nuclear18 <- reactive({
    nucleardf <- NULL
    if ("Nuclear" %in% input$icons || "All" %in% input$icons || "Non-renewables" %in% input$icons) {
      nucleardf <- subset(df18IL, df18IL$NUCLEAR > 0) 
    }
    return (nucleardf)
  })
  
  hydro18 <- reactive({
    hydrodf <- NULL
    if ("Hydro" %in% input$icons || "All" %in% input$icons || "Renewables" %in% input$icons) {
      hydrodf <- subset(df18IL, df18IL$HYDRO > 0) 
    }
    return (hydrodf)
  })
  
  biomass18 <- reactive({
    biomassdf <- NULL
    if ("Biomass" %in% input$icons || "All" %in% input$icons || "Renewables" %in% input$icons) {
      biomassdf <- subset(df18IL, df18IL$BIOMASS > 0) 
    }
    return (biomassdf)
  })
  
  wind18 <- reactive({
    winddf <- NULL
    if ("Wind" %in% input$icons || "All" %in% input$icons || "Renewables" %in% input$icons) {
      winddf <- subset(df18IL, df18IL$WIND > 0) 
    }
    return (winddf)
  })
  
  solar18 <- reactive({
    solardf <- NULL
    if ("Solar" %in% input$icons || "All" %in% input$icons || "Renewables" %in% input$icons) {
      solardf <- subset(df18IL, df18IL$SOLAR > 0) 
    }
    return (solardf)
  })
  
  geothermal18 <- reactive({
    geothermaldf <- NULL
    if ("Geothermal" %in% input$icons || "All" %in% input$icons || "Renewables" %in% input$icons) {
      geothermaldf <- subset(df18IL, df18IL$GEOTHERMAL > 0) 
    }
    return (geothermaldf)
  })
  
  other18 <- reactive({
    otherdf <- NULL
    if ("Other" %in% input$icons || "All" %in% input$icons || "Non-renewables" %in% input$icons) {
      otherdf <- subset(df18IL, df18IL$OTHER > 0) 
    }
    return (otherdf)
  })
  
#=====================================================================================================================================
  
  types <- c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar",
             "Geothermal", "Other")
  
  pal <- colorFactor(palette = c("black", "grey40", "midnightblue", "chartreuse4", "deepskyblue", "red2", "lightblue",
                                 "gold", "brown", "burlywood"), 
                     levels = types)
  
  # Static initial leaflet, has observer for checkbox presses
  output$test <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=39.8331, lng=-88.8985, zoom=6) %>%
      addLegend("bottomright", pal=pal, values=types, title="Energy Sources", opacity=1) %>%
      addControl(actionButton("zoomer", "Reset"), position="topright")
  })
  
  observe({
    
    # Reset the map first to account for deselecting of checkboxes?
    leafletProxy(mapId="test", data=df18IL) %>%
      clearMarkers()
    
    # check if All (default) is checked MIGHT NOT NEED THIS ****
    alldf <- all18()
    
    
    # if nothing is checked, then just clear the markers
    if (is.null(input$icons)) {
        
    } else {
        # Get necessary data that was selected, Part I of event handling
        coaldf <- coal18()
        oildf <- oil18()
        gasdf <- gas18()
        nucleardf <- nuclear18()
        hydrodf <- hydro18()
        biomassdf <- biomass18()
        winddf <- wind18()
        solardf <- solar18()
        geothermaldf <- geothermal18()
        otherdf <- other18()
        
        # Try adding circle markers for each source one by one independently instead of aggregating
        # add reactive values for each energy source so we can add circle markers one by one
        # Part II of event handling
      
        if (!is.null(coaldf)) {
          leafletProxy(mapId="test", data=coaldf) %>% # clearMarkers() %>%
          addCircleMarkers(data=coaldf, lng=coaldf$LON, lat=coaldf$LAT, radius=7,
                           color='black', stroke=FALSE, fillOpacity=1, label=paste("Source= Coal")) 
        } 
        
        if (!is.null(oildf)) {
          leafletProxy(mapId="test", data=oildf) %>% # clearMarkers() %>%
            addCircleMarkers(data=oildf, lng=oildf$LON, lat=oildf$LAT, radius=7,
                             color='grey', stroke=FALSE, fillOpacity=1, label=paste("Source= Oil")) 
        }
        
        if (!is.null(gasdf)) {
          leafletProxy(mapId="test", data=gasdf) %>% # clearMarkers() %>%
            addCircleMarkers(data=gasdf, lng=gasdf$LON, lat=gasdf$LAT, radius=7,
                             color='midnightblue', stroke=FALSE, fillOpacity=1, label=paste("Source= Gas")) 
        }
        
        if (!is.null(nucleardf)) {
          leafletProxy(mapId="test", data=nucleardf) %>% # clearMarkers() %>%
            addCircleMarkers(data=nucleardf, lng=nucleardf$LON, lat=nucleardf$LAT, radius=7,
                             color='green', stroke=FALSE, fillOpacity=1, label=paste("Source= Nuclear")) 
        }
        
        if (!is.null(hydrodf)) {
          leafletProxy(mapId="test", data=hydrodf) %>% # clearMarkers() %>%
            addCircleMarkers(data=hydrodf, lng=hydrodf$LON, lat=hydrodf$LAT, radius=7,
                             color='deepskyblue', stroke=FALSE, fillOpacity=1, label=paste("Source= Hydro")) 
        }
        
        if (!is.null(biomassdf)) {
          leafletProxy(mapId="test", data=biomassdf) %>% # clearMarkers() %>%
            addCircleMarkers(data=biomassdf, lng=biomassdf$LON, lat=biomassdf$LAT, radius=7,
                             color='red', stroke=FALSE, fillOpacity=1, label=paste("Source= Biomass")) 
        }
        
        if (!is.null(winddf)) {
          leafletProxy(mapId="test", data=winddf) %>% # clearMarkers() %>%
            addCircleMarkers(data=winddf, lng=winddf$LON, lat=winddf$LAT, radius=7,
                             color='lightblue', stroke=FALSE, fillOpacity=1, label=paste("Source= Wind")) 
        }
        
        if (!is.null(solardf)) {
          leafletProxy(mapId="test", data=solardf) %>% # clearMarkers() %>%
            addCircleMarkers(data=solardf, lng=solardf$LON, lat=solardf$LAT, radius=7,
                             color='gold', stroke=FALSE, fillOpacity=1, label=paste("Source= Solar")) 
        }
        
        if (!is.null(geothermaldf)) {
          leafletProxy(mapId="test", data=geothermaldf) %>% # clearMarkers() %>%
            addCircleMarkers(data=geothermaldf, lng=geothermaldf$LON, lat=geothermaldf$LAT, radius=7,
                             color='brown', stroke=FALSE, fillOpacity=1, label=paste("Source= Geothermal")) 
        }
        
        if (!is.null(otherdf)) {
          leafletProxy(mapId="test", data=otherdf) %>% # clearMarkers() %>%
            addCircleMarkers(data=otherdf, lng=otherdf$LON, lat=otherdf$LAT, radius=7,
                             color='burlywood', stroke=FALSE, fillOpacity=1, label=paste("Source= Other")) 
        }
        
    }
  })
  
  observeEvent(input$zoomer, {
    leafletProxy("test") %>% setView(lat=39.8331, lng=-88.8985, zoom=6)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

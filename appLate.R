#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(readxl)
library(leaflet)
library(ggplot2)
library(geojsonio)
library(maps)

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
df18$TOTAL <- df18$COAL + df18$OIL + df18$GAS + df18$NUCLEAR + df18$HYDRO + df18$BIOMASS + df18$WIND + df18$SOLAR + df18$OTHER

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

df18React <- subset(df18, !is.na(df18$GEOTHERMAL))

#mins18 <- c(min(df18$COAL), min(df18$OIL), min(df18$GAS), min(df18$NUCLEAR), min(df18$HYDRO), min(df18$BIOMASS), min(df18$WIND), min(df18$SOLAR), min(df18$OTHER))
#maxs18 <- c(max(df18$COAL), max(df18$OIL), max(df18$GAS), max(df18$NUCLEAR), max(df18$HYDRO), max(df18$BIOMASS), max(df18$WIND), max(df18$SOLAR), max(df18$OTHER))
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

# Removes locations that don't have coordinates
df10 <- subset(df10, !is.na(df10$LON))

df10$STATE <- factor(df10$STATE)

# need to compute
# 1.) Total generation
df10$TOTAL <- df10$COAL + df10$OIL + df10$GAS + df10$NUCLEAR + df10$HYDRO + df10$BIOMASS + df10$WIND + df10$SOLAR + df10$OTHER

# 2.) % of total for each of 10 types -> reactive
# 3.) total renewable (HYDRO, BIOMASS, WIND, SOLAR, GEOTHERMAL) -> Already in df
# 4.) total non-renewable (COAL, OIL, GAS, NUCLEAR, OTHER) -> Already in df

# 5.) % of total that is renewable
df10$PERCENT_R <- round((df10$TOTAL_R / df10$TOTAL), 3) * 100
# df18$PERCENT_R <- ifelse(!d18$TOTAL, 0, round((df18$TOTAL_R / df18$TOTAL), 3) * 100)

# 6.) % of total that is non-renewable
df10$PERCENT_NR <- round((df10$TOTAL_NR / df10$TOTAL), 3) * 100
# df18$PERCENT_NR <- ifelse(!d18$TOTAL, 0, round((df18$TOTAL_NR / df18$TOTAL), 3) * 100)

df10React <- subset(df10, !is.na(df10$GEOTHERMAL))
#mins10 <- c(min(df10$COAL), min(df10$OIL), min(df10$GAS), min(df10$NUCLEAR), min(df10$HYDRO), min(df10$BIOMASS), min(df10$WIND), min(df10$SOLAR), min(df10$OTHER))
#maxs10 <- c(max(df10$COAL), max(df10$OIL), max(df10$GAS), max(df10$NUCLEAR), max(df10$HYDRO), max(df10$BIOMASS), max(df10$WIND), max(df10$SOLAR), max(df10$OTHER))

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

# Removes locations that don't have coordinates
df00 <- subset(df00, !is.na(df00$LON))

# In 2000's dataset, longitude columns don't have '-', and since all these coordinates refer to US, add a '-'
df00$LON <- -df00$LON

df00$STATE <- factor(df00$STATE)

# List of sources to display and select from in checklist
sources <- c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar",
             "Geothermal", "Other", "Renewables", "Non-renewables", "All")

# need to compute
# 1.) Total generation
df00$TOTAL <- df00$COAL + df00$OIL + df00$GAS + df00$NUCLEAR + df00$HYDRO + df00$BIOMASS + df00$WIND + df00$SOLAR + df00$OTHER

# 2.) % of total for each of 10 types -> reactive
# 3.) total renewable (HYDRO, BIOMASS, WIND, SOLAR, GEOTHERMAL) -> Already in df
# 4.) total non-renewable (COAL, OIL, GAS, NUCLEAR, OTHER) -> Already in df

# 5.) % of total that is renewable
df00$PERCENT_R <- round((df00$TOTAL_R / df00$TOTAL), 3) * 100
# df10$PERCENT_R <- ifelse(!d10$TOTAL, 0, round((df10$TOTAL_R / df10$TOTAL), 3) * 100)

# 6.) % of total that is non-renewable
df00$PERCENT_NR <- round((df00$TOTAL_NR / df00$TOTAL), 3) * 100
# df10$PERCENT_NR <- ifelse(!d10$TOTAL, 0, round((df10$TOTAL_NR / df10$TOTAL), 3) * 100)

df00React <- subset(df00, !is.na(df00$GEOTHERMAL))
#mins00 <- c(min(df00$COAL), min(df00$OIL), min(df00$GAS), min(df00$NUCLEAR), min(df00$HYDRO), min(df00$BIOMASS), min(df00$WIND), min(df00$SOLAR), min(df00$OTHER))
#maxs00 <- c(max(df00$COAL), max(df00$OIL), max(df00$GAS), max(df00$NUCLEAR), max(df00$HYDRO), max(df00$BIOMASS), max(df00$WIND), max(df00$SOLAR), max(df00$OTHER))

#=====================================================================================================================================

power_plants <- read_sf("PowerPlants_US_202004.shp")
df18 %>% st_as_sf(coords = c("LON", "LAT"), crs=4326)
df10 %>% st_as_sf(coords = c("LON", "LAT"), crs=4326)
df00 %>% st_as_sf(coords = c("LON", "LAT"), crs=4326)

df18IL <- subset(df18, df18$STATE == 'IL')
df10IL <- subset(df10, df10$STATE == 'IL')
df00IL <- subset(df00, df00$STATE == 'IL')

# States
state <- state.name
state <- sort(state)
statedf <- read.csv("statelatlong.csv")

# Years
years <- c(2000, 2010, 2018)

leaflets <- c("Leaflet 1", "Leaflet 2", "Leaflet 3")

#=====================================================================================================================================

# Define UI for application that draws a histogram
ui <- navbarPage("CS424 Spring 2021 Project 2",
                 tabPanel("Illinois 2018 Data",
                          fluidRow(
                            leafletOutput("test")
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
                 tabPanel("Compare 2 States Over 3 Different Years",
                          fluidRow(
                            column(6,
                                   fluidRow( 
                                     box(title = "Leaflet 1", status="primary", width=12, 
                                         leafletOutput("leftleaflet"),
                                         selectInput("leafletL", "Change leaflet type", leaflets, selected = "Leaflet 1")
                                     )
                                   ),
                                   fluidRow(
                                     box(status="primary", width=12, solidHeader=TRUE,
                                         selectInput("stateLeft", "Select the state to visualize", state, selected = "Illinois"),
                                         selectInput("yearLeft", "Select year", years, selected=2000)
                                     )
                                   ),
                                   fluidRow( 
                                     box(title = "", status="primary", width=12, 
                                         checkboxGroupInput("iconsLeft", "Select energy sources to display:",
                                                            choiceNames = sources,
                                                            choiceValues = sources,
                                                            inline=TRUE)
                                     )
                                   )
                            ),
                            #column(2),
                            column(6,
                                   fluidRow( 
                                     box(title = "Leaflet 2", status="primary", width=12, 
                                         leafletOutput("rightleaflet"),
                                         selectInput("leafletR", "Change leaflet type", leaflets, selected = "Leaflet 1")
                                     )
                                   ),
                                   fluidRow(
                                     box(status="primary", width=12,
                                         selectInput("stateRight", "Select the state to visualize", state, selected = "Illinois"),
                                         selectInput("yearRight", "Select year", years, selected=2018)
                                     )
                                   ),
                                   fluidRow( 
                                     box(title = "", status="primary", width=12, 
                                         checkboxGroupInput("iconsRight", "Select energy sources to display:",
                                                            choiceNames = sources,
                                                            choiceValues = sources,
                                                            inline=TRUE)
                                     )
                                   )
                            )
                          ),
                 ),
                 tabPanel("Generation Over a Range",
                          column(4, 
                                 fluidRow(
                                   box(status="primary", width=12,
                                       sliderInput("minSlider", "Generation Range (kMWh) Min:", 
                                                   min = 0, max=16000, value=0),
                                       sliderInput("maxSlider", "Generation Range (kMWh) Max:",
                                                   min = 16000, max=32000, value=16000)
                                   )
                                 ),
                                 fluidRow( 
                                   box(title = "Select Energy Sources", status="primary", width=12, 
                                       checkboxGroupInput("iconsRange", "Select energy sources to display:",
                                                          choiceNames = sources,
                                                          choiceValues = sources,
                                                          inline=TRUE)
                                   )
                                 )
                          ),
                          column(8,
                                 fluidRow( 
                                   box(title = "Leaflet with Range", status="primary", width=12, 
                                       leafletOutput("rangeLeaflet"),
                                       selectInput("leafletRange", "Change leaflet type", leaflets, selected = "Leaflet 1")
                                   )
                                 ),
                                 fluidRow(
                                   box(status="primary", width=12,
                                       selectInput("yearRange", "Select year", years, selected=2018),
                                   )
                                 )
                          ),
                 ),
                 tabPanel("About",
                          h1("Project 2: Raw Power"),
                          h3("Developed By: Joshua Gonzales"),
                          h4("Project 2 in CS 424 (Data Analytics / Visualization) at the University of Illinois at Chicago Spring 2021"),
                          
                          h5("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
                          
                          h3(""),
                          h4("This project contains data on the power plants from 2000, 2010, and 2018 for all 50 states of the US"),
                          h3(""),
                          h4("The data focuses on these power plants that generate these energy sources: "),
                          h4("Coal, Oil, Gas, Nuclear, Hydro, Biomass, Wind, Solar, Geothermal, and Other fuels"),
                          strong("The application helps visualize power plant data by using leaflets, with part 1 being just Illinois, part 2 being comparisons between two states, and part 3 involving displaying the whole US"),
                          h3(""),
                          
                          h5("* Libraries Used: shiny, tidyverse, sf, readxl, leaflet, ggplot2, geojsonio"),
                          
                          h5("* U.S. Environmental Protection Agency ->  https://www.epa.gov/egrid/download-data"),
                          h5("Files used: eGRID2000_plant.xls, eGRID2010_Data.xls, and egrid2018_data_v2.xlsx"),
                          
                          h5("* Created using R, RStudio, Shiny ")
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
    df <- subset(df, is.na(df$COAL)) # df with data that don't have data about generation
    return (df)
  }) 
  
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
  # For part 2 with States and Years  
  
  # LEFT SIDE
  allL <- reactive({
    df <- NULL
    
    if ("All" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      if (input$yearLeft == 2000) {
        df <- subset(df00, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        df <- subset(df10, (STATE %in% inp))
      } else {
        df <- subset(df18, (STATE %in% inp))
      }
      df <- subset(df, is.null(df$COAL)) # Displays plants that don't have any info about their data
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
  coalL <- reactive({
    coaldf <- NULL
    if ("Coal" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Non-renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        coaldf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        coaldf <- subset(df10React, (STATE %in% inp))
      } else {
        coaldf <- subset(df18React, (STATE %in% inp))
      }
      
      coaldf <- subset(coaldf, coaldf$COAL > 0)
    } 
    
    return(coaldf)
  })
  
  oilL <- reactive({
    oildf <- NULL
    if ("Oil" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Non-renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        oildf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        oildf <- subset(df10React, (STATE %in% inp))
      } else {
        oildf <- subset(df18React, (STATE %in% inp))
      }
      
      oildf <- subset(oildf, oildf$OIL > 0) 
    }
    return (oildf)
  })
  
  gasL <- reactive({
    gasdf <- NULL
    if ("Gas" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Non-renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        gasdf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        gasdf <- subset(df10React, (STATE %in% inp))
      } else {
        gasdf <- subset(df18React, (STATE %in% inp))
      }
      
      gasdf <- subset(gasdf, gasdf$GAS > 0) 
    }
    return (gasdf)
  })
  
  nuclearL <- reactive({
    nucleardf <- NULL
    if ("Nuclear" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Non-renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        nucleardf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        nucleardf <- subset(df10React, (STATE %in% inp))
      } else {
        nucleardf <- subset(df18React, (STATE %in% inp))
      }
      
      nucleardf <- subset(nucleardf, nucleardf$NUCLEAR > 0) 
    }
    return (nucleardf)
  })
  
  hydroL <- reactive({
    hydrodf <- NULL
    if ("Hydro" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        hydrodf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        hydrodf <- subset(df10React, (STATE %in% inp))
      } else {
        hydrodf <- subset(df18React, (STATE %in% inp))
      }
      
      hydrodf <- subset(hydrodf, hydrodf$HYDRO > 0) 
    }
    return (hydrodf)
  })
  
  biomassL <- reactive({
    biomassdf <- NULL
    if ("Biomass" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        biomassdf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        biomassdf <- subset(df10React, (STATE %in% inp))
      } else {
        biomassdf <- subset(df18React, (STATE %in% inp))
      }
      
      biomassdf <- subset(biomassdf, biomassdf$BIOMASS > 0) 
    }
    return (biomassdf)
  })
  
  windL <- reactive({
    winddf <- NULL
    if ("Wind" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        winddf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        winddf <- subset(df10React, (STATE %in% inp))
      } else {
        winddf <- subset(df18React, (STATE %in% inp))
      }
      
      winddf <- subset(winddf, winddf$WIND > 0) 
    }
    return (winddf)
  })
  
  solarL <- reactive({
    solardf <- NULL
    if ("Solar" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        solardf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        solardf <- subset(df10React, (STATE %in% inp))
      } else {
        solardf <- subset(df18React, (STATE %in% inp))
      }
      
      solardf <- subset(solardf, solardf$SOLAR > 0) 
    }
    return (solardf)
  })
  
  geothermalL <- reactive({
    geothermaldf <- NULL
    if ("Geothermal" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        geothermaldf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        geothermaldf <- subset(df10React, (STATE %in% inp))
      } else {
        geothermaldf <- subset(df18React, (STATE %in% inp))
      }
      
      geothermaldf <- subset(geothermaldf, geothermaldf$GEOTHERMAL > 0) 
    }
    return (geothermaldf)
  })
  
  otherL <- reactive({
    otherdf <- NULL
    if ("Other" %in% input$iconsLeft || "All" %in% input$iconsLeft || "Non-renewables" %in% input$iconsLeft) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateLeft)]
      
      # Accounts for year input
      if (input$yearLeft == 2000) {
        otherdf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearLeft == 2010) {
        otherdf <- subset(df10React, (STATE %in% inp))
      } else {
        otherdf <- subset(df18React, (STATE %in% inp))
      }
      
      otherdf <- subset(otherdf, otherdf$OTHER > 0) 
    }
    return (otherdf)
  })
  
  #=====================================================================================================================================
  
  # RIGHT SIDE  
  allR <- reactive({
    df <- NULL
    # Accounts for state input
    inp <- state.abb[which(state.name==input$stateRight)]
    
    if ("All" %in% input$iconsRight) {
      if (input$yearRight == 2000) {
        df <- subset(df00, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        df <- subset(df10, (STATE %in% inp))
      } else {
        df <- subset(df18, (STATE %in% inp))
      }
      df <- subset(df, is.null(df$COAL)) # Displays plants that don't have any info about their data
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
  coalR <- reactive({
    coaldf <- NULL
    if ("Coal" %in% input$iconsRight || "All" %in% input$iconsRight || "Non-renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        coaldf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        coaldf <- subset(df10React, (STATE %in% inp))
      } else {
        coaldf <- subset(df18React, (STATE %in% inp))
      }
      
      coaldf <- subset(coaldf, coaldf$COAL > 0)
    } 
    
    return(coaldf)
  })
  
  oilR <- reactive({
    oildf <- NULL
    if ("Oil" %in% input$iconsRight || "All" %in% input$iconsRight || "Non-renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        oildf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        oildf <- subset(df10React, (STATE %in% inp))
      } else {
        oildf <- subset(df18React, (STATE %in% inp))
      }
      
      oildf <- subset(oildf, oildf$OIL > 0) 
    }
    return (oildf)
  })
  
  gasR <- reactive({
    gasdf <- NULL
    if ("Gas" %in% input$iconsRight || "All" %in% input$iconsRight || "Non-renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        gasdf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        gasdf <- subset(df10React, (STATE %in% inp))
      } else {
        gasdf <- subset(df18React, (STATE %in% inp))
      }
      
      gasdf <- subset(gasdf, gasdf$GAS > 0) 
    }
    return (gasdf)
  })
  
  nuclearR <- reactive({
    nucleardf <- NULL
    if ("Nuclear" %in% input$iconsRight || "All" %in% input$iconsRight || "Non-renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        nucleardf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        nucleardf <- subset(df10React, (STATE %in% inp))
      } else {
        nucleardf <- subset(df18React, (STATE %in% inp))
      }
      
      nucleardf <- subset(nucleardf, nucleardf$NUCLEAR > 0) 
    }
    return (nucleardf)
  })
  
  hydroR <- reactive({
    hydrodf <- NULL
    if ("Hydro" %in% input$iconsRight || "All" %in% input$iconsRight || "Renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        hydrodf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        hydrodf <- subset(df10React, (STATE %in% inp))
      } else {
        hydrodf <- subset(df18React, (STATE %in% inp))
      }
      
      hydrodf <- subset(hydrodf, hydrodf$HYDRO > 0) 
    }
    return (hydrodf)
  })
  
  biomassR <- reactive({
    biomassdf <- NULL
    if ("Biomass" %in% input$iconsRight || "All" %in% input$iconsRight || "Renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        biomassdf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        biomassdf <- subset(df10React, (STATE %in% inp))
      } else {
        biomassdf <- subset(df18React, (STATE %in% inp))
      }
      
      biomassdf <- subset(biomassdf, biomassdf$BIOMASS > 0) 
    }
    return (biomassdf)
  })
  
  windR <- reactive({
    winddf <- NULL
    if ("Wind" %in% input$iconsRight || "All" %in% input$iconsRight || "Renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        winddf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        winddf <- subset(df10React, (STATE %in% inp))
      } else {
        winddf <- subset(df18React, (STATE %in% inp))
      }
      
      winddf <- subset(winddf, winddf$WIND > 0) 
    }
    return (winddf)
  })
  
  solarR <- reactive({
    solardf <- NULL
    if ("Solar" %in% input$iconsRight || "All" %in% input$iconsRight || "Renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        solardf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        solardf <- subset(df10React, (STATE %in% inp))
      } else {
        solardf <- subset(df18React, (STATE %in% inp))
      }
      
      solardf <- subset(solardf, solardf$SOLAR > 0) 
    }
    return (solardf)
  })
  
  geothermalR <- reactive({
    geothermaldf <- NULL
    if ("Geothermal" %in% input$iconsRight || "All" %in% input$iconsRight || "Renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        geothermaldf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        geothermaldf <- subset(df10React, (STATE %in% inp))
      } else {
        geothermaldf <- subset(df18React, (STATE %in% inp))
      }
      
      geothermaldf <- subset(geothermaldf, geothermaldf$GEOTHERMAL > 0) 
    }
    return (geothermaldf)
  })
  
  otherR <- reactive({
    otherdf <- NULL
    if ("Other" %in% input$iconsRight || "All" %in% input$iconsRight || "Non-renewables" %in% input$iconsRight) {
      # Accounts for state input
      inp <- state.abb[which(state.name==input$stateRight)]
      
      # Accounts for year input
      if (input$yearRight == 2000) {
        otherdf <- subset(df00React, (STATE %in% inp))
      } else if (input$yearRight == 2010) {
        otherdf <- subset(df10React, (STATE %in% inp))
      } else {
        otherdf <- subset(df18React, (STATE %in% inp))
      }
      
      otherdf <- subset(otherdf, otherdf$OTHER > 0) 
    }
    return (otherdf)
  })
  
  #=====================================================================================================================================
  
  # RANGE REACTIVES  
  allRa <- reactive({
    df <- NULL
    
    if ("All" %in% input$iconsRange) {
      if (input$yearRange == 2000) {
        df <- subset(df00, df00$COAL > 0)
      } else if (input$yearRange == 2010) {
        df <- subset(df10, df10$COAL > 0)
      } else {
        df <- subset(df18, df18$COAL > 0)
      }
      df <- subset(df, is.null(df$COAL)) # Displays plants that don't have any info about their data
    }
    
    return (df)
  }) 
  
  # Get reactive data from selected items in checklist based on category and source
  coalRa <- reactive({
    coaldf <- NULL
    if ("Coal" %in% input$iconsRange || "All" %in% input$iconsRange || "Non-renewables" %in% input$iconsRange) {
      
      # Accounts for year input
      if (input$yearRange == 2000) {
        coaldf <- subset(df00React, df00React$COAL > 0)
      } else if (input$yearRange == 2010) {
        coaldf <- subset(df10React, df10React$COAL > 0)
      } else {
        coaldf <- subset(df18React, df18React$COAL > 0)
      }
      
    } 
    
    return(coaldf)
  })
  
  oilRa <- reactive({
    oildf <- NULL
    if ("Oil" %in% input$iconsRange || "All" %in% input$iconsRange || "Non-renewables" %in% input$iconsRange) {
      
      # Accounts for year input
      if (input$yearRange == 2000) {
        oildf <- subset(df00React, df00React$OIL > 0) 
      } else if (input$yearRange == 2010) {
        oildf <- subset(df10React, df10React$OIL > 0) 
      } else {
        oildf <- subset(df18React, df18React$OIL > 0) 
      }
      
    }
    return (oildf)
  })
  
  gasRa <- reactive({
    gasdf <- NULL
    if ("Gas" %in% input$iconsRange || "All" %in% input$iconsRange || "Non-renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        gasdf <- subset(df00React, df00React$GAS > 0) 
      } else if (input$yearRange == 2010) {
        gasdf <- subset(df10React, df10React$GAS > 0) 
      } else {
        gasdf <- subset(df18React, df18React$GAS > 0) 
      }
      
    }
    return (gasdf)
  })
  
  nuclearRa <- reactive({
    nucleardf <- NULL
    if ("Nuclear" %in% input$iconsRange || "All" %in% input$iconsRange || "Non-renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        nucleardf <- subset(df00React, df00React$NUCLEAR > 0) 
      } else if (input$yearRange == 2010) {
        nucleardf <- subset(df10React, df10React$NUCLEAR > 0) 
      } else {
        nucleardf <- subset(df18React, df18React$NUCLEAR > 0) 
      }
    }
    return (nucleardf)
  })
  
  hydroRa <- reactive({
    hydrodf <- NULL
    if ("Hydro" %in% input$iconsRange || "All" %in% input$iconsRange || "Renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        hydrodf <- subset(df00React, df00React$HYDRO > 0) 
      } else if (input$yearRange == 2010) {
        hydrodf <- subset(df10React, df10React$HYDRO > 0) 
      } else {
        hydrodf <- subset(df18React, df18React$HYDRO > 0) 
      }
      
    }
    return (hydrodf)
  })
  
  biomassRa <- reactive({
    biomassdf <- NULL
    if ("Biomass" %in% input$iconsRange || "All" %in% input$iconsRange || "Renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        biomassdf <- subset(df00React, df00React$BIOMASS > 0) 
      } else if (input$yearRange == 2010) {
        biomassdf <- subset(df10React, df10React$BIOMASS > 0) 
      } else {
        biomassdf <- subset(df18React, df18React$BIOMASS > 0) 
      }
    }
    return (biomassdf)
  })
  
  windRa <- reactive({
    winddf <- NULL
    if ("Wind" %in% input$iconsRange || "All" %in% input$iconsRange || "Renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        winddf <- subset(df00React, df00React$WIND > 0) 
      } else if (input$yearRange == 2010) {
        winddf <- subset(df10React, df10React$WIND > 0) 
      } else {
        winddf <- subset(df18React, df18React$WIND > 0) 
      }
    }
    return (winddf)
  })
  
  solarRa <- reactive({
    solardf <- NULL
    if ("Solar" %in% input$iconsRange || "All" %in% input$iconsRange || "Renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        solardf <- subset(df00React, df00React$SOLAR > 0) 
      } else if (input$yearRange == 2010) {
        solardf <- subset(df10React, df10React$SOLAR > 0) 
      } else {
        solardf <- subset(df18React, df18React$SOLAR > 0) 
      }
    }
    return (solardf)
  })
  
  geothermalRa <- reactive({
    geothermaldf <- NULL
    if ("Geothermal" %in% input$iconsRange || "All" %in% input$iconsRange || "Renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        geothermaldf <- subset(df00React, df00React$GEOTHERMAL > 0) 
      } else if (input$yearRange == 2010) {
        geothermaldf <- subset(df10React, df10React$GEOTHERMAL > 0) 
      } else {
        geothermaldf <- subset(df18React, df18React$GEOTHERMAL > 0) 
      }
    }
    return (geothermaldf)
  })
  
  otherRa <- reactive({
    otherdf <- NULL
    if ("Other" %in% input$iconsRange || "All" %in% input$iconsRange || "Non-renewables" %in% input$iconsRange) {
      # Accounts for year input
      if (input$yearRange == 2000) {
        otherdf <- subset(df00React, df00React$OTHER > 0) 
      } else if (input$yearRange == 2010) {
        otherdf <- subset(df10React, df10React$OTHER > 0) 
      } else {
        otherdf <- subset(df18React, df18React$OTHER > 0) 
      }
    }
    return (otherdf)
  })
  #=====================================================================================================================================
  # Display Output
  types <- c("None", "Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar",
             "Geothermal", "Other")
  
  pal <- colorFactor(palette = c("thistle1", "black", "grey40", "midnightblue", "chartreuse4", "deepskyblue", "red2", "lightblue",
                                 "gold", "brown", "burlywood"), 
                     levels = types)
  
  # PART 1 ILLINOIS *****************************************
  # Static initial leaflet, has observer for checkbox presses
  output$test <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=39.8331, lng=-88.8985, zoom=6) %>%
      addLegend("bottomright", pal=pal, values=types, title="Energy Sources", opacity=1) %>%
      addControl(actionButton("zoomer", "Reset"), position="bottomleft")
  })
  
  observeEvent(input$icons, {
    
    # Reset the map first to account for deselecting of checkboxes?
    leafletProxy(mapId="test", data=df18IL) %>%
      clearMarkers()
    
    # if nothing is checked, then just clear the markers
    if (is.null(input$icons)) {
      
    } else {
      # Get necessary data that was selected, Part I of event handling
      alldf18 <- all18()      
      coaldf18 <- coal18()
      oildf18 <- oil18()
      gasdf18 <- gas18()
      nucleardf18 <- nuclear18()
      hydrodf18 <- hydro18()
      biomassdf18 <- biomass18()
      winddf18 <- wind18()
      solardf18 <- solar18()
      geothermaldf18 <- geothermal18()
      otherdf18 <- other18()
      
      # Try adding circle markers for each source one by one independently instead of aggregating
      # add reactive values for each energy source so we can add circle markers one by one
      # Part II of event handling
      if (!is.null(alldf18)) {
        leafletProxy(mapId="test", data=alldf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=alldf18, lng=alldf18$LON, lat=alldf18$LAT, radius=7,
                           color='thistle1', stroke=FALSE, fillOpacity=1, label=paste("Source= None")) 
      }
      
      if (!is.null(coaldf18)) {
        leafletProxy(mapId="test", data=coaldf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=coaldf18, lng=coaldf18$LON, lat=coaldf18$LAT, radius=7,
                           color='black', stroke=FALSE, fillOpacity=1, label=paste("Source= Coal")) 
      } 
      
      if (!is.null(oildf18)) {
        leafletProxy(mapId="test", data=oildf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=oildf18, lng=(oildf18$LON+0.001), lat=oildf18$LAT, radius=7,
                           color='grey', stroke=FALSE, fillOpacity=1, label=paste("Source= Oil")) 
      }
      
      if (!is.null(gasdf18)) {
        leafletProxy(mapId="test", data=gasdf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=gasdf18, lng=(gasdf18$LON-0.001), lat=gasdf18$LAT, radius=7,
                           color='midnightblue', stroke=FALSE, fillOpacity=1, label=paste("Source= Gas")) 
      }
      
      if (!is.null(nucleardf18)) {
        leafletProxy(mapId="test", data=nucleardf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=nucleardf18, lng=nucleardf18$LON, (lat=nucleardf18$LAT+0.001), radius=7,
                           color='green', stroke=FALSE, fillOpacity=1, label=paste("Source= Nuclear")) 
      }
      
      if (!is.null(hydrodf18)) {
        leafletProxy(mapId="test", data=hydrodf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=hydrodf18, lng=hydrodf18$LON, (lat=hydrodf18$LAT-0.001), radius=7,
                           color='deepskyblue', stroke=FALSE, fillOpacity=1, label=paste("Source= Hydro")) 
      }
      
      if (!is.null(biomassdf18)) {
        leafletProxy(mapId="test", data=biomassdf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=biomassdf18, lng=(biomassdf18$LON+0.002), lat=biomassdf18$LAT, radius=7,
                           color='red', stroke=FALSE, fillOpacity=1, label=paste("Source= Biomass")) 
      }
      
      if (!is.null(winddf18)) {
        leafletProxy(mapId="test", data=winddf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=winddf18, (lng=winddf18$LON-0.002), lat=winddf18$LAT, radius=7,
                           color='lightblue', stroke=FALSE, fillOpacity=1, label=paste("Source= Wind")) 
      }
      
      if (!is.null(solardf18)) {
        leafletProxy(mapId="test", data=solardf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=solardf18, lng=solardf18$LON, (lat=solardf18$LAT+0.002), radius=7,
                           color='gold', stroke=FALSE, fillOpacity=1, label=paste("Source= Solar")) 
      }
      
      if (!is.null(geothermaldf18)) {
        leafletProxy(mapId="test", data=geothermaldf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=geothermaldf18, lng=geothermaldf18$LON, (lat=geothermaldf18$LAT-0.002), radius=7,
                           color='brown', stroke=FALSE, fillOpacity=1, label=paste("Source= Geothermal")) 
      }
      
      if (!is.null(otherdf18)) {
        leafletProxy(mapId="test", data=otherdf18) %>% # clearMarkers() %>%
          addCircleMarkers(data=otherdf18, (lng=otherdf18$LON+0.001), (lat=otherdf18$LAT+0.001), radius=7,
                           color='burlywood', stroke=FALSE, fillOpacity=1, label=paste("Source= Other")) 
      }
      
    }
  })
  
  observeEvent(input$zoomer, {
    leafletProxy("test") %>% setView(lat=39.8331, lng=-88.8985, zoom=6)
  })
  
  #=====================================================================================================================================  
  # PART 2 COMPARISON ************************************
  # Static initial leaflets, has observers for user inputs
  output$leftleaflet <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=39.8331, lng=-88.8985, zoom=6) %>%
      addCircleMarkers(data=df00IL, lng=df00IL$LON, lat=df00IL$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types)) %>%
      addLegendCustom(colors = c("black", "black", "black"), labels = c("Generation < 80000", "Generation < 150000", "Generation >= 150000"), sizes = c(6, 8, 10)) %>%
      addLegend("bottomright", pal=pal, values=types, title="Energy Sources", opacity=1) %>% 
      addControl(actionButton("zoomerLeft", "Reset"), position="bottomleft")
  })
  
  output$rightleaflet <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=39.8331, lng=-88.8985, zoom=6) %>%
      addCircleMarkers(data=df18IL, lng=df18IL$LON, lat=df18IL$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types)) %>%
      addLegendCustom(colors = c("black", "black", "black"), labels = c("Generation < 80000", "Generation < 150000", "Generation >= 150000"), sizes = c(6, 8, 10)) %>%
      addLegend("bottomright", pal=pal, values=types, title="Energy Sources", opacity=1) %>%
      addControl(actionButton("zoomerRight", "Reset"), position="bottomleft")
  })
  
  # In observe, make sure to change the label using paste
  #=====================================================================================================================================  
  # Left leaflet
  observeEvent(input$iconsLeft, {
    
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearLeft == 2000) {
      dfToUse = df00
    } else if (input$yearLeft == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    # From state input, get lat and lon from dfToUse
    inp <- state.abb[which(state.name==input$stateLeft)]
    dfToUse <- subset(dfToUse, STATE %in% inp)
    
    # Reset the map first to account for deselecting of checkboxes?
    leafletProxy(mapId="leftleaflet", data=dfToUse) %>%
      clearMarkers()
    
    # If nothing is checked, then just clear the markers
    if (is.null(input$iconsLeft)) {
      
    } else {
      # Get necessary data that was selected, Part I of event handling
      
      # Able to account for input$yearLeft and input$stateLeft in reactive values
      alldf <- allL()
      coaldf <- coalL()
      oildf <- oilL()
      gasdf <- gasL()
      nucleardf <- nuclearL()
      hydrodf <- hydroL()
      biomassdf <- biomassL()
      winddf <- windL()
      solardf <- solarL()
      geothermaldf <- geothermalL()
      otherdf <- otherL()
      
      sourceList <- c()
      
      # Try adding circle markers for each source one by one independently instead of aggregating
      # add reactive values for each energy source so we can add circle markers one by one
      # Part II of event handling
      
      # Check if All (default) is checked
      if (!is.null(alldf)) {
        leafletProxy(mapId="leftleaflet", data=alldf) %>% 
          addCircleMarkers(data=alldf, (lng=alldf$LON+0.001), (lat=alldf$LAT-0.001), radius=7,
                           color='thistle1', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= None || Plant Name: ", alldf$NAME, "Generation Capacity= None ", 
                                       " || Renewable= NA", alldf$PERCENT_R, " || Non-renewable= NA", alldf$PERCENT_NR)) 
      }
      
      if (!is.null(coaldf)) {
        sourceList <- append(sourceList, "Coal")
        leafletProxy(mapId="leftleaflet", data=coaldf) %>% 
          addCircleMarkers(data=coaldf, lng=coaldf$LON, lat=coaldf$LAT, radius=(if(coaldf$COAL<=80000) {6} else if (coaldf$COAL<=150000) {8} else {10}),
                           color='black', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Coal || Plant Name: ", coaldf$NAME, "Generation Capacity= ",  coaldf$COAL,
                                       " || Renewable= ", coaldf$PERCENT_R, "% || Non-renewable= ", coaldf$PERCENT_NR, "%")) 
      } 
      
      if (!is.null(oildf)) {
        sourceList <- append(sourceList, "Oil")
        leafletProxy(mapId="leftleaflet", data=oildf) %>% 
          addCircleMarkers(data=oildf, lng=(oildf$LON+0.001), lat=oildf$LAT, radius=(if(oildf$OIL<=80000) {6} else if (oildf$OIL<=150000) {8} else {10}),
                           color='grey', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Oil || Plant Name: ", oildf$NAME, "Generation Capacity= ",  oildf$OIL,
                                       " || Renewable= ", oildf$PERCENT_R, "% || Non-renewable= ", oildf$PERCENT_NR, "%"))
      }
      
      if (!is.null(gasdf)) {
        sourceList <- append(sourceList, "Gas")
        leafletProxy(mapId="leftleaflet", data=gasdf) %>% 
          addCircleMarkers(data=gasdf, lng=(gasdf$LON-0.001), lat=gasdf$LAT, radius=(if(gasdf$GAS<=80000) {6} else if (gasdf$GAS<=150000) {8} else {10}),
                           color='midnightblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Gas || Plant Name: ", gasdf$NAME, "Generation Capacity= ",  gasdf$GAS,
                                       " || Renewable= ", gasdf$PERCENT_R, "% || Non-renewable= ", gasdf$PERCENT_NR, "%"))
      }
      
      if (!is.null(nucleardf)) {
        sourceList <- append(sourceList, "Nuclear")
        leafletProxy(mapId="leftleaflet", data=nucleardf) %>% 
          addCircleMarkers(data=nucleardf, lng=nucleardf$LON, (lat=nucleardf$LAT+0.001), radius=(if(nucleardf$NUCLEAR<=80000) {6} else if (nucleardf$NUCLEAR<=150000) {8} else {10}),
                           color='green', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Nuclear || Plant Name: ", nucleardf$NAME, "Generation Capacity= ",  nucleardf$NUCLEAR,
                                       " || Renewable= ", nucleardf$PERCENT_R, "% || Non-renewable= ", nucleardf$PERCENT_NR, "%")) 
      }
      
      if (!is.null(hydrodf)) {
        sourceList <- append(sourceList, "Hydro")
        leafletProxy(mapId="leftleaflet", data=hydrodf) %>% 
          addCircleMarkers(data=hydrodf, lng=hydrodf$LON, (lat=hydrodf$LAT-0.001), radius=(if(hydrodf$HYDRO<=80000) {6} else if (hydrodf$HYDRO<=150000) {8} else {10}),
                           color='deepskyblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Hydro || Plant Name: ", hydrodf$NAME, "Generation Capacity= ",  hydrodf$HYDRO,
                                       " || Renewable= ", hydrodf$PERCENT_R, "% || Non-renewable= ", hydrodf$PERCENT_NR, "%"))
      }
      
      if (!is.null(biomassdf)) {
        sourceList <- append(sourceList, "Biomass")
        leafletProxy(mapId="leftleaflet", data=biomassdf) %>% 
          addCircleMarkers(data=biomassdf, lng=(biomassdf$LON+0.002), lat=biomassdf$LAT, radius=(if(biomassdf$BIOMASS<=80000) {6} else if (biomassdf$BIOMASS<=150000) {8} else {10}),
                           color='red', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Biomass || Plant Name: ", biomassdf$NAME, "Generation Capacity= ",  biomassdf$BIOMASS,
                                       " || Renewable= ", biomassdf$PERCENT_R, "% || Non-renewable= ", biomassdf$PERCENT_NR, "%"))
      }
      
      if (!is.null(winddf)) {
        sourceList <- append(sourceList, "Wind")
        leafletProxy(mapId="leftleaflet", data=winddf) %>% 
          addCircleMarkers(data=winddf, (lng=winddf$LON-0.002), lat=winddf$LAT, radius=7,
                           color='lightblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Wind || Plant Name: ", winddf$NAME, "Generation Capacity= ",  winddf$WIND,
                                       " || Renewable= ", winddf$PERCENT_R, "% || Non-renewable= ", winddf$PERCENT_NR, "%"))
      }
      
      if (!is.null(solardf)) {
        sourceList <- append(sourceList, "Solar")
        leafletProxy(mapId="leftleaflet", data=solardf) %>% 
          addCircleMarkers(data=solardf, lng=solardf$LON, (lat=solardf$LAT+0.002), radius=7,
                           color='gold', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Solar || Plant Name: ", solardf$NAME, "Generation Capacity= ",  solardf$SOLAR,
                                       " || Renewable= ", solardf$PERCENT_R, "% || Non-renewable= ", solardf$PERCENT_NR, "%"))
      }
      
      if (!is.null(geothermaldf)) {
        sourceList <- append(sourceList, "Geothermal")
        leafletProxy(mapId="leftleaflet", data=geothermaldf) %>% 
          addCircleMarkers(data=geothermaldf, lng=geothermaldf$LON, (lat=geothermaldf$LAT-0.002), radius=7,
                           color='brown', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Geothermal || Plant Name: ", geothermaldf$NAME, "Generation Capacity= ",  geothermaldf$GEOTHERMAL,
                                       " || Renewable= ", geothermaldf$PERCENT_R, "% || Non-renewable= ", geothermaldf$PERCENT_NR, "%"))
      }
      
      if (!is.null(otherdf)) {
        sourceList <- append(sourceList, "Other")
        leafletProxy(mapId="leftleaflet", data=otherdf) %>% 
          addCircleMarkers(data=otherdf, (lng=otherdf$LON+0.001), (lat=otherdf$LAT+0.001), radius=(if(otherdf$OTHER<=80000) {6} else if (otherdf$OTHER<=150000) {8} else {10}),
                           color='burlywood', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Other || Plant Name: ", otherdf$NAME, "Generation Capacity= ",  otherdf$OTHER,
                                       " || Renewable= ", otherdf$PERCENT_R, "% || Non-renewable= ", otherdf$PERCENT_NR, "%"))
      }
      
    }
  })
  
  #=====================================================================================================================================    
  # Right leaflet
  observeEvent(input$iconsRight, {
    
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearRight == 2000) {
      dfToUse = df00
    } else if (input$yearRight == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    # From state input, get lat and lon from dfToUse
    inp <- state.abb[which(state.name==input$stateRight)]
    dfToUse <- subset(dfToUse, STATE %in% inp)
    
    # Reset the map first to account for deselecting of checkboxes?
    leafletProxy(mapId="rightleaflet", data=dfToUse) %>%
      clearMarkers() 
    
    # if nothing is checked, then just clear the markers
    if (is.null(input$iconsRight)) {
      
    } else {
      # Get necessary data that was selected, Part I of event handling
      
      # Able to account for input$yearLeft and input$stateLeft in reactive values
      alldf <- allR()
      coaldf <- coalR()
      oildf <- oilR()
      gasdf <- gasR()
      nucleardf <- nuclearR()
      hydrodf <- hydroR()
      biomassdf <- biomassR()
      winddf <- windR()
      solardf <- solarR()
      geothermaldf <- geothermalR()
      otherdf <- otherR()
      
      sourceList <- c()
      
      # Try adding circle markers for each source one by one independently instead of aggregating
      # add reactive values for each energy source so we can add circle markers one by one
      # Part II of event handling
      if (!is.null(alldf)) {
        leafletProxy(mapId="rightleaflet", data=alldf) %>% 
          addCircleMarkers(data=alldf, (lng=alldf$LON+0.001), (lat=alldf$LAT-0.001), radius=7,
                           color='thistle1', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= None || ", alldf$NAME, "Generation Capacity= None", 
                                       " || Renewable= NA", alldf$PERCENT_R, " || Non-renewable= NA", alldf$PERCENT_NR)) 
      }
      
      if (!is.null(coaldf)) {
        sourceList <- append(sourceList, "Coal")
        leafletProxy(mapId="rightleaflet", data=coaldf) %>% 
          addCircleMarkers(data=coaldf, lng=coaldf$LON, lat=coaldf$LAT, radius=(if(coaldf$COAL<=80000) {6} else if (coaldf$COAL<=150000) {8} else {10}),
                           color='black', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Coal || ", coaldf$NAME, "Generation Capacity= ",  coaldf$COAL,
                                       " || Renewable= ", coaldf$PERCENT_R, "% || Non-renewable= ", coaldf$PERCENT_NR, "%")) 
      } 
      
      if (!is.null(oildf)) {
        sourceList <- append(sourceList, "Oil")
        leafletProxy(mapId="rightleaflet", data=oildf) %>% 
          addCircleMarkers(data=oildf, lng=(oildf$LON+0.001), lat=oildf$LAT, radius=(if(oildf$OIL<=80000) {6} else if (oildf$OIL<=150000) {8} else {10}),
                           color='grey', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Oil || ", oildf$NAME, "Generation Capacity= ",  oildf$OIL,
                                       " || Renewable= ", oildf$PERCENT_R, "% || Non-renewable= ", oildf$PERCENT_NR, "%"))
      }
      
      if (!is.null(gasdf)) {
        sourceList <- append(sourceList, "Gas")
        leafletProxy(mapId="rightleaflet", data=gasdf) %>% 
          addCircleMarkers(data=gasdf, lng=(gasdf$LON-0.001), lat=gasdf$LAT, radius=(if(gasdf$GAS<=80000) {6} else if (gasdf$GAS<=150000) {8} else {10}),
                           color='midnightblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Gas || ", gasdf$NAME, "Generation Capacity= ",  gasdf$OIL,
                                       " || Renewable= ", gasdf$PERCENT_R, "% || Non-renewable= ", gasdf$PERCENT_NR, "%"))
      }
      
      if (!is.null(nucleardf)) {
        sourceList <- append(sourceList, "Nuclear")
        leafletProxy(mapId="rightleaflet", data=nucleardf) %>% 
          addCircleMarkers(data=nucleardf, lng=nucleardf$LON, (lat=nucleardf$LAT+0.001), radius=(if(nucleardf$NUCLEAR<=80000) {6} else if (nucleardf$NUCLEAR<=150000) {8} else {10}),
                           color='green', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Nuclear || ", nucleardf$NAME, "Generation Capacity= ",  nucleardf$NUCLEAR,
                                       " || Renewable= ", nucleardf$PERCENT_R, "% || Non-renewable= ", nucleardf$PERCENT_NR, "%")) 
      }
      
      if (!is.null(hydrodf)) {
        sourceList <- append(sourceList, "Hydro")
        leafletProxy(mapId="rightleaflet", data=hydrodf) %>% 
          addCircleMarkers(data=hydrodf, lng=hydrodf$LON, (lat=hydrodf$LAT-0.001), radius=(if(hydrodf$HYDRO<=80000) {6} else if (hydrodf$HYDRO<=150000) {8} else {10}),
                           color='deepskyblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Hydro || ", hydrodf$NAME, "Generation Capacity= ",  hydrodf$HYDRO,
                                       " || Renewable= ", hydrodf$PERCENT_R, "% || Non-renewable= ", hydrodf$PERCENT_NR, "%"))
      }
      
      if (!is.null(biomassdf)) {
        sourceList <- append(sourceList, "Biomass")
        leafletProxy(mapId="rightleaflet", data=biomassdf) %>% 
          addCircleMarkers(data=biomassdf, lng=(biomassdf$LON+0.002), lat=biomassdf$LAT, radius=(if(biomassdf$BIOMASS<=80000) {6} else if (biomassdf$BIOMASS<=150000) {8} else {10}),
                           color='red', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Biomass || ", biomassdf$NAME, "Generation Capacity= ",  biomassdf$BIOMASS,
                                       " || Renewable= ", biomassdf$PERCENT_R, "% || Non-renewable= ", biomassdf$PERCENT_NR, "%"))
      }
      
      if (!is.null(winddf)) {
        sourceList <- append(sourceList, "Wind")
        leafletProxy(mapId="rightleaflet", data=winddf) %>% 
          addCircleMarkers(data=winddf, (lng=winddf$LON-0.002), lat=winddf$LAT, radius=(if(winddf$WIND<=80000) {6} else if (winddf$WIND<=150000) {8} else {10}),
                           color='lightblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Wind || ", winddf$NAME, "Generation Capacity= ",  winddf$WIND,
                                       " || Renewable= ", winddf$PERCENT_R, "% || Non-renewable= ", winddf$PERCENT_NR, "%"))
      }
      
      if (!is.null(solardf)) {
        sourceList <- append(sourceList, "Solar")
        leafletProxy(mapId="rightleaflet", data=solardf) %>% 
          addCircleMarkers(data=solardf, lng=solardf$LON, (lat=solardf$LAT+0.002), radius=7,
                           color='gold', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Solar || ", solardf$NAME, "Generation Capacity= ",  solardf$SOLAR,
                                       " || Renewable= ", solardf$PERCENT_R, "% || Non-renewable= ", solardf$PERCENT_NR, "%"))
      }
      
      if (!is.null(geothermaldf)) {
        sourceList <- append(sourceList, "Geothermal")
        leafletProxy(mapId="rightleaflet", data=geothermaldf) %>% 
          addCircleMarkers(data=geothermaldf, lng=geothermaldf$LON, (lat=geothermaldf$LAT-0.002), radius=7,
                           color='brown', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Geothermal || ", geothermaldf$NAME, "Generation Capacity= ",  geothermaldf$GEOTHERMAL,
                                       " || Renewable= ", geothermaldf$PERCENT_R, "% || Non-renewable= ", geothermaldf$PERCENT_NR, "%"))
      }
      
      if (!is.null(otherdf)) {
        sourceList <- append(sourceList, "Other")
        leafletProxy(mapId="rightleaflet", data=otherdf) %>% 
          addCircleMarkers(data=otherdf, (lng=otherdf$LON+0.001), (lat=otherdf$LAT+0.001), radius=(if(otherdf$OTHER<=80000) {6} else if (otherdf$OTHER<=150000) {8} else {10}),
                           color='burlywood', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Other || ", otherdf$NAME, "Generation Capacity= ",  otherdf$OTHER,
                                       " || Renewable= ", otherdf$PERCENT_R, "% || Non-renewable= ", otherdf$PERCENT_NR, "%"))
      }
      
    }
  })
  
  #===================================================================================================================================== 
  # PART 3 Range US map
  output$rangeLeaflet <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=37.0902, -95.7129, zoom=4) %>%
      addLegend("bottomright", pal=pal, values=types, title="Energy Sources", opacity=1) %>% #
      addCircleMarkers(data=df18, lng=df18$LON, lat=df18$LAT, radius=5, color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types)) %>%
      addLegendCustom(colors = c("black", "black", "black"), labels = c("Generation < 80000", "Generation < 150000", "Generation >= 150000"), sizes = c(6, 8, 10)) %>%
      addControl(actionButton("zoomerRange", "Reset"), position="bottomleft")
  })
  
  #===================================================================================================================================== 
  # Sets view based on state selected
  observeEvent(input$stateLeft, {
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearLeft == 2000) {
      dfToUse = df00
    } else if (input$yearLeft == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    # From state input, get lat and lon from dfToUse
    inp <- state.abb[which(state.name==input$stateLeft)]
    dfToUse <- subset(dfToUse, STATE %in% inp)
    
    state_data <- subset(statedf, State %in% inp)
    
    # Set view to look at selected State
    leafletProxy(mapId="leftleaflet", data=dfToUse) %>%
      clearMarkers() %>%
      setView(lat=state_data$Latitude, lng=state_data$Longitude, zoom=6) %>% 
      addCircleMarkers(data=dfToUse, lng=dfToUse$LON, lat=dfToUse$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types))
  })
  
  observeEvent(input$stateRight, {
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearRight == 2000) {
      dfToUse = df00
    } else if (input$yearRight == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    # From state input, get lat and lon from dfToUse
    inp <- state.abb[which(state.name==input$stateRight)]
    dfToUse <- subset(dfToUse, STATE %in% inp)
    
    state_data <- subset(statedf, State %in% inp)
    
    # Set view to look at selected State
    leafletProxy(mapId="rightleaflet", data=dfToUse) %>%
      clearMarkers() %>% 
      setView(lat=state_data$Latitude, lng=state_data$Longitude, zoom=6) %>% 
      addCircleMarkers(data=dfToUse, lng=dfToUse$LON, lat=dfToUse$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types))
  })
  
  observeEvent(input$yearLeft, {
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearRight == 2000) {
      dfToUse = df00
    } else if (input$yearRight == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    # From state input, get lat and lon from dfToUse
    inp <- state.abb[which(state.name==input$stateRight)]
    dfToUse <- subset(dfToUse, STATE %in% inp)
    
    state_data <- subset(statedf, State %in% inp)
    
    # Set view to look at selected State
    leafletProxy(mapId="rightleaflet", data=dfToUse) %>%
      clearMarkers() %>% 
      setView(lat=state_data$Latitude, lng=state_data$Longitude, zoom=6) %>% 
      addCircleMarkers(data=dfToUse, lng=dfToUse$LON, lat=dfToUse$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types))
  })
  
  observeEvent(input$yearRight, {
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearRight == 2000) {
      dfToUse = df00
    } else if (input$yearRight == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    # From state input, get lat and lon from dfToUse
    inp <- state.abb[which(state.name==input$stateRight)]
    dfToUse <- subset(dfToUse, STATE %in% inp)
    
    state_data <- subset(statedf, State %in% inp)
    
    # Set view to look at selected State
    leafletProxy(mapId="rightleaflet", data=dfToUse) %>%
      clearMarkers() %>% 
      setView(lat=state_data$Latitude, lng=state_data$Longitude, zoom=6) %>% 
      addCircleMarkers(data=dfToUse, lng=dfToUse$LON, lat=dfToUse$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types))
  })
  
  #===================================================================================================================================== 
  # Resets view to original position
  observeEvent(input$zoomerLeft, {
    leafletProxy("leftleaflet") %>% setView(lat=37.0902, -95.7129, zoom=4)
  })
  
  observeEvent(input$zoomerRight, {
    leafletProxy("rightleaflet") %>% setView(lat=37.0902, lng=-95.7129, zoom=4)
  })
  
  observeEvent(input$zoomerRange, {
    leafletProxy("rangeLeaflet") %>% setView(lat=37.0902, lng=-95.7129, zoom=4)
  })
  
  #===================================================================================================================================== 
  # Changes leaflets
  observeEvent(input$leafletL, {
    if (input$leafletL == "Leaflet 1") {
      leafletProxy("leftleaflet") %>% clearMarkers() %>% addTiles()
    } else if (input$leafletL == "Leaflet 2") {
      leafletProxy("leftleaflet") %>% clearMarkers() %>% addProviderTiles(providers$Stamen.Terrain)
    } else {
      leafletProxy("leftleaflet") %>% clearMarkers() %>% addProviderTiles(providers$Esri.WorldImagery)
    }
    
  })
  
  observeEvent(input$leafletR, {
    if (input$leafletR == "Leaflet 1") {
      leafletProxy("rightleaflet") %>% clearMarkers() %>% addTiles()
    } else if (input$leafletR == "Leaflet 2") {
      leafletProxy("rightleaflet") %>% clearMarkers() %>% addProviderTiles(providers$Stamen.Terrain)
    } else {
      leafletProxy("rightleaflet") %>% clearMarkers() %>% addProviderTiles(providers$Esri.WorldImagery)
    } 
  })
  
  observeEvent(input$leafletRange, {
    if (input$leafletRange == "Leaflet 1") {
      leafletProxy("rangeLeaflet") %>% clearMarkers() %>% addTiles()
    } else if (input$leafletRange == "Leaflet 2") {
      leafletProxy("rangeLeaflet") %>% clearMarkers() %>% addProviderTiles(providers$Stamen.Terrain)
    } else {
      leafletProxy("rangeLeaflet") %>% clearMarkers() %>% addProviderTiles(providers$Esri.WorldImagery)
    } 
  })
  
  #===================================================================================================================================== 
  
  observeEvent(input$minSlider, {
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearRange == 2000) {
      dfToUse = df00
    } else if (input$yearRange == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    coaldf <- subset(dfToUse, dfToUse$COAL > input$minSlider & dfToUse$COAL < input$maxSlider)
    oildf <- subset(dfToUse, dfToUse$OIL > input$minSlider & dfToUse$OIL < input$maxSlider)
    gasdf <- subset(dfToUse, dfToUse$GAS > input$minSlider & dfToUse$GAS < input$maxSlider)
    nucleardf <- subset(dfToUse, dfToUse$NUCLEAR > input$minSlider & dfToUse$NUCLEAR < input$maxSlider)
    hydrodf <- subset(dfToUse, dfToUse$HYDRO > input$minSlider & dfToUse$HYDRO < input$maxSlider)
    biomassdf <- subset(dfToUse, dfToUse$BIOMASS > input$minSlider & dfToUse$BIOMASS < input$maxSlider)
    winddf <- subset(dfToUse, dfToUse$WIND > input$minSlider & dfToUse$WIND < input$maxSlider)
    solardf <- subset(dfToUse, dfToUse$SOLAR > input$minSlider & dfToUse$SOLAR < input$maxSlider)
    geothermaldf <- subset(dfToUse, dfToUse$GEOTHERMAL > input$minSlider & dfToUse$GEOTHERMAL < input$maxSlider)
    otherdf <- subset(dfToUse, dfToUse$OTHER > input$minSlider & dfToUse$OTHER < input$maxSlider)
    
    dfToUse <- rbind(coaldf, oildf)
    dfToUse <- rbind(gasdf, dfToUse)
    dfToUse <- rbind(nucleardf, dfToUse)
    dfToUse <- rbind(hydrodf, dfToUse)
    dfToUse <- rbind(biomassdf, dfToUse)
    dfToUse <- rbind(winddf, dfToUse)
    dfToUse <- rbind(solardf, dfToUse)
    dfToUse <- rbind(geothermaldf, dfToUse)
    dfToUse <- rbind(otherdf, dfToUse)
    
    # Set view to look at selected State
    leafletProxy(mapId="rangeLeaflet", data=dfToUse) %>%
      clearMarkers() %>% 
      addCircleMarkers(data=dfToUse, lng=dfToUse$LON, lat=dfToUse$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types))
  })
  
  observeEvent(input$maxSlider, {
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearRange == 2000) {
      dfToUse = df00
    } else if (input$yearRange == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    coaldf <- subset(dfToUse, dfToUse$COAL > input$minSlider & dfToUse$COAL < input$maxSlider)
    oildf <- subset(dfToUse, dfToUse$OIL > input$minSlider & dfToUse$OIL < input$maxSlider)
    gasdf <- subset(dfToUse, dfToUse$GAS > input$minSlider & dfToUse$GAS < input$maxSlider)
    nucleardf <- subset(dfToUse, dfToUse$NUCLEAR > input$minSlider & dfToUse$NUCLEAR < input$maxSlider)
    hydrodf <- subset(dfToUse, dfToUse$HYDRO > input$minSlider & dfToUse$HYDRO < input$maxSlider)
    biomassdf <- subset(dfToUse, dfToUse$BIOMASS > input$minSlider & dfToUse$BIOMASS < input$maxSlider)
    winddf <- subset(dfToUse, dfToUse$WIND > input$minSlider & dfToUse$WIND < input$maxSlider)
    solardf <- subset(dfToUse, dfToUse$SOLAR > input$minSlider & dfToUse$SOLAR < input$maxSlider)
    geothermaldf <- subset(dfToUse, dfToUse$GEOTHERMAL > input$minSlider & dfToUse$GEOTHERMAL < input$maxSlider)
    otherdf <- subset(dfToUse, dfToUse$OTHER > input$minSlider & dfToUse$OTHER < input$maxSlider)
    
    dfToUse <- rbind(coaldf, oildf)
    dfToUse <- rbind(gasdf, dfToUse)
    dfToUse <- rbind(nucleardf, dfToUse)
    dfToUse <- rbind(hydrodf, dfToUse)
    dfToUse <- rbind(biomassdf, dfToUse)
    dfToUse <- rbind(winddf, dfToUse)
    dfToUse <- rbind(solardf, dfToUse)
    dfToUse <- rbind(geothermaldf, dfToUse)
    dfToUse <- rbind(otherdf, dfToUse)
    
    # Set view to look at selected State
    leafletProxy(mapId="rangeLeaflet", data=dfToUse) %>%
      clearMarkers() %>% 
      addCircleMarkers(data=dfToUse, lng=dfToUse$LON, lat=dfToUse$LAT, radius=7,
                       color=~pal(types), stroke=FALSE, fillOpacity=1, label=paste("Source=", types))
  })  
  
  observeEvent(input$iconsRange, {
    # If select input has selected 2000's, 2010's, or 2018's df 
    if (input$yearRange == 2000) {
      dfToUse = df00
    } else if (input$yearRange == 2010) {
      dfToUse = df10
    } else { # if input$yearLeft == 2018
      dfToUse = df18
    }
    
    # Reset the map first to account for deselecting of checkboxes?
    leafletProxy(mapId="rangeLeaflet", data=dfToUse) %>%
      clearMarkers() 
    
    # if nothing is checked, then just clear the markers
    if (is.null(input$iconsRange)) {
      
    } else {
      # Get necessary data that was selected, Part I of event handling
      
      # Able to account for input$yearLeft and input$stateLeft in reactive values
      alldf <- allRa()
      coaldf <- coalRa()
      oildf <- oilRa()
      gasdf <- gasRa()
      nucleardf <- nuclearRa()
      hydrodf <- hydroRa()
      biomassdf <- biomassRa()
      winddf <- windRa()
      solardf <- solarRa()
      geothermaldf <- geothermalRa()
      otherdf <- otherRa()
      
      # Gets correct Generation range of each source
      #coaldf <- subset(coaldf, coaldf$COAL > input$minSlider & coaldf$COAL < input$maxSlider)
      #oildf <- subset(oildf, oildf$OIL > input$minSlider & oildf$OIL < input$maxSlider)
      #gasdf <- subset(gasdf, gasdf$GAS > input$minSlider & gasdf$GAS < input$maxSlider)
      #nucleardf <- subset(nucleardf, nucleardf$NUCLEAR > input$minSlider & nucleardf$NUCLEAR < input$maxSlider)
      #hydrodf <- subset(hydrodf, hydrodf$HYDRO > input$minSlider & hydrodf$HYDRO < input$maxSlider)
      #biomassdf <- subset(biomassdf, biomassdf$BIOMASS > input$minSlider & biomassdf$BIOMASS < input$maxSlider)
      #winddf <- subset(winddf, winddf$WIND > input$minSlider & winddf$WIND < input$maxSlider)
      #solardf <- subset(solardf, solardf$SOLAR > input$minSlider & solardf$SOLAR < input$maxSlider)
      #geothermaldf <- subset(geothermaldf, geothermaldf$GEOTHERMAL > input$minSlider & geothermaldf$GEOTHERMAL < input$maxSlider)
      #otherdf <- subset(otherdf, otherdf$OTHER > input$minSlider & otherdf$OTHER < input$maxSlider)
      
      sourceList <- c()
      
      # Try adding circle markers for each source one by one independently instead of aggregating
      # add reactive values for each energy source so we can add circle markers one by one
      # Part II of event handling
      if (!is.null(alldf)) {
        leafletProxy(mapId="rangeLeaflet", data=alldf) %>% 
          addCircleMarkers(data=alldf, (lng=alldf$LON+0.001), (lat=alldf$LAT-0.001), radius=7,
                           color='thistle1', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= None || ", alldf$NAME, "Generation Capacity= None", 
                                       " || Renewable= NA", alldf$PERCENT_R, " || Non-renewable= NA", alldf$PERCENT_NR)) 
      }
      
      if (!is.null(coaldf)) {
        print("lol")
        sourceList <- append(sourceList, "Coal")
        leafletProxy(mapId="rangeLeaflet", data=coaldf) %>% 
          addCircleMarkers(data=coaldf, lng=coaldf$LON, lat=coaldf$LAT, radius=(if(coaldf$COAL<=80000) {6} else if (coaldf$COAL<=150000) {8} else {10}),
                           color='black', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Coal || ", coaldf$NAME, "Generation Capacity= ",  coaldf$COAL,
                                       " || Renewable= ", coaldf$PERCENT_R, "% || Non-renewable= ", coaldf$PERCENT_NR, "%")) 
      } 
      
      if (!is.null(oildf)) {
        sourceList <- append(sourceList, "Oil")
        leafletProxy(mapId="rangeLeaflet", data=oildf) %>% 
          addCircleMarkers(data=oildf, lng=(oildf$LON+0.001), lat=oildf$LAT, radius=(if(oildf$OIL<=80000) {6} else if (oildf$OIL<=150000) {8} else {10}),
                           color='grey', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Oil || ", oildf$NAME, "Generation Capacity= ",  oildf$OIL,
                                       " || Renewable= ", oildf$PERCENT_R, "% || Non-renewable= ", oildf$PERCENT_NR, "%"))
      }
      
      if (!is.null(gasdf)) {
        sourceList <- append(sourceList, "Gas")
        leafletProxy(mapId="rangeLeaflet", data=gasdf) %>% 
          addCircleMarkers(data=gasdf, lng=(gasdf$LON-0.001), lat=gasdf$LAT, radius=(if(gasdf$GAS<=80000) {6} else if (gasdf$GAS<=150000) {8} else {10}),
                           color='midnightblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Gas || ", gasdf$NAME, "Generation Capacity= ",  gasdf$OIL,
                                       " || Renewable= ", gasdf$PERCENT_R, "% || Non-renewable= ", gasdf$PERCENT_NR, "%"))
      }
      
      if (!is.null(nucleardf)) {
        sourceList <- append(sourceList, "Nuclear")
        leafletProxy(mapId="rangeLeaflet", data=nucleardf) %>% 
          addCircleMarkers(data=nucleardf, lng=nucleardf$LON, (lat=nucleardf$LAT+0.001), radius=(if(nucleardf$NUCLEAR<=80000) {6} else if (nucleardf$NUCLEAR<=150000) {8} else {10}),
                           color='green', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Nuclear || ", nucleardf$NAME, "Generation Capacity= ",  nucleardf$NUCLEAR,
                                       " || Renewable= ", nucleardf$PERCENT_R, "% || Non-renewable= ", nucleardf$PERCENT_NR, "%")) 
      }
      
      if (!is.null(hydrodf)) {
        sourceList <- append(sourceList, "Hydro")
        leafletProxy(mapId="rangeLeaflet", data=hydrodf) %>% 
          addCircleMarkers(data=hydrodf, lng=hydrodf$LON, (lat=hydrodf$LAT-0.001), radius=(if(hydrodf$HYDRO<=80000) {6} else if (hydrodf$HYDRO<=150000) {8} else {10}),
                           color='deepskyblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Hydro || ", hydrodf$NAME, "Generation Capacity= ",  hydrodf$HYDRO,
                                       " || Renewable= ", hydrodf$PERCENT_R, "% || Non-renewable= ", hydrodf$PERCENT_NR, "%"))
      }
      
      if (!is.null(biomassdf)) {
        sourceList <- append(sourceList, "Biomass")
        leafletProxy(mapId="rangeLeaflet", data=biomassdf) %>% 
          addCircleMarkers(data=biomassdf, lng=(biomassdf$LON+0.002), lat=biomassdf$LAT, radius=(if(biomassdf$BIOMASS<=80000) {6} else if (biomassdf$BIOMASS<=150000) {8} else {10}),
                           color='red', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Biomass || ", biomassdf$NAME, "Generation Capacity= ",  biomassdf$BIOMASS,
                                       " || Renewable= ", biomassdf$PERCENT_R, "% || Non-renewable= ", biomassdf$PERCENT_NR, "%"))
      }
      
      if (!is.null(winddf)) {
        sourceList <- append(sourceList, "Wind")
        leafletProxy(mapId="rangeLeaflet", data=winddf) %>% 
          addCircleMarkers(data=winddf, (lng=winddf$LON-0.002), lat=winddf$LAT, radius=(if(winddf$WIND<=80000) {6} else if (winddf$WIND<=150000) {8} else {10}),
                           color='lightblue', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Wind || ", winddf$NAME, "Generation Capacity= ",  winddf$WIND,
                                       " || Renewable= ", winddf$PERCENT_R, "% || Non-renewable= ", winddf$PERCENT_NR, "%"))
      }
      
      if (!is.null(solardf)) {
        sourceList <- append(sourceList, "Solar")
        leafletProxy(mapId="rangeLeaflet", data=solardf) %>% 
          addCircleMarkers(data=solardf, lng=solardf$LON, (lat=solardf$LAT+0.002), radius=7,
                           color='gold', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Solar || ", solardf$NAME, "Generation Capacity= ",  solardf$SOLAR,
                                       " || Renewable= ", solardf$PERCENT_R, "% || Non-renewable= ", solardf$PERCENT_NR, "%"))
      }
      
      if (!is.null(geothermaldf)) {
        sourceList <- append(sourceList, "Geothermal")
        leafletProxy(mapId="rangeLeaflet", data=geothermaldf) %>% 
          addCircleMarkers(data=geothermaldf, lng=geothermaldf$LON, (lat=geothermaldf$LAT-0.002), radius=7,
                           color='brown', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Geothermal || ", geothermaldf$NAME, "Generation Capacity= ",  geothermaldf$GEOTHERMAL,
                                       " || Renewable= ", geothermaldf$PERCENT_R, "% || Non-renewable= ", geothermaldf$PERCENT_NR, "%"))
      }
      
      if (!is.null(otherdf)) {
        sourceList <- append(sourceList, "Other")
        leafletProxy(mapId="rangeLeaflet", data=otherdf) %>% 
          addCircleMarkers(data=otherdf, (lng=otherdf$LON+0.001), (lat=otherdf$LAT+0.001), radius=(if(otherdf$OTHER<=80000) {6} else if (otherdf$OTHER<=150000) {8} else {10}),
                           color='burlywood', stroke=FALSE, fillOpacity=1, 
                           label=paste("Source= Other || ", otherdf$NAME, "Generation Capacity= ",  otherdf$OTHER,
                                       " || Renewable= ", otherdf$PERCENT_R, "% || Non-renewable= ", otherdf$PERCENT_NR, "%"))
      }
      
    }
  })
  
  #===================================================================================================================================== 
  addLegendCustom <- function(map, colors, labels, sizes, opacity = 1){
    colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    
    return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
  }
  
  # Gets list of sources that have generation > 0 
  #getSources <- function(df) {
  # get a df from specific column names
  #df <- df[, names(df) %in% c("COAL", "OIL", "GAS", "NUCLEAR", "HYDRO", "BIOMASS", "WIND", "SOLAR", "GEOTHERMAL", "OTHER")]
  #sourceList <- apply(df[-1], 1, function(x) (names(which(x >0))))
  #return (sourceList)
  #}
  
}

# Run the application 
shinyApp(ui = ui, server = server)

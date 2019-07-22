# Multi-Window Shiny App: Population Dynamics
# By Hannah De los Santos
# Originated on: 6/26/16
# goal: illustrate population dynamics in us population in 2010

# load libraries ----

library(mwshiny)
library(censusapi)
library(ggplot2)
library(plotly)
library(magrittr)
library(censusapi)

key <- "" #PLEASE GET YOUR OWN US CENSUS API TOKEN
mapbox <- "" # PLEASE GET YOUR OWN MAPBOX TOKEN

Sys.setenv('MAPBOX_TOKEN' = mapbox)

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

census2010_codes <- read.csv("census_2010_codes.csv", stringsAsFactors = F)
naics07 <- read.csv("naics07.csv", stringsAsFactors = F)
us_area2010 <- read.csv("US_AREA_2010.csv", stringsAsFactors = F)
us_postal <- read.csv("US_POSTAL_STATES.csv", stringsAsFactors = F)

# create data frames of census data ----


# looking at data2010

data2010_county_counts <- getCensus(name = "dec/sf1",
                                    vintage = 2010,
                                    vars = c("NAME", census2010_codes$Code),
                                    key = key,
                                    region = "COUNTY") # OR STATE
data2010_county_counts[["st_ct"]] <- paste0(data2010_county_counts$state, data2010_county_counts$county)
data2010_county_counts$county <- as.numeric(data2010_county_counts$county)
data2010_county_counts$state <- as.numeric(data2010_county_counts$state)

data2010_county_perc <- data2010_county_counts

data2010_state_counts <- data2010_state_perc <- getCensus(name = "dec/sf1",
                                                          vintage = 2010,
                                                          vars = c("NAME", census2010_codes$Code),
                                                          key = key,
                                                          region = "STATE") # OR STATE
data2010_state_counts[["st_ct"]] <- paste0(data2010_state_counts$state,"000")
data2010_state_counts$state <- as.numeric(data2010_state_counts$state)
rownames(data2010_state_counts) <- data2010_state_counts$NAME
data2010_state_counts[us_postal$State, "postal_abbrev"] <- us_postal$Postal_Abbr
data2010_state_perc <- data2010_state_counts 

# converting to percentages
conv_to_perc <- function(data2010_state_perc){
  # population
  data2010_state_perc$P004001 <- data2010_state_perc$P004001/data2010_state_perc$P001001
  data2010_state_perc$P003002 <- data2010_state_perc$P003002/data2010_state_perc$P001001
  data2010_state_perc$P003003 <- data2010_state_perc$P003003/data2010_state_perc$P001001
  data2010_state_perc$P003004 <- data2010_state_perc$P003004/data2010_state_perc$P001001
  data2010_state_perc$P003005 <- data2010_state_perc$P003005/data2010_state_perc$P001001
  data2010_state_perc$P003006 <- data2010_state_perc$P003006/data2010_state_perc$P001001
  data2010_state_perc$P003007 <- data2010_state_perc$P003007/data2010_state_perc$P001001
  data2010_state_perc$P003008 <- data2010_state_perc$P003008/data2010_state_perc$P001001
  data2010_state_perc$P012001 <- data2010_state_perc$P012001/data2010_state_perc$P001001
  data2010_state_perc$P012026 <- data2010_state_perc$P012026/data2010_state_perc$P001001
  
  # population by area
  # sort by st_ct
  data2010_state_perc <- data2010_state_perc[order(as.numeric(data2010_state_perc$st_ct)),]
  # subset he area
  us_area2010 <- us_area2010[us_area2010$STCOU %in% as.numeric(data2010_state_perc$st_ct),]
  us_area2010 <- us_area2010[order(us_area2010$STCOU),]
  # subset to get rid of states we don't have area for
  data2010_state_perc <- data2010_state_perc[as.numeric(data2010_state_perc$st_ct) %in% as.numeric(us_area2010$STCOU),]
  
  data2010_state_perc$P001001 <- data2010_state_perc$P001001/us_area2010$Area_sqmi_2010
  
  # houses
  data2010_state_perc$PCT015002 <- data2010_state_perc$PCT015002/data2010_state_perc$H001001
  data2010_state_perc$PCT015013 <- data2010_state_perc$PCT015013/data2010_state_perc$H001001
  data2010_state_perc$H002002 <- data2010_state_perc$H002002/data2010_state_perc$H001001
  data2010_state_perc$H002005 <- data2010_state_perc$H002005/data2010_state_perc$H001001
  data2010_state_perc$H003002 <- data2010_state_perc$H003002/data2010_state_perc$H001001
  data2010_state_perc$H003003 <- data2010_state_perc$H003003/data2010_state_perc$H001001
  
  return(data2010_state_perc)
}

data2010_county_perc <- conv_to_perc(data2010_county_perc)
data2010_state_perc <- conv_to_perc(data2010_state_perc)

# get rid of the main things eliminated from the counts as well --puerto rico and the virgin islands
data2010_county_counts <- data2010_county_counts[data2010_county_counts$st_ct %in% data2010_county_perc$st_ct,]
data2010_state_counts <- data2010_state_counts[data2010_state_counts$st_ct %in% data2010_state_perc$st_ct,]

# add state names to county data
state_names <- data2010_state_counts$NAME
names(state_names) <- data2010_state_counts$state

data2010_county_counts[["ST_NAME"]] <- state_names[as.character(data2010_county_counts$state)]
data2010_county_perc[["ST_NAME"]] <- state_names[as.character(data2010_county_perc$state)]

# organize data ----

# so as to not write out all the choices for statistics chosen
# as well as specific states to focus on for density plots
stat_choi <- census2010_codes$Code
names(stat_choi) <- census2010_codes$Name

stat_chosen <- census2010_codes$Name
names(stat_chosen) <- census2010_codes$Code

foc_choi <- data2010_state_counts$state
names(foc_choi) <- data2010_state_counts$NAME

foc_chosen <- data2010_state_counts$NAME
names(foc_chosen) <- data2010_state_counts$state

# make a list for easy access of data frames
dat_list <- list("State_Count"=data2010_state_counts,
                 "State_Percentage/Ratio" = data2010_state_perc,
                 "County_Count" = data2010_county_counts,
                 "County_Percentage/Ratio" = data2010_county_perc)


# UI -----

# window titles
win_titles <- c("Controller", "Map")

# preallocating our user interfaces for each window
ui_list <- list()

# user interface for the controller, which also has overall population dynamics
ui_list[["Controller"]] <- fluidPage(
  titlePanel("Exploring Population Dynamics with mwshiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stat", "Which statistic would you like to visualize?",
                  choices = stat_choi),
      selectInput("type_stat", "What type of statistics would you like to see (if available)?",
                  choices = c("Count", "Percentage/Ratio")),
      selectInput("pop_level", "At which level would you like to explore?",
                  choices = c("State", "County")),
      selectInput("focus","Which states would you like to focus on (Aggrgate, State Level)?",
                  choices = foc_choi, multiple = T),
      selectInput("focus_map","Which state would you like to focus on (County Level)?",
                  choices = foc_choi),
      actionButton("go", "Visualize!")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Aggregate Dynamics",
                 plotOutput("overall_dens"),
                 plotOutput("state_dens")
                 ),
        tabPanel("Data Information")
      )
    )
  )
)

# user interface for map window, which has shows exact population dynamics
ui_list[["Map"]] <- fluidPage(
  tags$h6("Use click wheel to zoom in and out. Click and drag to pan."),
  plotlyOutput("map", height = "1000px"),
  tags$h6("Data from US Census 2010")
)

# CALC ----

serv_calc <- list()

serv_calc[[1]] <- function(calc, sess){
  observeEvent(calc$go, {
    # we're going to make the density plots first
    if (calc$pop_level == "State"){
      calc[["over_df"]] <- dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]
      
      calc[["state_df"]] <- dat_list[[paste0("County", "_", calc$type_stat)]]
      
      calc$state_df <- calc$state_df[calc$state_df$state %in% as.numeric(calc$focus),]
      
      
      calc[["over_titl"]] <- paste0("Overall ", stat_chosen[calc$stat], " (", calc$type_stat,")")
    } else {
      calc[["over_df"]] <- dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]
      calc$over_df <- calc$over_df[calc$over_df$state %in% as.numeric(calc$focus_map),]
      
      calc[["state_df"]] <- NULL
      
      calc[["over_titl"]] <- paste0("Overall ", stat_chosen[calc$stat], " for ", foc_chosen[calc$focus_map] ," (", calc$type_stat,")")
    }
    
    # now we're going to look at maps
    # redundant if statement, but serves to break up code more nicely
    if (calc$pop_level == "State"){
      all_states <- map_data("state")
      
      pop <- data.frame("stat" =  dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]][[calc$stat]],
                        "name" =  dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]$NAME,
                        "state" =  dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]$state,
                        "lowername" = tolower(dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]$NAME),
                        stringsAsFactors = F
      )
      
      merge_pop <- merge(all_states, pop, by.x = "region", by.y="lowername")
      
      
    } else { # county level
      state_interest <- tolower(foc_chosen[calc$focus_map])
      
      counties <- map_data("county") %>%
        filter(region %in% state_interest)
      
      pop <- data.frame("stat" =  dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]][[calc$stat]],
                        "name" =  dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]$NAME,
                        "state" =  dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]$state,
                        "county" =  dat_list[[paste0(calc$pop_level, "_", calc$type_stat)]]$county,
                        stringsAsFactors = F
      )
      
      county_names <- data.frame(do.call(rbind,strsplit(pop$name, ", ", fixed = T)),
                                 stringsAsFactors = F)
      county_names$X1 <- unlist(strsplit(county_names$X1, split=" Parish", fixed=TRUE))
      county_names$X1 <- unlist(strsplit(county_names$X1, split=" County", fixed=TRUE))
      county_names$X1 <- gsub( ".", "", county_names$X1,fixed = T)
      pop$county_names <- tolower(county_names$X1)
      pop$state_names <- tolower(county_names$X2)
      if (state_interest=="alabama"){
        pop$county_names[pop$county_names == "dekalb"] <- "de kalb"
      }
      pop <- pop[pop$state_names %in% state_interest,]
      
      merge_pop <- merge(counties, pop, by.x = "subregion", by.y = "county_names")
    }
    
    scl <- (max(merge_pop$stat) - min(merge_pop$stat))/10
    scl_long <- c(seq(max(min(merge_pop$stat)-scl, 0), max(merge_pop$stat)+scl, by = scl))
    
    scl_names <- c()
    for (i in 2:length(scl_long)){ 
      scl_names[length(scl_names)+1] <- paste0("[",scl_long[i-1],",",scl_long[i],")")
    }
    
    merge_pop$pop_cat <- cut(merge_pop$stat, 
                             breaks = scl_long,
                             labels=scl_names)
    
    merge_pop$legnd <- scl_names[merge_pop$pop_cat]
    
    calc[["merge_pop"]] <- merge_pop
    
    # just so as to not trigger everything based on input
    calc[["stat_obs"]] <- calc$stat
    calc[["type_obs"]] <- calc$type_stat
  })
}

# OUTPUT ----

serv_out <- list()

serv_out[["overall_dens"]] <- function(calc, sess){
  renderPlot({
    if (!is.null(calc$over_df)){
      ggplot(calc$over_df, aes_string(calc$stat_obs))+
        geom_density()+
        ggtitle(calc$over_titl)+
        labs(caption = "Data from US Census 2010")+
        xlab(stat_chosen[calc$stat_obs])+
        NULL
    } else {
      ggplot()
    }
  })
}

serv_out[["state_dens"]] <- function(calc, sess){
  renderPlot({
    if (!is.null(calc$state_df)){
      ggplot(calc$state_df, aes_string(calc$stat_obs, fill = "ST_NAME", colour = "ST_NAME"))+
        geom_density(alpha = .5)+
        ggtitle(paste0(stat_chosen[calc$stat_obs]," for Selected States", " (", calc$type_obs,")"))+
        labs(caption = "Data from US Census 2010",
             subtitle = "Based on county data for each state.",
             colour = "State",
             fill = "State")+
        theme(legend.position="bottom")+
        xlab(stat_chosen[calc$stat_obs])+
        NULL
    } else {
      ggplot()
    }
  })
}

serv_out[["map"]] <- function(calc, sess){
  renderPlotly({
    if (!is.null(calc$merge_pop)){
      calc$merge_pop %>%
        group_by(group) %>%
        plot_mapbox(x = ~long, y = ~lat,
                    color = ~pop_cat,
                    colors = c('#ffeda0','#f03b20'),
                    text = ~paste0("Name:", name, "\n", stat_chosen[calc$stat_obs],": ", stat),
                    hoverinfo = "text") %>%
        add_polygons(line = list(width = 0.4))  %>%
        layout(
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
          mapbox = list(
            style = 'light',
            zoom = 4,
            center = list(lat = ~median(lat), lon = ~median(long))),
          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
          showlegend = T
        )
    }
  })
}

# RUN ----

depend <- list()
depend[["htmlwidgets"]] <- c("www/htmlwidgets.js")
depend[["shiny"]] <- c("www/shared/selectize/js/selectize.min.js",
                       "www/shared/selectize/css/selectize.bootstrap3.css")
depend[["plotly"]] <- c("htmlwidgets/lib/plotlyjs/plotly-latest.min.js",
                        "htmlwidgets/lib/plotlyjs/plotly-htmlwidgets.css",
                        "htmlwidgets/plotly.js",
                        "htmlwidgets/lib/typedarray/typedarray.min.js")
depend[["crosstalk"]] <- c("www/css/crosstalk.css",
                           "www/js/crosstalk.min.js")

mwsApp(win_titles, ui_list, serv_calc, serv_out, depend)

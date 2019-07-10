# Making a Multi-Window Shiny App for ~Art~
# By Hannah De los Santos
# Originated on: 7/4/19

# load libraries and data, state functions ----

library(mwshiny)
library(plotly)
library(magrittr)

#https://stackoverflow.com/questions/3452086/getting-path-of-an-r-script/35842176#35842176
# set working directory - only works in RStudio (with rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

mapbox <- "" # IF RUNNING THIS, PLEASE GET YOUR OWN MAPBOX TOKEN
Sys.setenv('MAPBOX_TOKEN' = mapbox)

artist_info <- read.csv("artist_info.csv", stringsAsFactors = F)

paint_content <- function(num, calc){
  paste0(paste0("<center><img src='",artist_info[calc$art_person==artist_info$Name, paste0("Painting.",num,".Link")],"' style='width:400px'></center>"),
         "<br>",
         h2(paste0(artist_info[calc$art_person==artist_info$Name, paste0("Painting.",num,".Title")],
                   " (", artist_info[calc$art_person==artist_info$Name, paste0("Painting.",num,".Year")], ")"), align = "center"),
         tags$br(),
         h6(HTML(artist_info[calc$art_person==artist_info$Name, paste0("Painting.",num,".Attribution")]), align = "right")
  )
}

# UI ----

win_titles <- c("Controller","Art_Monitor")

ui_list <- list()

ui_list[["Controller"]] <- fluidPage(
  titlePanel("Learn about some of your favorite artists!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("art_wait", "Which artist would you like to learn about?",
                  choices = artist_info$Name),
      radioButtons("which_tab", "What representative picture would you like to see?",
                  choices = c("Artist" = "artiste",
                              "Birthplace" = "birthplace",
                              "Painting 1" = "painting_1",
                              "Painting 2" = "painting_2",
                              "Painting 3" = "painting_3"
                              )),
      actionButton("go", "Learn!")
    ),
    mainPanel()
  )
)

ui_list[["Art_Monitor"]] <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Artist Information"),
      htmlOutput("info"),
      h6("Information from Wikipedia.", align = "right"),
      width = 5
    ),
    mainPanel(
      tabsetPanel(id = "art",
                  tabPanel("Artist", value = "artiste", 
                           htmlOutput("artist")),
                  tabPanel("Birthplace", value = "birthplace",
                           # "hey"),
                           plotlyOutput("map", height = "800px")),
                  tabPanel("Painting 1", value = "painting_1", 
                           # "hey"),
                           htmlOutput("painting_1")),
                  tabPanel("Painting 2", value = "painting_2", 
                           # "hey"),
                           htmlOutput("painting_2")),
                  tabPanel("Painting 3", value = "painting_3",
                           # "hey")
                           htmlOutput("painting_3"))
                  ),
      width = 7
    )
  )
)

# CALC ----

serv_calc <- list()

# first we do the calculation for input switching
serv_calc[[1]] <- function(calc, sess){
  observeEvent(calc$go, {
    calc[["select_tab"]] <- calc$which_tab
    
    calc[["art_person"]] <- calc$art_wait
    calc[["art_born"]] <- artist_info[artist_info$Name==calc$art_person,]
    
    updateTabsetPanel(sess, "art",
                      selected = calc$select_tab)
  })
}


# OUTPUT ----

serv_out <- list()

serv_out[["info"]] <- function(calc, sess){
  renderText({
    paste0(
      "<b>Artist:</b> ", calc$art_person, "<br>",
      "<b>Born:</b> ", artist_info$Born.Day[calc$art_person==artist_info$Name],
      " in ", artist_info$Born.Place[calc$art_person==artist_info$Name], "<br>",
      "<b>Died:</b> ", artist_info$Died.Day[calc$art_person==artist_info$Name],
      " (Age ", artist_info$Age[calc$art_person==artist_info$Name], ")<br>",
      "<b>Movement:</b> ", artist_info$Movement[calc$art_person==artist_info$Name], "<br>",
      "<b>Nationality:</b> ", artist_info$Nationality[calc$art_person==artist_info$Name], "<br>",
      "<b>Summary:</b> ", artist_info$Small.Summary[calc$art_person==artist_info$Name]
    )
  })
}

serv_out[["artist"]] <- function(calc, sess){
  renderText({
    paste0(paste0("<center><img src='",artist_info$Artist.Picture[calc$art_person==artist_info$Name],"' style='width:400px'></center>"),
           "<br>",
           h2(calc$art_person, align = "center"),
           tags$br(),
           h6(HTML(artist_info$Artist.Picutre.Attribution[calc$art_person==artist_info$Name]), align = "right"))
  })
}

serv_out[["painting_1"]] <- function(calc, sess){
  renderText({
    paint_content(1, calc)
  })
}

serv_out[["painting_2"]] <- function(calc, sess){
  renderText({
    paint_content(2, calc)
  })
}

serv_out[["painting_3"]] <- function(calc, sess){
  renderText({
    paint_content(3, calc)
  })
}

serv_out[["map"]] <- function(calc, sess){
  renderPlotly({
    if (!is.null(calc$art_born)){
      calc$art_born %>%
        plot_mapbox(x = ~Longitude.Born, 
                    y = ~Latitude.Born) %>%
        add_markers(
          text = ~paste(" Born in:",Born.Place),
          hoverinfo = "text",
          color = ~Born.Place,
          symbol = I("circle"),
          size= I(100)) %>%
        layout(
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
          mapbox = list(
            style = 'light',
            zoom = 4,
            center = list(lat = ~Latitude.Born,
                          lon = ~Longitude.Born)),
          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
          showlegend = T
        )
    }
  })
}

# DEPENDS ----

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

# RUN ----

mwsApp(win_titles, ui_list, serv_calc, serv_out, depend)
suppressMessages(library(shiny))
suppressMessages(library(DBI))
suppressMessages(library(odbc))
suppressMessages(library(RPostgreSQL))
suppressMessages(library(pool))
suppressMessages(library(rtweet))
suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(rsconnect))
suppressMessages(library(lubridate))
suppressMessages(library(scales))
suppressMessages(library(DT))

onStop(function() { poolClose(monitor_db) })

####################################################################################################

ui <- fluidPage(tags$head(includeHTML("google-analytics.html")),

  navbarPage(title = "Beta Version (0.5)",
             theme = "custom.css",

             tabPanel(tags$div(icon("fire", class = "icons")," Trends"),
                      mod_trends_ui("trends")),
             tabPanel(tags$div(icon("chart-line", class = "icons")," Term popularity"),
                      mod_popularity_ui("popularity")),
             tabPanel(tags$div(icon("table", class = "icons")," Tweets"),
                      mod_tweets_ui("tweets")),
             tabPanel(tags$div(icon("user", class = "icons"), " Profiles"),
                      mod_profile_ui("profiles"))
  )
)

####################################################################################################

server <- function(input, output, session){

  ##################################
  #CONNECT TO DATABASE
  monitor_db <- dbPool(drv = "PostgreSQL", dbname = "", host = "", port = , user = "", password = "")


  ##################################

  mod_trends_server("trends", monitor_db)
  mod_popularity_server("popularity", monitor_db)
  mod_tweets_server("tweets", monitor_db)
  mod_profile_server("profiles", monitor_db)

}

####################################################################################################

shinyApp(ui = ui, server = server)

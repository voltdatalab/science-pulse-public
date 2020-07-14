#############################
#############################
###
### MODULE - TRENDS TAB

library(stringr)
library(stringi)
library(cld2)
library(tidytext)

###################################################################################################

### SERVER

mod_trends_server <- function(id, base) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # Load data
      sql_t <- "SELECT * from base 
      ORDER BY created_at DESC
      LIMIT 20000"
      query_t <- sqlInterpolate(base, sql_t)
      trends <- as_tibble(dbGetQuery(base, query_t))
      
      # Create current time column
      trends$current_time <- lubridate::now()
      trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
      trends$created_at <- trends$created_at - hours(3)
      
      # Identify tweets texts' language
      trends$language <- detect_language(trends$text)
      
      #  Filter last 24h
      trends_24h <- filter(trends, created_at > current_time - hours(12))
      
      ###  Clean text variable, but preserving original text to future use
      trends_24h$original_text <- trends_24h$text 
      
      trends_24h$text <- tolower(stri_trans_general(trends_24h$text, "Latin-ASCII")) # clean accents and transform text to lowercase
      trends_24h$text <- gsub('http\\S+\\s*',"", trends_24h$text) # remove links
      trends_24h$text <- gsub("(^|[^@\\w])@(\\w{1,15})\\b","", trends_24h$text) # remove mentions
      trends_24h$text <- gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+", "", trends_24h$text) # remove hashtags
      trends_24h$text <- tolower(stri_trans_general(trends_24h$text, "Latin-ASCII")) # limpa acentos e coloca em minusculas\
      
      ##########
      ### FIVE MOST RTS OF THE DAY
      
      retweets <- trends_24h
      
      # Among RT tweets in the sample, 
      # which ones had more RTs considering total RT count (retweet_count column)
      output$rts24h_overall <- renderTable({
        
        retweets %>% 
          mutate(original_text = paste0('<blockquote class="twitter-tweet"><p lang="', language, '" dir="ltr">', original_text, '</p>&mdash;', name, '(@', screen_name, ') <a href="https://twitter.com/', screen_name, '/status/', status_id, '">', created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
          # mutate(original_text = paste0("<br>",original_text, "<a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>", "<hr><span style='font-family: Roboto Mono, monospace;font-size:0.8em'><i class='far fa-user'></i>", " <a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'>@<strong style='color: #d91c5c'>", retweet_screen_name, "</strong></a> ", "<br><i class='far fa-clock'></i> ", created_at, "</span>")) %>%
          filter(language == input$langRTtrend,
                 is_retweet == T) %>%
          group_by(language) %>%
          arrange(desc(retweet_count)) %>%
          distinct(original_text) %>%
          slice(1:5) %>%
          mutate(original_text = paste0("<strong>", 1:n(), "º //</strong>", original_text)) %>%
          rename("Other popular tweets <i class='fas fa-random'></i>" = original_text) %>%
          ungroup()
        
      }, 
      
      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")
      
      # Among RT tweets in the sample, 
      # which ones had more RTs considering number of times it was RT in the sample
      output$rts24h_sample <- renderTable({
        
        retweets %>% 
          mutate(original_text = paste0('<blockquote class="twitter-tweet"><p lang="', language, '" dir="ltr">', original_text, '</p>&mdash;', name, '(@', screen_name, ') <a href="https://twitter.com/', screen_name, '/status/', status_id, '">', created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
          # mutate(original_text = paste0(" <em style='font-size:0.9em'><i class='far fa-clock'></i>", created_at, " </em><br><a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'>@<strong style='color: #d91c5c'>", retweet_screen_name, "</strong></a>: ", original_text, "<a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>")) %>%
          filter(language == input$langRTtrend,
                 is_retweet == T) %>%
          group_by(language) %>%
          count(original_text) %>%
          arrange(desc(n)) %>%
          select(original_text) %>%
          slice(1:5) %>%
          ungroup() %>%
          mutate(original_text = paste0("<strong>", 1:n(), "º //</strong> ", original_text)) %>%
          rename("Popular among scientists <i class='fas fa-retweet'>" = original_text) 
      },  
      
      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")
      
      # Among tweets in the sample, 
      # which ones were more tweeted considering number of times it appeared in the sample
      output$own_sample <- renderTable({
        
        retweets %>% 
          filter(is_retweet == F) %>%
          mutate(original_text = paste0('<blockquote class="twitter-tweet"><p lang="', language, '" dir="ltr">', original_text, '</p>&mdash;', name, '(@', screen_name, ') <a href="https://twitter.com/', screen_name, '/status/', status_id, '">', created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
          #mutate(original_text = paste0(" <em style='font-size:0.9em'><i class='far fa-clock'></i>", created_at, " </em><br><a href='https://twitter.com/", screen_name,"/status/",status_id,"' target='_blank'>@<strong style='color: #d91c5c'>", screen_name, "</strong></a>: ", original_text, "<a href='https://twitter.com/", screen_name,"/status/",status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>")) %>%
          filter(language == input$langRTtrend) %>%
          group_by(language) %>%
          count(original_text) %>%
          arrange(desc(n)) %>%
          select(original_text) %>%
          slice(1:5) %>%
          ungroup() %>%
          mutate(original_text = paste0("<strong>", 1:n(), "º //</strong> ", original_text)) %>%
          rename("Popular within Pulse <i class='fas fa-fire'></i>" = original_text) 
      },  sanitize.text.function = function(x) x, striped = FALSE, target="_blank")
      
    }
    
  )
  
}

###################################################################################################

### UI

mod_trends_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    tags$div(class = "", style = "text-align:center;margin: 12px auto !important;max-width:360px !important",
             tags$h1("POPULAR TWEETS"),
             tags$p("The tables below show popular tweets among experts and scientists in Pulse's database in the last 12h. Each table has different metrics designed to improve the discovery of content.",
                    selectInput(inputId = ns("langRTtrend"),
                                label = tags$div(tags$p(style="font-weight:300", icon("language", class = "icons"), "choose language")),
                                c("English" = "en",
                                  "Español" = "es",
                                  "Português" = "pt")
                    )
             )),
    tags$div(class = "container-fluid", style = "text-align:center",
             column(4,
                    tags$p("This table shows the trending tweets from scientists within Pulse database, and does no include RTs from other users. They are often about science."),
                    tableOutput(ns("own_sample"))),
             column(4, 
                    tags$p("This table shows popular tweets trending among scientists, including retweets from profiles outside Pulse. They are often about science."),
                    tableOutput(ns("rts24h_sample"))),
             column(4, 
                    tags$p("This table shows very popular tweets anywhere on Twitter shared at least once by scientists within Pulse. They are usually not about science."),
                    tableOutput(ns("rts24h_overall"))))
    
  )
  
}


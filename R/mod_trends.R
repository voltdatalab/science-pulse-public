###################################################################################################
###################################################################################################
###################
###################                    MODULE - TRENDS TAB

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

      trends_dataset <- reactive({

      # Load data
      sql_t <- "SELECT created_at,text,is_retweet,language,name,screen_name,status_id,retweet_count,retweet_name,retweet_screen_name,retweet_status_id,retweet_created_at from base
      ORDER BY created_at DESC
      LIMIT 20000"
      query_t <- sqlInterpolate(base, sql_t)
      trends <- as_tibble(dbGetQuery(base, query_t))

      trends <- trends %>%
        filter(language == input$langRTtrend)

      # Create current time column
      trends$current_time <- lubridate::now()
      trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
      trends$created_at <- trends$created_at - hours(3)

      ### Clean text variable to better detect language,
      #   but preserving original text to future use
      trends$text <- trends$text

      #  Filter last 12h
      trends_12h <- filter(trends, created_at > current_time - hours(12))

      })

      ##################################################
      ##### FIVE MOST RTS OF THE DAY

      ### POPULAR WITHIN PULSE

      # Among tweets in the sample (posted in the sample), which ones were more RT (by the whole universe of twitter users)?
      # Count includes RTs by users outside Pulse. However, original tweets were posted only by members.

      output$own_sample <- renderTable({

        trends_dataset() %>%
          filter(is_retweet == F) %>%
          mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                                        language, '" dir="ltr">',
                                        text, '</p>&mdash;',
                                        name, '(@',
                                        screen_name, ') <a href="https://twitter.com/',
                                        screen_name, '/status/',
                                        status_id, '">',
                                        created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
          #mutate(text = paste0(" <em style='font-size:0.9em'><i class='far fa-clock'></i>", created_at, " </em><br><a href='https://twitter.com/", screen_name,"/status/",status_id,"' target='_blank'>@<strong style='color: #d91c5c'>", screen_name, "</strong></a>: ", text, "<a href='https://twitter.com/", screen_name,"/status/",status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>")) %>%
          group_by(language) %>%
          arrange(desc(as.numeric(retweet_count))) %>%
          #distinct(text) %>%
          slice(1:5) %>%
          select(language, text) %>%
          ungroup() %>%
          mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
          rename("Popular within Pulse <i class='fas fa-fire'></i>" = text)

      },

      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ### POPULAR AMONG SCIENTISTS

      # Among RT tweets in the sample (those retweeted by Pulse's members), which ones had more RTs considering number of times it was RTed
      # *only* by sample members. They include tweets from any Twitter account. However, they are displayed according to the number of times
      # the original tweet appeared in our sample (since each RT is included as one row in the dataset).

      output$rts12h_sample <- renderTable({

        trends_dataset() %>%
          mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                                        language, '" dir="ltr">',
                                        text, '</p>&mdash;',
                                        retweet_name, '(@',
                                        retweet_screen_name, ') <a href="https://twitter.com/',
                                        retweet_screen_name, '/status/',
                                        retweet_status_id, '">',
                                        retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
          # mutate(text = paste0(" <em style='font-size:0.9em'><i class='far fa-clock'></i>", created_at, " </em><br><a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'>@<strong style='color: #d91c5c'>", retweet_screen_name, "</strong></a>: ", text, "<a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>")) %>%
          filter(#language == input$langRTtrend,
                 is_retweet == T) %>%
          group_by(language) %>%
          count(text) %>%
          arrange(desc(n)) %>%
          select(language, text) %>%
          slice(1:5) %>%
          mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
          rename("Popular among scientists <i class='fas fa-retweet'>" = text) %>%
          ungroup()

      },

      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ### OTHER POPULAR TWEETS

      # Among RT tweets in the sample (those retweeted by Pulse's members), which ones had more RTs considering all RT (retweet_count) at the moment
      # data was last extracted from Twitter's API. Generally, they are related to comedy or politics.

      output$rts12h_overall <- renderTable({

        trends_dataset() %>%
          mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                                        language, '" dir="ltr">',
                                        text, '</p>&mdash;',
                                        retweet_name, '(@',
                                        retweet_screen_name, ') <a href="https://twitter.com/',
                                        retweet_screen_name, '/status/',
                                        retweet_status_id, '">',
                                        retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
          # mutate(text = paste0("<br>",text, "<a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>", "<hr><span style='font-family: Roboto Mono, monospace;font-size:0.8em'><i class='far fa-user'></i>", " <a href='https://twitter.com/", retweet_screen_name,"/status/",retweet_status_id,"' target='_blank'>@<strong style='color: #d91c5c'>", retweet_screen_name, "</strong></a> ", "<br><i class='far fa-clock'></i> ", created_at, "</span>")) %>%
          filter(#language == input$langRTtrend,
                 is_retweet == T) %>%
          group_by(language) %>%
          arrange(desc(as.numeric(retweet_count))) %>%
          distinct(text) %>%
          slice(1:5) %>%
          mutate(text = paste0("<strong>", 1:n(), "º //</strong>", text)) %>%
          select(language, text) %>%
          rename("Other popular tweets <i class='fas fa-random'></i>" = text) %>%
          ungroup()

      },

      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

    }

  )

}

###################################################################################################

### UI

mod_trends_ui <- function(id){

  ns <- NS(id)

  tagList(

    # Page title and description
    tags$div(class = "sheet_topper",
    tags$div(class = "sheet_header", style = "",
             tags$h1("POPULAR TWEETS", style = "text-align:center"),
             tags$p("The tables below show popular tweets in the last 12h among experts, scientists and scientific organizations curated by Science Pulse. Each table uses different metrics designed to improve the discovery of content. You can", tags$a("click here", href="https://sciencepulse.org/methodology", target="_blank") , "to read the methodology.", style = "font-family: 'Roboto Mono', monospace"),
             tags$br(),
                    selectInput(inputId = ns("langRTtrend"),
                                label = tags$div(tags$p(style="font-weight:300", icon("language", class = "icons"), "choose language")),
                                c("English" = "en",
                                  "Español" = "es",
                                  "Português" = "pt")
                    )
             )),

    tags$div(class = "container-fluid", style = "text-align:center",
             column(4,
                    tags$p("This table shows the trending tweets from scientists within Pulse database, and does not include RTs from other users. They are often about science."),
                    withSpinner(tableOutput(ns("own_sample")),
                                type = getOption("spinner.type", default = 6),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("own_sample")))) NULL else "300px")
                    ),
             column(4,
                    tags$p("This table shows popular tweets trending among scientists, including retweets from profiles outside Pulse. They are often about science."),
                    withSpinner(tableOutput(ns("rts12h_sample")),
                                type = getOption("spinner.type", default = 6),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("rts12h_sample")))) NULL else "300px")
                    ),
             column(4,
                    tags$p("This table shows very popular tweets anywhere on Twitter shared at least once by scientists within Pulse. They are usually not about science."),
                    withSpinner(tableOutput(ns("rts12h_overall")),
                                type = getOption("spinner.type", default = 6),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("rts12h_overall")))) NULL else "300px"
                    )
                    ))

  )

}


###################################################################################################
###################################################################################################
###################
###################                    MODULE - EXPLORE TAB

###################################################################################################

### SERVER

mod_explore_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      trends_dataset <- reactive({

      # Load data
      sql_t <- "SELECT created_at,text,language,screen_name,hashtags,is_retweet,name,status_id,retweet_count from base
      ORDER BY created_at DESC
      LIMIT 20000"
      query_t <- sqlInterpolate(base, sql_t)
      trends <- as_tibble(dbGetQuery(base, query_t))

      trends <- trends %>%
        filter(language == input$language)

      # Create current time column
      trends$current_time <- lubridate::now()
      trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
      trends$created_at <- trends$created_at - hours(3)

      #  Filter last 12h
      trends_12h <- filter(trends, created_at > current_time - hours(12))

      })

      ##################################################
      ##### EXPLORE

      ### MOST ACTIVE USERS

      # Users in our sample that posted the highest number of tweets on the last 12h.

      output$active_users <- renderTable({

        trends_dataset() %>%
          #filter(language == input$language) %>%
          group_by(language, screen_name) %>%
          count(sort = T) %>%
          ungroup() %>%
          slice(1:5) %>%
          select(language, screen_name) %>%
          mutate(screen_name = paste0("@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #d91c5c'>", screen_name, "</a>")) %>%
          rename("Active users <i class='fas fa-users'></i>" = screen_name)

      },

      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ### MOST TWEETED HASHTAGS

      output$hashtags <- renderTable({

        # Extract most shared hashtags from last 12h tweets
        hashtags <- trends_dataset() %>%
          #filter(language == input$language) %>%
          .$hashtags
        lista_hashtags <- unlist(str_split(hashtags, ","))
        lista_hashtags <- tibble(hashtag = str_trim(tolower(stri_trans_general(lista_hashtags, "Latin-ASCII")), "both"))

        lista_hashtags <- lista_hashtags %>%
          filter(hashtag != "na") %>%
          mutate(hashtag = toupper(hashtag)) %>%
          mutate(hashtag = paste0("#<a href='https://twitter.com/hashtag/", hashtag, "' target='_blank' style='color: #d91c5c'>", hashtag, "</a>")) %>%
          count(hashtag, sort = T) %>%
          slice(1:5) %>%
          select(n, hashtag) %>%
          rename("Hashtags <i class='fas fa-hashtag'></i>" = hashtag)

      },

      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ### ALSO POPULAR WITHIN PULSE

      # Among tweets in the sample (posted in the sample), which ones were more RT (by the whole universe of twitter users)?
      # Count includes RTs by users outside Pulse. However, original tweets were posted only by members.

      output$own_sample <- renderTable({

        set.seed(12345)

        own_sample_trends <- trends_dataset() %>%
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
        #  filter(language == input$language) %>%
          group_by(language) %>%
          arrange(desc(as.numeric(retweet_count))) %>%
          ungroup()

        # use kmeans a first time to get the centers
        centers <- kmeans(as.numeric(own_sample_trends$retweet_count), centers = 4, nstart = 1000)$centers
        # order the centers
        centers <- sort(centers)
        # call kmeans again but this time passing the centers calculated in the previous step
        own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count), centers = centers)$cluster

        own_sample_trends %>%
          filter(cluster == 2) %>%
          slice(1:5) %>%
          select(language, text) %>%
          ungroup() %>%
          mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
          rename("Also popular within Pulse <i class='fas fa-angle-double-up'></i>" = text)

      },

      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ### HIDDEN GEMS - ON YOUR RADAR?

      output$hidden_gems <- renderTable({

        set.seed(12345)

        own_sample_trends <- trends_dataset() %>%
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
         # filter(language == input$language) %>%
          group_by(language) %>%
          arrange(desc(as.numeric(retweet_count))) %>%
          ungroup()

        # use kmeans a first time to get the centers
        centers <- kmeans(as.numeric(own_sample_trends$retweet_count), centers = 4, nstart = 1000)$centers
        # order the centers
        centers <- sort(centers)
        # call kmeans again but this time passing the centers calculated in the previous step
        own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count), centers = centers)$cluster

        own_sample_trends %>%
          filter(cluster == 2) %>%
          sample_n(5) %>%
          select(language, text) %>%
          ungroup() %>%
          mutate(text = paste0("<strong>//</strong> ", text)) %>%
          rename("Pulse radar <i class='fas fa-dot-circle'></i>" = text)

      },

      sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      }

  )

}

###################################################################################################

### UI

mod_explore_ui <- function(id){

  ns <- NS(id)

  tagList(

    # Page title and description
    tags$div(class = "sheet_topper",
             tags$div(class = "sheet_header", style = "",
                      tags$h1("DIG DEEPER", style = "text-align:center"),
                      tags$p("This section reveals further information from our datasets. See other trending tweets and some hidden content from profiles that don't necessarily have many followers or engagement in their posts. You can", tags$a("click here", href="https://sciencepulse.org/methodology", target="_blank") , "to read the methodology.", style = "font-family: 'Roboto Mono', monospace"),
                      tags$br(),
                      # Language input
                      selectInput(inputId = ns("language"),
                                  label = tags$div(tags$p(style="font-weight:300", icon("language", class = "icons"), "choose language")),
                                  c("English" = "en",
                                    "Español" = "es",
                                    "Português" = "pt"))
             )),

    # Page layout
    tags$div(class = "container-fluid", style = "text-align:center",

    column(2,

           #tags$br(),

           # Left-tables description
           tags$p("Most active users and hashtags in the last 12h."),

           # Active users table
           withSpinner(tableOutput(ns("active_users")),
                       type = getOption("spinner.type", default = 1),
                       color = getOption("spinner.color", default = "#d91c5c"),
                       size = getOption("spinner.size", default = 1),
                       color.background = getOption("spinner.color.background", default = "#d91c5c"),
                       custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("rts12h_sample")))) NULL else "300px"),
           tags$br(),

           # Hashtags table
           withSpinner(tableOutput(ns("hashtags")),
                       type = getOption("spinner.type", default = 1),
                       color = getOption("spinner.color", default = "#d91c5c"),
                       size = getOption("spinner.size", default = 1),
                       color.background = getOption("spinner.color.background", default = "#d91c5c"),
                       custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("rts12h_sample")))) NULL else "300px")

           ),

    column(1),

    # Also popular within pulse
    column(4,
           tags$p("Somewhat popular tweets in Science Pulse that did not make into the trends."),
           withSpinner(tableOutput(ns("own_sample")),
                       type = getOption("spinner.type", default = 6),
                       color = getOption("spinner.color", default = "#d91c5c"),
                       size = getOption("spinner.size", default = 1),
                       color.background = getOption("spinner.color.background", default = "#d91c5c"),
                       custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("own_sample")))) NULL else "300px")
    ),

    #column(1),

    # Hidden gems?
    column(4,
           tags$p("Random sample of other popular (but usually not trending) tweets."),
           withSpinner(tableOutput(ns("hidden_gems")),
                       type = getOption("spinner.type", default = 6),
                       color = getOption("spinner.color", default = "#d91c5c"),
                       size = getOption("spinner.size", default = 1),
                       color.background = getOption("spinner.color.background", default = "#d91c5c"),
                       custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("hidden_gems")))) NULL else "300px")
           )

    # Close tags$div
    )

  # Close tagList
  )

}

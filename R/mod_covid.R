###################################################################################################
###################################################################################################
###################
###################                    MODULE - COVID TAB

###################################################################################################

### SERVER

mod_covid_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      trends_dataset <- reactive({

      # Load data
      sql_t <- "SELECT created_at,text,language,screen_name,hashtags,name,status_id,retweet_count,retweet_name,retweet_screen_name,retweet_status_id,retweet_created_at,is_retweet from base
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

      # Select keywords
      keywords <- c("Covid", "covid", "Coronavirus", "coronavirus",
                    "Corona", "corona", "SARS-CoV-2", "Sars-CoV-2",
                    "SRAG", "sindrome", "syndrome", "pandemic",
                    "pandemia", "WHO", "OMS", "quarantine", "social distancing",
                    "quarentena", "isolamento social", "distanciamento social",
                    "mascara", "mask", "distanciamiento social", "spread", "asymptomatic",
                    "epidemic", "outbreak", "epidemia", "vacina", "vaccine", "wuhan", "Wuhan")

      # Filter tweets that contain those keywords
      trends_12h <- filter(trends_12h, str_detect(text, paste(keywords, collapse = "|")))

      })

      ##################################################
      ##### COVID-19 TRENDS

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

      sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ### MOST TWEETED HASHTAGS

      output$hashtags <- renderTable({

        exclude_hashtags <- c("coronavirus", "coronaviruses", "covid", "covid__19",
                              "covid_19", "covidー19", "covid19", "covid19brasil",
                              "covid19ma", "covid2019", "covid9", "pandemia",
                              "pandemias", "pandemic", "pandemic2020",
                              "sars_cov_2", "sarscov2")

        # Extract most shared hashtags from last 12h tweets
        hashtags <- trends_dataset() %>%
          #filter(language == input$language) %>%
          .$hashtags
        lista_hashtags <- unlist(str_split(hashtags, ","))
        lista_hashtags <- tibble(hashtag = str_trim(tolower(stri_trans_general(lista_hashtags, "Latin-ASCII")), "both")) %>%
          filter(!hashtag %in% exclude_hashtags)

        lista_hashtags <- lista_hashtags %>%
          filter(hashtag != "na") %>%
          mutate(hashtag = toupper(hashtag)) %>%
          mutate(hashtag = paste0("#<a href='https://twitter.com/hashtag/", hashtag, "' target='_blank' style='color: #d91c5c'>", hashtag, "</a>")) %>%
          count(hashtag, sort = T) %>%
          slice(1:5) %>%
          select(n, hashtag) %>%
          rename("Hashtags <i class='fas fa-hashtag'></i>" = hashtag)

      },

      sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

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
          #filter(language == input$language) %>%
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
          filter(#language == input$language,
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

    }

  )

}

###################################################################################################

### UI

mod_covid_ui <- function(id){

  ns <- NS(id)

  tagList(

    # Page title and description
    tags$div(class = "sheet_topper",
             tags$div(class = "sheet_header", style = "",
                      tags$h1("COVID-19 ESPECIAL", style = "text-align:center"),
                      tags$p("Tables in this tab show trends on tweets, hashtags and active users regarding Covid-19 keywords topics in the last 12h. You can", tags$a("click here", href="https://sciencepulse.org/methodology", target="_blank") , "to read the methodology.", style = "font-family: 'Roboto Mono', monospace"),
                      tags$br(),
                      # Language input
                      selectInput(inputId = ns("language"),
                                  label = tags$div(tags$p(style="font-weight:300", icon("language", class = "icons"), "choose language")),
                                  c("English" = "en",
                                    "Español" = "es",
                                    "Português" = "pt"))
             )),

    # Page title and description
    tags$div(class = "container-fluid", style = "text-align:center",

    column(2,

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
                       custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("rts12h_sample")))) NULL else "300px"),

           # Tables "subtitle"
           tags$em("Common hashtags, such as #COVID or #COVID19, were not considered.")),

    column(1),

    # Popular within pulse
    column(4,
           tags$p("This table shows the trending tweets from scientists within Pulse database, and does no include RTs from other users. They are often about science."),
           withSpinner(tableOutput(ns("own_sample")),
                       type = getOption("spinner.type", default = 6),
                       color = getOption("spinner.color", default = "#d91c5c"),
                       size = getOption("spinner.size", default = 1),
                       color.background = getOption("spinner.color.background", default = "#d91c5c"),
                       custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("rts12h_sample")))) NULL else "300px")
           ),

    #column(1),

    # Popular among scientists
    column(4,
           tags$p("This table shows popular tweets trending among scientists, including retweets from profiles outside Pulse. They are often about science."),
           withSpinner(tableOutput(ns("rts12h_sample")),
                       type = getOption("spinner.type", default = 6),
                       color = getOption("spinner.color", default = "#d91c5c"),
                       size = getOption("spinner.size", default = 1),
                       color.background = getOption("spinner.color.background", default = "#d91c5c"),
                       custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("rts12h_sample")))) NULL else "300px")
                       )

    # Close tags$div
    )

  # Close tagList
  )

}

#############################
#############################
###
### MODULE - TWEETS TABLE TAB

###################################################################################################

### SERVER

mod_tweets_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ####################
      ### DATASET

      tweets_dataset <- reactive({

      sql <- "SELECT name, verified, is_retweet, screen_name, retweet_count, language, created_at, text, status_id, reply_to_screen_name from base
        WHERE language = ?language
        AND created_at - interval '3 hours' >= ?datum
        AND created_at - interval '3 hours' <= cast(?datum2 as timestamp) + interval '23 hours 59 minutes 59 seconds'
        ORDER BY created_at
        "
      query  <- sqlInterpolate(base, sql, language = input$lang, datum = input$date[1], datum2 = input$date[2])
      tweets <- as_tibble(dbGetQuery(base, query))

      # Clean created_at variable
      #tweets$created_at <- as.POSIXct(tweets$created_at, format="%Y-%m-%d %H:%M:%S")
      tweets$created_at <- tweets$created_at - hours(3)
      tweets$day  <- strptime(tweets$created_at, "%Y-%m-%d")
      tweets$created_at <- format(tweets$created_at, "%d/%m/%Y, %H:%M:%S")
      tweets$dow  <- weekdays(tweets$day)
      tweets$is_replied <- ifelse(is.na(tweets$reply_to_screen_name), "FALSE", "TRUE")
      tweets <- tweets %>% arrange(desc(created_at))

      })

      ##########
      # BASIC STATS

      output$basics <- renderText({

        user_n <- tweets_dataset()
        user_n$is_replied <- ifelse(is.na(user_n$reply_to_screen_name), "FALSE", "TRUE")

        # Filter by verification, RTs and replies
        if (input$verification != "Show all profiles") {
          user_n <- user_n[user_n$verified == input$verification,]
        }
        if (input$rts != "Show retweets") {
          user_n <- user_n[user_n$is_retweet == input$rts,]
        }
        if (input$reply != "Show replies") {
          user_n <- user_n[user_n$is_replied == input$reply,]
        }

        # Main description data
        n_profiles <- user_n %>% summarise(count = n_distinct(screen_name))
        n_tweets <- user_n %>% summarise(count = n_distinct(status_id)) %>% mutate(count = format(count, big.mark=',', scientific=FALSE))

        paste("Your filters resulted in", n_tweets, "tweets from", n_profiles$count, "profiles.")

    })

      ##########
      # Main Table

      output$table <- DT::renderDataTable(DT::datatable({

        # Filter and order tweets table by date
        main_table <- tweets_dataset() %>%
          arrange(desc(created_at)) %>%
          mutate(retweet_count = format(retweet_count, big.mark=',', scientific=FALSE))

        # Filter by verification, RTs and replies
        if (input$verification != "Show all profiles") {
          main_table <- main_table[main_table$verified == input$verification,]
        }
        if (input$rts != "Show retweets") {
          main_table <- main_table[main_table$is_retweet == input$rts,]
        }
        if (input$reply != "Show replies") {
          main_table <- main_table[main_table$is_replied == input$reply,]
        }

        # Create link_tweet and handle variables;
        # select and rename variables to display on app
        main_table <- main_table %>%
          mutate(link_tweet = if_else(is_retweet == "TRUE",
                                      paste0("<i class='fas fa-retweet fa-lg' style='color:#231f20'></i> ", text, "<a href='https://twitter.com/", screen_name,"/status/",status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>"),
                                      if_else(is_replied == "TRUE",
                                              paste0("<i class='fas fa-reply fa-lg' style='color:#231f20'></i> ", text, "<a href='https://twitter.com/", screen_name,"/status/",status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>"),
                                              paste0(text, "<a href='https://twitter.com/", screen_name,"/status/",status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a></a>")))) %>%
          mutate(handle = if_else(verified == "TRUE",
                                  paste0(name, " <i class='fas fa-certificate fa-xs' style='color:rgba(29,161,242,1.00)'></i> <br><a href='https://twitter.com/", screen_name,"' target='_blank'>@", screen_name, "</a>"),
                                  paste0(name, "<br><a href='https://twitter.com/", screen_name,"' target='_blank'>@", screen_name, "</a>"))
          )%>%
          select(name, verified, is_retweet, handle, screen_name, retweet_count, language, created_at, text, link_tweet, status_id, reply_to_screen_name) %>%
          rename("profile" = handle, "date" = created_at, "RTs" = retweet_count, "tweet" = link_tweet)

        main_table

      },

      escape = FALSE,
      #rownames= FALSE,
      options = list(
        language = list(searchPlaceholder = "Type here to search...", sSearch = ""),
        pageLength = 100,
        dom = "ftipr",
        #fixedHeader= TRUE,
        searchHighlight = TRUE,
        info = FALSE,
        lengthMenu = list(c(10, 50, 100, 1000), c('10', '50', '100', '1000')),
        columnDefs = list(
          list(visible = FALSE,    targets = c(1,2,3,5,7,9,11,12)),
          list(width   = c("8%"),  targets = c(6)),
          list(width   = c("22%"), targets = c(4))
        ))
      ))

    }
  )
}

###################################################################################################

### UI

mod_tweets_ui <- function(id){

  ns <- NS(id)

  tagList(

    # Page title and description
    tags$div(class = "sheet_topper",
             tags$div(class = "sheet_header", style = "",
                      tags$h1("FIND TWEETS", style = "text-align:center"),
                      tags$p("In this page you can search for tweets in Science Pulse's database. You can apply several filters to narrow your discovery. Table is updated with new tweets and counts every 20 minutes. Since for now we stop updating older tweets, exact counts may vary from this table to actual tweets.", style = "font-family: 'Roboto Mono', monospace"),
                             tags$br(),
                             selectInput(inputId = ns("lang"),
                                         label = tags$div(icon("language", class = "icons"), 'Choose language input'),
                                         c("English" = "en",
                                           "Deutsche" = "de",
                                           "Español" = "es",
                                           "Français" = "fr",
                                           "Italiano" = "it",
                                           "Português" = "pt")
                             )
                      )),

    ##########
    # Create column on the right with inputs to filter table

    tags$div(class="filters",
             column(3,
                    dateRangeInput(inputId = ns("date"),
                                   label = tags$div(icon("calendar", class = "icons"), 'Select date range (d/m/y)', tags$p("For now, results are limited to 30 days.", style="font-weight:300")),
                                   start = Sys.Date()-3,
                                   end = Sys.Date(),
                                   min = Sys.Date()-30,
                                   max = Sys.Date(),
                                   format = "dd/mm/yyyy",
                                   weekstart = 0,
                                   language = "en",
                                   separator = " to ",
                                   width = NULL,
                                   autoclose = TRUE),
                    tags$br(),
                    selectInput(inputId = ns("verification"),
                                label = tags$div(icon("certificate", class = "icons"), 'Filter profiles verified by Twitter'),
                                c("Show all profiles",
                                  "Show verified accounts only" = "TRUE")),
                    selectInput(inputId = ns("rts"),
                                label = tags$div(icon("retweet", class = "icons"), 'Filter retweets'),
                                c("Show retweets",
                                  "Hide retweets" = "FALSE")),
                    selectInput(inputId = ns("reply"),
                                label = tags$div(icon("reply", class = "icons"), 'Filter replied posts'),
                                c("Show replies",
                                  "Hide replies" = "FALSE"))
                    )
             ),

    ##########
    # Display table

    column(5,
           tags$p( withSpinner(textOutput(ns("basics")),
                               type = getOption("spinner.type", default = 7),
                               color = getOption("spinner.color", default = "#d91c5c"),
                               size = getOption("spinner.size", default = 0.4),
                               color.background = getOption("spinner.color.background", default = "#d91c5c"),
                               custom.css = FALSE, proxy.height = "20px"), style = "font-family: 'Roboto Mono', monospace"),
           withSpinner(DT::dataTableOutput(
        ns("table")),
        type = getOption("spinner.type", default = 6),
        color = getOption("spinner.color", default = "#d91c5c"),
        size = getOption("spinner.size", default = 1),
        color.background = getOption("spinner.color.background", default = "#d91c5c"),
        custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", DT::dataTableOutput(
          ns("table")))) NULL else "300px")
      )

  )

}


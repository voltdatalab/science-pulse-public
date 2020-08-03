#############################
#############################
###
### MODULE - PROFILES TAB

###################################################################################################

### SERVER

mod_profile_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##########
      # LOAD EXPERTS DATA

      experts <- as_tibble(dbGetQuery(base, "SELECT user_id,account_created_at,name,followers_count,verified,screen_name,description,location,statuses_count from experts"))

      # Cleans tweets date format and corrects timezone
      experts$account_created_at <- as.POSIXct(experts$account_created_at, format="%Y-%m-%d %H:%M:%S")
      experts$account_created_at <- experts$account_created_at - hours(3)
      experts$account_created_at <- format(experts$account_created_at, "%d/%m/%Y, %H:%M:%S")

      # Remove emoji
      experts$name <- gsub('\\p{So}|\\p{Cn}', trimws(''), experts$name, perl = TRUE)

      # Count number of experts
      experts_count <- experts %>% count()

      ##########
      # PROFILES TABLE

      output$profiles <- DT::renderDataTable(DT::datatable({

        # Create tibble with selected variables of experts information
        experts_tbl <- experts %>%
          filter(followers_count > input$followers) %>%
          mutate(name = if_else(verified == "TRUE",
                                paste0(name, " <i class='fas fa-certificate fa-xs' style='color:rgba(29,161,242,1.00)'></i> <br><a href='https://twitter.com/", screen_name,"' target='_blank'>@", screen_name, "</a>"),
                                paste0(name, "<br><a href='https://twitter.com/", screen_name,"' target='_blank'>@", screen_name, "</a>"))
                 ) %>%
          select(verified, name, screen_name, description, location, followers_count, statuses_count, account_created_at) %>%
          arrange(screen_name) %>%
          mutate(followers_count = format(followers_count, big.mark=',', scientific=FALSE)) %>%
          mutate(statuses_count = format(statuses_count, big.mark=',', scientific=FALSE)) %>%
          rename("account creation" = account_created_at, "followers" = followers_count, "posts" = statuses_count)

        # Display table considering "prof_verified" input

        if (input$prof_verified != "Show all profiles") {
          experts_tbl <- experts_tbl[experts_tbl$verified == input$prof_verified,]
          }

        experts_tbl

        },
        escape = FALSE,

        options = list(
          language = list(searchPlaceholder = "Type to search...", sSearch = ""),
          dom = "ftir",
          pageLength = 500,
          searchHighlight = TRUE,
          info = FALSE,
          columnDefs = list(
            list(visible=FALSE, targets=c(1)),
            list(width = '150px', targets = c(2,7))
            ),
          lengthMenu = list(c(10, 50, 100, 1000), c('10', '50', '100', '1000')))
      ))

      ##########
      # Text about n. of profiles

      output$experts_count <- renderText({
        paste("There are", experts_count$n, "profiles in our database.")
      })

      ### DO YOU KNOW THEM?

      # A random sample of 10 from our less-followed experts on the list

      output$new_users <- renderTable({

        experts %>%
          mutate(mediana = median(followers_count)) %>%
          filter(followers_count < mediana) %>%
          sample_n(5) %>%
          #select(screen_name, followers_count) %>%
          select(user_id, name, description, screen_name) %>%
          #pivot_longer(-user_id,
                       #names_to = "screen_name") %>%
          #mutate(screen_name = paste0("@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #d91c5c'>", screen_name, "</a>")) %>%
          mutate(value_display = paste0("<h3 style='margin-top:0'>@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #d91c5c'>", screen_name, "</a></h3><p style='margin-botom:10px'><em>", description, "</em></p>")) %>%
          select(user_id, value_display) %>%
          rename("Find new experts" = value_display)

      },

      sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

    }
  )
}

###################################################################################################

### UI

mod_profile_ui <- function(id){

  ns <- NS(id)

  tagList(
    # Page title and description
    tags$div(class = "sheet_topper",
             tags$div(class = "sheet_header", style = "",
                      tags$h1("WHO WE FOLLOW", style = "text-align:center"),
                      tags$p("In this page you can search for the profiles of scientists, experts, organizations and scientific initiatives that are the core of the", tags$b("Science Pulse."), "You can download this table", tags$a(href="https://docs.google.com/spreadsheets/d/11W4Sw3M4pJ12yolY03UsU_lUDjnuaOYLYcldy4xZPfg/edit?usp=sharing", target="_blank", "right here."), style = "font-family: 'Roboto Mono', monospace"),
                      tags$br()
                      )),

    tags$div(class="filters",
             column(3,
                    # Verified input
                    selectInput(inputId = ns("prof_verified"),
                                label = tags$div(icon("certificate", class = "icons"), 'Filter profiles verified by Twitter'),
                                c("Show all profiles",
                                "Show verified accounts only" = "TRUE")),

                    # Followers input
                    numericInput(inputId = ns("followers"),
                                 label = tags$div(icon("user", class = "icons"), 'Minimum followers'),
                                 value = 0,
                                 min = NA,
                                 max = NA,
                                 step = NA,
                                 width = "50px"),
             # Do you know them?
             tags$br(),
             withSpinner(tableOutput(ns("new_users")),
                         type = getOption("spinner.type", default = 1),
                         color = getOption("spinner.color", default = "#d91c5c"),
                         size = getOption("spinner.size", default = 1),
                         color.background = getOption("spinner.color.background", default = "#d91c5c"),
                         custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("new_users")))) NULL else "300px"
             )
             )),

             column(5,
                    withSpinner(textOutput(ns("experts_count")),
                                type = getOption("spinner.type", default = 7),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 0.4),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = "20px"
                    ),
                    tags$br(),
                    withSpinner(DT::dataTableOutput(ns("profiles")),
                                type = getOption("spinner.type", default = 6),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", DT::dataTableOutput(ns("profiles")))) NULL else "300px")

                    )

    )
}


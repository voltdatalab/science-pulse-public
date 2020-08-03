#############################
#############################
###
### MODULE - POPULARITY TAB

library(ggthemes)
library(sysfonts)

###################################################################################################

### SERVER

mod_popularity_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      font_add_google("Barlow", "barlow")

      # Create theme for graph

      tema <- function(base_size = 14, base_family = "Barlow") {
        (theme_foundation(base_size = base_size, base_family = base_family) +
           theme(
             plot.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
             panel.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
             #line = element_line(colour = "#231f20"),
             text = element_text(colour = "#231f20"),

             #axis.title = element_blank(),
             axis.text = element_text(size = rel(0.8), margin=margin(0,40,0,0)),
             axis.ticks = element_blank(),
             axis.line = element_blank(),
             axis.title = element_text(size = rel(0.9), colour = "#999999"),

             legend.text = element_text(size=rel(0.9), angle = 0),
             legend.title = element_blank(),
             #legend.background = element_rect(colour = "#eeeeee", fill="#eeeeee", size=0),
             legend.key = element_rect(fill = "#eeeeee", colour = "#eeeeee", size = 0.5, linetype='dashed'),
             legend.key.width = unit(0.6, "cm"),
             #legend.position = c(0, 1.05),
             legend.position = NULL,
             legend.justification = c(-0.05, 0),
             legend.background = element_blank(),
             legend.direction = "horizontal",
             legend.margin = (margin=margin(0,0,0,0)),
             legend.box = NULL,
             #legend.box = "vertical",
             #legend.position='top',
             #legend.justification='left',

             panel.border = element_rect(colour = "#eeeeee", fill=NA, size=2),
             #panel.grid = element_line(colour = "#e0e0e0"),
             panel.grid.major = element_line(colour = "#e4e4e4"),
             panel.grid.minor = element_line(colour = "#e6e6e6"),
             panel.grid.minor.x = element_line(colour = "#e4e4e4"),

             plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold", colour = "#231f20"),
             plot.title.position = "plot",
             strip.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
             plot.subtitle = element_text(hjust = 0, margin=margin(0,0,40,0),size = rel(1), lineheight = 1),
             plot.caption = element_text(size = rel(0.75), hjust = 1, margin=margin(20,0,0,0), colour = "#555555", lineheight = 1),
             plot.margin = unit(c(-3, 2, 1, 1), "lines")
           )
        )
      }

      # Create vector with colors
      #cores <- c("#4b31dd", "#f4346f", "#ffb14e", "#159e84", "#231f20", "#e2a805",
      #           "#000000", "#ad7cc8", "#ad0b13", "#2283aa", "#f47018", "#1a6429")

      ####################
      ### DATASET

      popularity_dataset <- reactive({

      # Load data for popularity chart
      sql_t <- "SELECT status_id,screen_name,hashtags,text,created_at,language from base
        WHERE language = ?language
        AND created_at - interval '3 hours' >= ?datum
        AND created_at - interval '3 hours' <= cast(?datum2 as timestamp) + interval '23 hours 59 minutes 59 seconds'
        ORDER BY created_at
        "
      query_t <- sqlInterpolate(base, sql_t, language = input$language, datum = input$datepop[1], datum2 = input$datepop[2])
      pop <- as_tibble(dbGetQuery(base, query_t))

      # Cleans tweets date format and corrects timezone
      pop$created_at <- as.POSIXct(strptime(pop$created_at, "%Y-%m-%d %H:%M:%S"))
      pop$created_at <- pop$created_at - hours(3)
      pop$dia <- as.POSIXct(strptime(pop$created_at, "%Y-%m-%d"))
      return(pop)

      })

      ####################
      ### OUTPUTS

      ####################
      # HASHTAGS

      output$hashtags <- renderTable({

        # Extract most shared hashtags
        hashtags_variable <- popularity_dataset()
        hashtags_variable <- select(hashtags_variable, hashtags)
        hashtags <- hashtags_variable$hashtags
        lista_hashtags <- unlist(str_split(hashtags, ","))
        lista_hashtags <- tibble(hashtag = str_trim(lista_hashtags, "both"))

        lista_hashtags <- lista_hashtags %>%
          filter(hashtag != "NA") %>%
          mutate(hashtag = toupper(stri_trans_general(hashtag, "Latin-ASCII"))) %>%
          mutate(hashtag = paste0("#<a href='https://twitter.com/hashtag/", hashtag, "' target='_blank' style='color: #d91c5c'>", hashtag, "</a>")) %>%
          count(hashtag, sort = T) %>%
          slice(1:5) %>%
          select(n, hashtag) %>%
          rename("Popular Hashtags" = hashtag)

      },

      sanitize.text.function = function(x) x,scrollX = TRUE, striped = TRUE, target="_blank")

      ####################
      # ACTIVE USERS

      output$active_users <- renderTable({

        popularity_dataset() %>%
          group_by(language, screen_name) %>%
          count(sort = T) %>%
          ungroup() %>%
          slice(1:5) %>%
          select(language, screen_name) %>%
          mutate(screen_name = paste0("@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #d91c5c'>", screen_name, "</a>")) %>%
          rename("Active users" = screen_name)

      },

      sanitize.text.function = function(x) x,scrollX = TRUE, striped = TRUE, target="_blank")

      ####################
      # CHART WITH TERM POPULARITY


      plotar_grafico <- eventReactive(input$generate, {

        # Creates column with rounded minuts
        # Other possible aggregations: “secs”, “mins”, “hours”, “days”, “months”, “years”
        pop <- popularity_dataset()
        pop$min_redondo <- round(pop$created_at, units = "hours")
        pop$new <- as.character(pop$min_redondo)

        # Filter dataset with the term inserted by the user
        contagem <- pop %>%
          mutate(is_filtered = str_detect(text, regex(input$chartTerms, ignore_case = TRUE))) %>%
          filter(is_filtered == T) %>%
          group_by(new) %>%
          #group_by(dia) %>%
          #summarize(count=n())
          count() %>%
          ungroup()

        contagem$new <- as.POSIXct(strptime(contagem$new, "%Y-%m-%d %H:%M:%S"))

        contagem %>%
          ggplot(aes(new, n), show.legend = FALSE) +
          geom_step(colour = "#d91c5c", alpha = 0.5) +
          #geom_area(fill = "#d91c5c", alpha = 0.5) +
          # geom_bar(stat="identity", fill = "#d91c5c", width=1000, na.rm=TRUE) +
          #geom_point(size = 0.9, colour = "#d91c5c") +
          geom_smooth(method = 'loess', se=F, alpha = 0.6, size = 0.7, colour = "#000000", na.rm=TRUE) +
          #scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),breaks = scales::pretty_breaks(n = 3)) +
          scale_x_datetime(labels   = date_format("%d.%b\n%Hh")
                           #date_breaks = "24 hours"
          ) +
          labs(x="",
               y="Number of mentions per hour",
               subtitle = "",
               title = "",
               caption = "Source: Science Pulse analysis with Twitter data") +
          tema()

        })

        output$chart_popularity <- renderPlot({

          # Plot the chart using "contagem" dataset
        suppressWarnings(plotar_grafico())

        })

        # Chart subtitle reactive
        subtitulo_grafico <- eventReactive(input$generate, {

          a <- paste("The steps accounts for mentions in one hour, and the line is the trend.", tags$br(), "Data considers only profiles mapped in Pulse's database")
          HTML(a)

        })

        # Include chart subtitle
        output$chart_subtitle <- renderUI({

          subtitulo_grafico()

        })

      # Chart legend reactive
      legenda_grafico <- eventReactive(input$generate, {

        a <- paste0("Popularity of the term <span style='color:#d91c5c'>", input$chartTerms , "</span> among scientists")
        HTML(a)

      })

      # Include chart legend
      output$chart_legend <- renderUI({

        legenda_grafico()

        })

    }

  )

}

###################################################################################################

### UI

mod_popularity_ui <- function(id){

  ns <- NS(id)


  tagList(
    # Page title and description
    tags$div(class = "sheet_topper",
             tags$div(class = "sheet_header", style = "",
                      tags$h1("TERM POPULARITY", style = "text-align:center"),
                      tags$p("In this page you can see the popularity of certain terms, science profiles and hashtags on Twitter. You can search for keywords and change the date range (maximum allowed for now is a 30-day interval.)", style = "font-family: 'Roboto Mono', monospace"),
                      tags$br(),
                      # Language input
                      selectInput(inputId = ns("language"),
                                  label = tags$div(icon("language", class = "icons"), 'Choose language input'),
                                  c("English" = "en",
                                    "Español" = "es",
                                    "Português" = "pt"))
             )),

    # Page config
    sidebarLayout(position = c("left", "right"), fluid = TRUE,

                  # START SIDE PANEL
                  sidebarPanel(width = 3,
    tags$div(class = "container-fluid", style = "text-align:center",

             # Date range input
             dateRangeInput(inputId = ns("datepop"),
                                               label = tags$div(icon("calendar", class = "icons"), 'Date range (d/m/y)'),
                                               start = Sys.Date()-7,
                                               end = Sys.Date(),
                                               min = Sys.Date()-30,
                                               max = Sys.Date(),
                                               format = "dd/mm/yyyy",
                                               startview = "week",
                                               weekstart = 0,
                                               language = "en",
                                               separator = " to ",
                                               width = NULL,
                                               autoclose = TRUE),

             tags$br(),

                    withSpinner(
                      tableOutput(ns("hashtags")),
                                type = getOption("spinner.type", default = 1),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("hashtags")))) NULL else "300px"),
                    tags$br(),
                    withSpinner(
                      tableOutput(ns("active_users")),
                                type = getOption("spinner.type", default = 1),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("active_users")))) NULL else "300px")
             # close div
             )
             # close SidebarPanel
             ),

            # OPEN MAIN PANEL
            mainPanel(width = 7,
               tags$br(),

               # Search term input
               column(4, textInput(inputId = ns("chartTerms"),
                              label = tags$div(icon("search", class = "icons"), 'Search keywords'),
                              value = "COVID",
                              width = NULL,
                              placeholder = "COVID")),

               # Action button to trigger search
               column(1, tags$div(style="margin-top:25px",
                                  actionButton(inputId = ns("generate"),
                                               label   = "click to search"))),
                    # CHART
              column(12,
                     tags$h1(class = "chart_title", htmlOutput(ns("chart_legend"))),
                     tags$h4(class = "chart_subtitle", htmlOutput(ns("chart_subtitle"))),

                     withSpinner(plotOutput(ns("chart_popularity"), width = "100%"),
                                type = getOption("spinner.type", default = 6),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(ns("active_users")))) NULL else "300px"))
    # close main Panel
            )
    #close sidebarLayout
    )
    #taglist close
    )

}

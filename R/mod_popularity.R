#############################
#############################
###
### MODULE - POPULARITY TAB

library(ggplot2)
library(ggthemes)
library(sysfonts)
library(lubridate)

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
      cores <- c("#4b31dd", "#f4346f", "#ffb14e", "#159e84", "#231f20", "#e2a805", 
                 "#000000", "#ad7cc8", "#ad0b13", "#2283aa", "#f47018", "#1a6429")
      
      ##########
      # TABLE WITH TRENDING HASHTAGS 
      output$hashtags_trends <- renderTable({
        # Load data
        sql_t <- "SELECT * from base 
        WHERE language = ?language 
        ORDER BY created_at DESC
        LIMIT 30000
        "
        query_t <- sqlInterpolate(base, sql_t, language = input$language_chart)
        trends <- as_tibble(dbGetQuery(base, query_t))
        
        # Create current time column
        trends$current_time <- lubridate::now()
        trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
        trends$created_at <- trends$created_at - hours(3)
        
        # Identify tweets texts' language
        trends$language <- detect_language(trends$text)
        
        #  Filter last 24h
        trends_24h <- filter(trends, created_at > current_time - hours(12))
        
        # Hashtags por dia/lingua
        linguas <- unique(trends_24h$language)
        final_dataset <- NULL
        
        for(lingua in c("es", "pt", "en")){
          trends_24h_lingua <- filter(trends_24h, language == lingua)
          
          hashtags <- trends_24h_lingua$hashtags
          lista_hashtags <- str_split(hashtags, ",")
          lista_hashtags <- unlist(lista_hashtags)
          lista_hashtags <- tibble(hashtag = str_trim(lista_hashtags, "both"))
          
          lista_hashtags <- lista_hashtags %>%
            filter(!is.na(hashtag)) %>%
            mutate(hashtag = toupper(stri_trans_general(hashtag, "Latin-ASCII"))) %>%
            mutate(hashtag = paste0("#<a href='https://twitter.com/hashtag/", hashtag, "' target='_blank' style='color: #d91c5c'>", hashtag, "</a>")) %>%
            count(hashtag, sort = T) %>%
            slice(1:5) %>%
            mutate(language = lingua)
          
          # Final dataset with 10 hashtags in each language
          final_dataset <- rbind.data.frame(final_dataset, lista_hashtags)
        }
        
        final_dataset %>% 
          filter(language == input$language_chart) %>% 
          select(n, hashtag) %>% 
          rename("Hashtags (last 12 hours)" = hashtag)
      }, 
      
      sanitize.text.function = function(x) x,scrollX = TRUE, striped = TRUE, target="_blank")
      
      ##########
      # CHART WITH HASHTAG POPULARITY
      output$chart_popularity <- renderPlot({
        sql_t <- "SELECT status_id,text,created_at,language from base 
        WHERE language = ?language
        AND created_at - interval '3 hours' >= ?datum
        AND created_at - interval '3 hours' <= now()
        ORDER BY created_at 
        "
        query_t <- sqlInterpolate(base, sql_t, language = input$language_chart, datum = input$datepop[1])
        pop <- as_tibble(dbGetQuery(base, query_t))
        
        # Cleans tweets date format and corrects timezone
        pop$created_at <- as.POSIXct(strptime(pop$created_at, "%Y-%m-%d %H:%M:%S"))
        pop$created_at <- pop$created_at - hours(3)
        pop$dia <- as.POSIXct(strptime(pop$created_at, "%Y-%m-%d"))
        
        # Creates column with rounded minuts
        # Other possible aggregations: “secs”, “mins”, “hours”, “days”, “months”, “years”
        pop$min_redondo <- round(pop$created_at, units = "hours")
        pop$new <- as.character(pop$min_redondo)
        
        contagem <- pop %>%
          mutate(is_filtered = str_detect(text, regex(input$chartTerms, ignore_case = TRUE))) %>%
          filter(is_filtered == T) %>%
          group_by(new) %>%
          #group_by(dia) %>%
          #summarize(count=n())
          count() %>%
          ungroup()
        
        contagem$new <- as.POSIXct(strptime(contagem$new, "%Y-%m-%d %H:%M:%S"))
        
        # Plot the chart using "contagem" dataset
        suppressWarnings(
          ggplot(contagem, aes(new, n), show.legend = FALSE) + 
            geom_step(colour = "#d91c5c", alpha = 0.5) +
            #geom_area(fill = "#d91c5c", alpha = 0.5) +
            # geom_bar(stat="identity", fill = "#d91c5c", width=1000, na.rm=TRUE) + 
            #geom_point(size = 0.9, colour = "#d91c5c") +
            geom_smooth(method = 'loess', se=F, alpha = 0.6, size = 0.7, colour = "#000000", na.rm=TRUE) + 
            scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE), 
                               breaks = scales::pretty_breaks(n = 10)) +
            scale_x_datetime(labels   = date_format("%d.%b\n%Hh") 
                             #date_breaks = "24 hours"
                            ) +
            labs(x="", 
            y="Number of mentions per hour", 
            subtitle = "",
            title = "",
            caption = "Source: Science Pulse analysis with Twitter data") +
            tema())
      })
      
      # Include chart subtitle
      output$chart_subtitle <- renderUI({
        a <- paste("The steps accounts for mentions in one hour, and the line is the trend.", tags$br(), "Data considers only profiles mapped in Pulse's database")
        HTML(a)
      })
      # Include chart legend
      output$chart_legend <- renderUI({
        a <- paste0("Popularity of the term <span style='color:#d91c5c'>", input$chartTerms , "</span> among scientists")
        HTML(a)
      })
       
    }
    
  )

}

###################################################################################################

### UI

mod_popularity_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    tags$div(class = "container-fluid",
             
             column(3, 
                    tableOutput(ns("hashtags_trends")),
                    dateRangeInput(inputId = ns("datepop"), 
                                   label = tags$div(icon("calendar", class = "icons"), 'Select date range', tags$p("For now, searches are limited to 30 days", style="font-weight:300")), 
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
                    tags$br()),
             
             column(3,
                    selectInput(inputId = ns("language_chart"),
                                   label = tags$div(icon("language", class = "icons"), 'Choose language input'),
                                   c("English" = "en", 
                                     "Español" = "es",
                                     "Português" = "pt"))),
             
             column(3, textInput(inputId = ns("chartTerms"), 
                                 label = tags$div(icon("search", class = "icons"), 'Search keywords. Example: COVID'), 
                                 value = "COVID", 
                                 width = NULL,
                                 placeholder = "Search here")),
             
             column(5,
                    
                    tags$h1(class = "chart_title", htmlOutput(ns("chart_legend"))),
                    tags$h4(class = "chart_subtitle", htmlOutput(ns("chart_subtitle"))),
                    
                    plotOutput(ns("chart_popularity")))
             )
    
  )
    
}

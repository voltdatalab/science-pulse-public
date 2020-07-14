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
      
      experts <- as_tibble(dbGetQuery(base, "SELECT * from experts"))
      experts$account_created_at <- as.POSIXct(experts$account_created_at, format="%Y-%m-%d %H:%M:%S")
      experts$account_created_at <- experts$account_created_at - hours(3)
      experts$account_created_at <- format(experts$account_created_at, "%d/%m/%Y, %H:%M:%S")
      experts$name <- gsub('\\p{So}|\\p{Cn}', trimws(''), experts$name, perl = TRUE)
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
      # Text about profiles
      
      output$experts_count <- renderText({
        paste("There are", experts_count$n, "profiles.")
      })
      
    }
  )    
}

##########
# PROFILES TABLE

mod_profile_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    tags$div(class="filters",
             column(2, 
                    tags$p("These are profiles of scientists, experts, organizations and scientific initiatives that are the core of the", 
                    tags$b("Science Pulse."), "You can download this table", tags$a(href="https://docs.google.com/spreadsheets/d/11W4Sw3M4pJ12yolY03UsU_lUDjnuaOYLYcldy4xZPfg/edit?usp=sharing", target="_blank", "here."), 
                    textOutput(ns("experts_count"))),
                             
                    selectInput(inputId = ns("prof_verified"),
                                label = tags$div(icon("certificate", class = "icons"), 'Filter profiles verified by Twitter'),
                                c("Show all profiles",
                                "Show verified accounts only" = "TRUE")),
                    
                    numericInput(inputId = ns("followers"), 
                                 label = tags$div(icon("user", class = "icons"), 'Minimum followers'), 
                                 value = 0, 
                                 min = NA, 
                                 max = NA, 
                                 step = NA,
                                 width = "50px"))
                      ),
             
             column(5, DT::dataTableOutput(ns("profiles")))
             
    )  
}


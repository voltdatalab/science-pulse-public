#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULO - TWITTER - EXPLORE          #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_explore_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      ##### PREPARAR BASE

      trends_dataset <- reactive({

        # Carregar dados da view explore
        sql_t <- "SELECT * from v_mod_explore"
        query_t <- sqlInterpolate(base, sql_t)
        trends <- as_tibble(dbGetQuery(base, query_t))

        # Filtro de idioma
        trends <- trends %>%
          filter(language == input$language)

        # Atualiza o fuso-horario dos tweets
        trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
        trends$created_at <- trends$created_at - hours(3)

        # Filtra pelas ultimas 12h
        trends$current_time <- lubridate::now()
        trends <- filter(trends, created_at > current_time - hours(12))

      })

      ##################################################
      ##### EXPLORE

      ##########
      ### USUARIOS MAIS ATIVOS
      output$active_users <- renderTable({

        trends_dataset() %>%
          active_users()

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ##########
      ### HASHTAGS MAIS TUITADAS
      output$hashtags <- renderTable({

        hashtags <- trends_dataset() %>%
          .$hashtags

        lista_hashtags <- unlist(str_split(hashtags, ","))
        lista_hashtags <- tibble(hashtag = str_trim(stri_trans_general(lista_hashtags, "Latin-ASCII"), "both"))
        lista_hashtags %>%
          most_hashtags()

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ##########
      ### TAMBEM POPULARES NO PULSE
      output$own_sample <- renderTable({

        trends_dataset() %>%
          also_popular()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### RADAR PULSE
      output$hidden_gems <- renderTable({

        trends_dataset() %>%
          pulse_radar()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### POPULAR ENTRE CIENTISTAS
      output$rts12h_overall <- renderTable({

        trends_dataset() %>%
          popular_among_scientists()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

    }

  )

}

#########################################################################################

### UI

mod_explore_ui <- function(id, i18n){

  ns <- NS(id)

  tagList(

    ### TITULO E DESCRICAO DA PAGINA
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),

             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("EXPLORE MAIS", style = "text-align:center"),
                      tags$p("Esta seção contém informações complementares às", tags$b("TENDÊNCIAS"),
                             "a partir dos nossos conjuntos de dados. Encontre outros tweets relevantes e publicações escondidas de perfis que não necessariamente possuem muitos seguidores ou engajamento em seus posts. Você pode",
                      tags$a("clicar aqui", href="https://sciencepulse.org/metodologia", target="_blank"),
                      "para ler a metodologia.")),
             selectInput(inputId = ns("language"),
                         label = tags$div(icon("language", class = "icons"), 'Escolha o idioma dos tweets'),
                         c("Português" = "pt",
                           "Inglês" = "en",
                           "Espanhol" = "es"))
                      ),

    ### PAGINA PRINCIPAL
    tags$div(class = "container-fluid", style = "text-align:center",

             ### TABELAS A ESQUERDA
             column(2,

                    tags$p("Perfis mais ativos e principais hashtags nas últimas 12 horas."),
                    # Usuarios mais ativos
                    include_spinner_thin_column(ns("active_users")),
                    tags$br(),
                    # Hashtags mais usadas
                    include_spinner_thin_column(ns("hashtags"))

             ),

             ### COLUNAS PRINCIPAIS

             column(1),

             column(3,

                    tags$div(tags$b("TAMBÉM POPULARES NO PULSE"), icon("angle-double-up")),
                    tags$br(),
                    tags$p("Tweets populares que não entraram nos trends. Essa coluna é uma ampliação da métrica de tweets mais populares."),
                    include_spinner_large_column(ns("own_sample"))

                    ),

             column(3,

                    tags$div(tags$b("RADAR PULSE"), icon("dot-circle")),
                    tags$br(),
                    tags$p("Amostra aleatória de tweets populares. Essa coluna serve para aumentar a descoberta de conteúdo que fica de fora dos trends."),
                    include_spinner_large_column(ns("hidden_gems"))

                    ),

             column(3,

                    tags$div(tags$b("POPULAR ENTRE CIENTISTAS"), icon("retweet")),
                    tags$br(),
                    tags$p("Tweets de assuntos populares. Essa métrica inclui RTs de perfis fora do Science Pulse e pode incluir tópicos além da ciência."),
                    include_spinner_large_column(ns("rts12h_overall"))

                    )

             # Fecha tags$div
             )

    # Fecha tagList
    )

}

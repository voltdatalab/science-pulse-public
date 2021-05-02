#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULO - TWITTER - TRENDS           #######################
###################                                               #######################

#########################################################################################

##### SERVER

mod_trends_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      ##### PREPARAR BASE

      trends_dataset <- reactive({

        # Carregar dados da view de trends
        sql_t <- "SELECT * from v_mod_trends"
        query_t <- sqlInterpolate(base, sql_t)
        trends <- as_tibble(dbGetQuery(base, query_t))

        # Filtro de idioma
        trends <- trends %>%
          filter(language == input$language)

        # Atualiza o fuso-horario dos tweets
        trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
        trends$created_at <- trends$created_at - hours(3)

        # Filtra pelas ultimas 12h
        # a view coleta tweets das 12h antes da ultima coleta. Como a coleta
        # e feita a cada 12h, pode existir um delay. Por isso, filtramos novamente
        trends$current_time <- lubridate::now()
        trends <- filter(trends, created_at > current_time - hours(12))

      })

      ##################################################
      ##### PRINCIPAIS TRENDS
      
      output$n_tweets <- renderText(
        trends_dataset() %>% count()
      )

      ##########
      ### POPULARIDADE EM ALTA
      output$overperform <- renderTable({
        trends_dataset() %>%
          overperforming()
      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### POPULAR NO PULSE
      output$own_sample <- renderTable({

        # DESCOBERTA (RT-RATIO)
        if(input$mode == "Discovery"){
          trends_dataset() %>%
            rising_popularity()
        }
        # POPULARIDADE
        else{
          trends_dataset() %>%
            popular_within_pulse() }
        }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### DESCUBRA MAIS

      # Cria botao de sorteio de nova tabela
      sample_table <- eventReactive(input$new_sample, {
        trends_dataset() %>%
          sample_more_than_one()
        })

      # Gera tabela reativa
      output$pulse_radar <- renderTable({
        if(input$new_sample == 0){
          trends_dataset() %>%
            sample_more_than_one()
          } else
            sample_table()
        }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

    # Fecha modulo
    })

}

###################################################################################################

### UI

mod_trends_ui <- function(id){

  ns <- NS(id)

  tagList(

    ### TITULO E DESCRICAO DA PAGINA
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),

             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace", textOutput("n_tweets"),
                      tags$h1("TENDÊNCIAS E DESTAQUES", style = "text-align:center"),
                      tags$p("O", tags$em("crème de la crème"), "das últimas 12 horas na curadoria do Science Pulse. Explore nossas três métricas diferentes e descubra novos conteúdos. Leia",
                             tags$a("aqui", href="https://sciencepulse.org/metodologia", target="_blank"),
                             "nossa metodologia."),
                      tags$br(),
                      selectInput(inputId = ns("language"),
                                  label = tags$div(icon("language", class = "icons"), 'Escolha o idioma dos tweets'),
                                  c("Português" = "pt",
                                    "Inglês" = "en",
                                    "Espanhol" = "es"))
                      )),

    ### COLUNAS
    tags$div(class = "container-fluid", style = "text-align:center",
             tags$style(HTML(".control-label{float:left;margin-left:30px;margin-top:3px}")),

             column(4,

                    tags$div(tags$b("ISSO É TENDÊNCIA"), icon("chart-line")),
                    tags$br(),
                    tags$p("Nossa métrica principal, que mostra tweets com maior número de interações em relação ao que este perfil teria no geral", tags$em("(overperforming)"), ". Essa métrica visa identificar tweets de usuários que se destacam em relação ao seu próprio desempenho."),
                    tags$br(),
                    tags$br(),
                    include_spinner_large_column(ns("overperform"))

             ),

             column(4,

                    tags$div(tags$b("PEGANDO FOGO"), icon("fire")),
                    tags$br(),
                    tags$p("Os tweets mais populares. Escolha entre",
                           tags$b("Modo Popularidade,"), "que mostra posts mais compartilhados, e", tags$b("Modo Descoberta"),
                           "que calcula a proporção de RTs pelo número de seguidores."),
                    tags$br(),
                    radioButtons(inputId = ns("mode"),
                                 label = tags$div(style="font-weight:200",
                                           icon("stream", class = "icons")," Modo"),
                                 inline = TRUE,
                                 c("Popularidade" = "Popularity",
                                   "Descoberta"   = "Discovery")),
                    include_spinner_large_column(ns("own_sample"))

             ),

             column(4,

                    tags$div(tags$b("RADAR"), icon("random")),
                    tags$br(),
                    tags$p("Amostra aleatória de tweets publicados por perfis de curadoria do Science Pulse. Essa métrica considera publicações com mais de um RT. Clique no botão para ver novos posts."),
                    actionButton(inputId = ns("new_sample"),
                                 label   = "Mostrar novos tweets"),
                    tags$br(),
                    include_spinner_large_column(ns("pulse_radar"))

             )

    ))

}

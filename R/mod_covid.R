#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULO - TWITTER - COVID            #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_covid_server <- function(id, base){

  shiny::moduleServer(
    id,
    function(input, output, session){

      ##################################################
      ##### PREPARAR BASE

      trends_dataset <- reactive({

        # Carregar dados da view de covid
        sql_t <- "SELECT * from v_mod_covid"
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

        # Identifica as palavras-chave de Covid
        keywords <- c("Covid", "covid", "Coronavirus", "coronavirus",
                      "Corona", "corona", "SARS-CoV-2", "Sars-CoV-2",
                      "SRAG", "sindrome", "syndrome", "pandemic",
                      "pandemia", "WHO", "OMS", "quarantine", "social distancing",
                      "quarentena", "isolamento social", "distanciamento social",
                      "mascara", "mask", "distanciamiento social", "spread",
                      "asymptomatic", "epidemic", "outbreak", "epidemia",
                      "vacina", "vaccine", "wuhan", "Wuhan", "herd immunity",
                      "imunidade de rebanho", "imunidade coletiva", "lockdown",
                      "blood clot", "coágulo", "AstraZeneca", "Astrazeneca",
                      "astrazeneca", "Coronovac", "CoronoVac", "coronavac",
                      "Janssen", "janssen", "Sputnik", "sputnik",
                      "máscara", "mascara", "mask")

        # Filtra tweets que contem ao menos uma dessas palavras-chave
        trends <- filter(trends,
                             str_detect(text, paste(keywords, collapse = "|")))

      })

      ##################################################
      ##### COVID-19 TRENDS

      ##########
      ### USUARIOS MAIS ATIVOS
      output$active_users <- renderTable({

        trends_dataset() %>%
          active_users()

        }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ##########
      ### HASHTAGS MAIS TUITADAS
      output$hashtags <- renderTable({

        # Excluir hashtags comuns
        exclude_hashtags <- c("coronavirus", "coronaviruses", "covid", "covid__19",
                              "covid_19", "covidー19", "covid-19", "covid19",
                              "covid19brasil", "covid19ma", "covid2019",
                              "covid9", "pandemia", "pandemias", "pandemic",
                              "pandemic2020", "sars_cov_2", "sarscov2")

        # Extrai as hashtags e as coloca numa tabela
        hashtags <- trends_dataset() %>%
          .$hashtags
        lista_hashtags <- unlist(str_split(hashtags, ","))
        lista_hashtags <- tibble(hashtag = tolower(
        # Limpa formatacao e apara
          str_trim(stri_trans_general(lista_hashtags, "Latin-ASCII"), "both")))
        # Exclui as comuns e conta as demais
        lista_hashtags %>%
          filter(!hashtag %in% exclude_hashtags) %>%
          most_hashtags()

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

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
        else {

          trends_dataset() %>%
            popular_within_pulse() }

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### POPULAR ENTRE CIENTISTAS
      output$rts12h_sample <- renderTable({

        trends_dataset() %>%
          popular_among_scientists()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

    # Fecha modulo
    })

}

###################################################################################################

### UI

mod_covid_ui <- function(id){

  ns <- NS(id)

  tagList(

    ### TITULO E DESCRICAO DA PAGINA
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),

             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("ESPECIAL COVID-19", style = "text-align:center"),
                      tags$p("Esta seção mostra tendências sobre tweets, hashtags e perfis mais ativos com palavras-chave relacionadas à pandemia de Covid-19, nas últimas 12 horas. Você pode",
                             tags$a("clicar aqui", href="https://sciencepulse.org/metodologia", target="_blank"),
                             "para ler a metodologia."),
                      tags$br(),
                      selectInput(inputId = ns("language"),
                                  label = tags$div(icon("language", class = "icons"), 'Escolha o idioma dos tweets'),
                                  c("Português" = "pt",
                                    "Inglês" = "en",
                                    "Espanhol" = "es"))
             )),

    tags$div(class = "container-fluid", style = "text-align:center",

             ### TABELAS A ESQUERDA
             column(2,

                    tags$p("Perfis mais ativos e principais hashtags nas últimas 12 horas."),

                    # USUARIOS MAIS ATIVOS
                    include_spinner_thin_column(ns("active_users")),
                    tags$br(),

                    # HASHTAGS MAIS USADAS
                    include_spinner_thin_column(ns("hashtags")),

                    # Legenda das tabelas
                    tags$em("Hashtags comuns, como #COVID ou #COVID19, são desconsideradas.")),

             ### TABELAS PRINCIPAIS
             column(1),

             column(3,

                    tags$div(tags$b("POPULARIDADE EM ALTA"), icon("chart-line")),
                    tags$br(),
                    tags$p("Tweets com maior número de interações em relação ao que esta conta teria no geral. Essa métrica visa identificar tweets de usuários que se destaquem em relação a sua tendência."),
                    include_spinner_large_column(ns("overperform"))

             ),

             column(3,

                    tags$div(tags$b("POPULAR NO PULSE"), icon("fire")),
                    tags$br(),
                    tags$style(HTML(".control-label{float:left;margin-left:30px;margin-top:3px}")),
                    tags$p("Tweets mais populares. Essa métrica não inclui RTs de perfis fora do Science Pulse. Escolha entre",
                           tags$em("Modo Popularidade"), "e", tags$em("Modo Descoberta"),
                           "que calcula a proporção de RTs pelo número de seguidores."),
                    radioButtons(inputId = ns("mode"),
                                 label = tags$div(style="font-weight:200",
                                                  icon("stream", class = "icons")),
                                 inline = TRUE,
                                 c("Descoberta" = "Discovery",
                                   "Popularidade" = "Popularity")),
                    include_spinner_large_column(ns("own_sample"))

                    ),

             column(3,

                    tags$div(tags$b("POPULAR ENTRE CIENTISTAS"), icon("retweet")),
                    tags$br(),
                    tags$p("Tweets de assuntos populares. Essa métrica inclui RTs de perfis fora do Science Pulse e pode incluir tópicos além da ciência."),
                    tags$br(),
                    include_spinner_large_column(ns("rts12h_sample"))

                    )

             # Fechar tags$div
             )

    # Fechar tagList
    )

}

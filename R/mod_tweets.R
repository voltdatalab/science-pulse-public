#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULO - TWITTER - TABLE            #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_tweets_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      ##### PREPARAR BASE

      tweets_dataset <- reactive({

        # Carregar dados da procedure de tweets
        sql <- "select * from p_mod_tweets(?datum, ?datum2, ?language)"
        query  <- sqlInterpolate(base, sql, language = input$language,
                                 datum = input$date[1], datum2 = input$date[2])
        tweets <- as_tibble(dbGetQuery(base, query))

        # Atualiza o fuso-horario dos tweets e ordena por horario
        tweets$created_at <- tweets$created_at - hours(3)

        # Cria coluna is_replied
        tweets$is_replied <- ifelse(is.na(tweets$reply_to_screen_name), "FALSE", "TRUE")

        # Ordena pelo horario de criacao - sempre vir por ultimo
        tweets <- tweets %>% arrange(desc(created_at))

      })

      ##########
      # MENSAGEM COM DESCRICAO DA SELECAO

      output$basics <- renderText({

        # Importar os dados principais e corrigir variavel reply_to_screen_name
        user_n <- tweets_dataset()
        #user_n$is_replied <- ifelse(is.na(user_n$reply_to_screen_name), "FALSE", "TRUE")

        # Filtros: verificado, RTs e replies
        if (input$verified != "Todos os perfis") {
          user_n <- user_n[user_n$verified == input$verified,]
        }
        if (input$rts != "Mostrar") {
          user_n <- user_n[user_n$is_retweet == input$rts,]
        }
        if (input$reply != "Mostrar") {
          user_n <- user_n[user_n$is_replied == input$reply,]
        }

        # Descricao principal e printa a mensagem
        n_profiles <- user_n %>%
          summarise(count = n_distinct(screen_name))
        n_tweets <- user_n %>%
          summarise(count = n_distinct(status_id)) %>%
          mutate(count = format(count, big.mark=',', scientific=FALSE))

        paste("Seus filtros resultaram em", n_tweets, "tweets de", n_profiles$count, "perfis.")

      })

      ##########
      # TABELA PRINCIPAL

      output$table <- DT::renderDataTable(DT::datatable({

        # Importa os dados principais
        main_table <- tweets_dataset()

        # Filtros: verificado, RTs e replies
        if (input$verified != "Todos os perfis") {
          main_table <- main_table[main_table$verified == input$verified,]
        }
        if (input$rts != "Mostrar") {
          main_table <- main_table[main_table$is_retweet == input$rts,]
        }
        if (input$reply != "Mostrar") {
          main_table <- main_table[main_table$is_replied == input$reply,]
        }

        # Gera a tabela principal
        main_table <- main_table %>%
          # Cria as variaveis com links para o tweet e a conta
          mutate(link_tweet = if_else(is_retweet == "TRUE",
                                      paste0("<i class='fas fa-retweet fa-lg' style='color:#231f20'></i> ",
                                             text, "<a href='https://twitter.com/", screen_name,"/status/",
                                             status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>"),
                                      if_else(is_replied == "TRUE",
                                              paste0("<i class='fas fa-reply fa-lg' style='color:#231f20'></i> ",
                                                     text, "<a href='https://twitter.com/", screen_name,"/status/",
                                                     status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>"),
                                              paste0(text, "<a href='https://twitter.com/", screen_name, "/status/",
                                                     status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a></a>")))
                 ) %>%
          mutate(handle = if_else(verified == "TRUE",
                                  paste0(name,
                                         " <i class='fas fa-certificate fa-xs' style='color:rgba(29,161,242,1.00)'></i> <br><a href='https://twitter.com/",
                                         screen_name,"' target='_blank'>@", screen_name, "</a>"),
                                  paste0(name, "<br><a href='https://twitter.com/", screen_name,
                                         "' target='_blank'>@", screen_name, "</a>"))
                 ) %>%
          # Ajusta formatacao de RTs
          mutate(retweet_count = format(retweet_count, big.mark=',', scientific=FALSE)) %>%
          # Seleciona variaveis-base e renomeia as que vao aparecer
          select(name, verified, is_retweet, handle, screen_name, retweet_count, language,
                 created_at, text, link_tweet, status_id, reply_to_screen_name) %>%
          rename("Perfil" = handle, "Data" = created_at, "RTs" = retweet_count, "Tweet" = link_tweet)

        main_table

      }, escape = FALSE,

      # CONFIGURACOES GERAIS DA TABELA
      options = list(
        language = list(searchPlaceholder = "Busca por palavra-chave...", sSearch = ""),
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

      # Fecha DT::datatable
      ) %>%

        # Corrige a ordenacao e formato da variavel data
        formatDate("Data", "toLocaleString"))

    # Fecha modulo
    }

# Fecha server
)}

###################################################################################################

### UI

mod_tweets_ui <- function(id, i18n){

  ns <- NS(id)

  tagList(

    ### TITULO E DESCRICAO DA PAGINA
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),
             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("ENCONTRE TWEETS", style = "text-align:center"),
                      tags$p("Nesta página você pode buscar por tweets dentro do banco de dados do Science Pulse. É possível aplicar diversos tipos de filtros para refinar sua descoberta. A tabela é atualizada com novos tweets e contagens a cada 20 minutos -- por isso, dados sobre compartilhamentos e curtidas podem variar em relação aos tweets originais."),
                      tags$br(),
                      selectInput(inputId = ns("language"),
                                  label = tags$div(icon("language", class = "icons"), 'Escolha o idioma dos tweets'),
                                  c("Português" = "pt",
                                    "Inglês" = "en",
                                    "Espanhol" = "es"))
             )),

    ### COLUNAS A ESQUERDA: filtros
    tags$div(class="filters",

             column(3,

                    dateRangeInput(inputId = ns("date"),
                                   label = tags$div(icon("calendar", class = "icons"),
                                                    'Selecione datas (d/m/a)',
                                                    tags$p("Buscas estão limitadas a 90 dias",
                                                           style="font-weight:300")),
                                   start = Sys.Date()-3,  end = Sys.Date(),
                                   min = Sys.Date()-90,   max = Sys.Date(),
                                   format = "dd/mm/yyyy", weekstart = 0,
                                   language = "pt",       separator = " até ",
                                   width = NULL,          autoclose = TRUE),
                    tags$br(),

                    selectInput(inputId = ns("verified"),
                                label = tags$div(icon("certificate", class = "icons"),
                                                 'Filtrar por perfis verificados pelo Twitter'),
                                c("Todos os perfis",
                                  "Somente verificados" = "TRUE")),

                    selectInput(inputId = ns("rts"),
                                label = tags$div(icon("retweet", class = "icons"),
                                                 'Filtrar retweets'),
                                c("Mostrar",
                                  "Esconder" = "FALSE")),

                    selectInput(inputId = ns("reply"),
                                label = tags$div(icon("reply", class = "icons"),
                                                 'Filtrar respostas de posts'),
                                c("Mostrar",
                                 "Esconder" = "FALSE"))
             # Fecha coluna
             )

    # Fecha filtros
    ),

    ### TABELA PRINCIPAL
    column(5,
           tags$p(include_spinner_small(ns("basics"))),
           include_spinner_tables(ns("table"))
           )

    # Fecha TagList
    )

# Fecha UI
}

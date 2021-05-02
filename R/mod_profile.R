#########################################################################################
#########################################################################################
###################                                               #######################
###################            MODULO - TWITTER - PROFILES        #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_profile_server <- function(id, base, idioma) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      # CARREGAR DADOS DE EXPERTS
      experts <- as_tibble(dbGetQuery(base, "select name_clean,screen_name,user_id,
                                                    gender_pt,type_ontology_pt,race,
                                                    google_scholar,known_affiliations,
                                                    main_general_field_pt, not_male, not_white,
                                                    ver_location,verified,description,
                                                    below_median,followers_count from v_experts_form_data"))

      # Remove emojis e apara a variavel de nome
      experts$name <- gsub('\\p{So}|\\p{Cn}', trimws(''),
                           experts$name_clean, perl = TRUE)

      # Cria a variavel filtro_tipo_genero que sera usada no filtro
      experts <- experts %>%
        mutate(filtro_tipo_genero = ifelse(type_ontology_pt == "Pessoa",
                                           gender_pt, type_ontology_pt))

      ##########
      ### TABELA DE PERFIS
      output$profiles <- DT::renderDataTable(DT::datatable({

        # FILTROS: aplica os filtros na tabela principal
        experts_tbl <- experts %>%
          filter(followers_count > input$followers,
                 str_detect(filtro_tipo_genero, paste(c(input$type, input$gender), collapse = "|")),
                 str_detect(main_general_field_pt,   paste(input$field, collapse = "|")))

        if(input$verified == "TRUE"){

          experts_tbl <- experts_tbl %>%
            filter(verified == "TRUE")

          }

        # CRIA A TABELA PRINCIPAL
        experts_tbl <- experts_tbl %>%
          # Transforma a coluna nome para mostrar o selo de verificado
          # e linkar a conta do twitter pelo screen_name
          mutate(name = if_else(str_detect(verified, "TRUE"),
                                paste0(name, " <i class='fas fa-certificate fa-xs' style='color:rgba(29,161,242,1.00)'></i> <br><a href='https://twitter.com/",
                                       screen_name,"' target='_blank'>@", screen_name, "</a>"),
                                paste0(name, "<br><a href='https://twitter.com/", screen_name,"' target='_blank'>@", screen_name, "</a>"))
          ) %>%
          # Transforma a variavel de afiliacao institucional para linkar
          # o Google Scholar, quando existir
          mutate(known_affiliations = if_else(!is.na(google_scholar),
                                              paste0("<a href='", google_scholar, "' target='_blank'>", known_affiliations, "</a>"),
                                              known_affiliations)) %>%
          # Ordena em ordem alfabetica
          arrange(name) %>%
          # Coloca marcador de milhar na coluna de seguidores
          mutate(followers_count = format(followers_count, big.mark=',', scientific=FALSE)) %>%
          # Seleciona e renomeia as colunas que vao aparecer
          select(verified, google_scholar, below_median, screen_name, type_ontology_pt,
                 name, gender_pt, main_general_field_pt, known_affiliations,
                 description, ver_location, followers_count) %>%
          rename(nome = name,
                 tipo = type_ontology_pt,
                 `gênero` = gender_pt,
                 `grande área` = main_general_field_pt,
                 `afiliação institucional` = known_affiliations,
                 `descrição` = description,
                 `localização` = ver_location,
                 `n. de seguidores` = followers_count)

        experts_tbl

      }, escape = FALSE,

      # CONFIGURACOES GERAIS DA TABELA
      options = list(
        language = list(searchPlaceholder = "Busca por palavra-chave...", sSearch = ""),
        dom = "ftipr",
        pageLength = 100,
        searchHighlight = TRUE,
        info = FALSE,
        lengthMenu = list(c(10, 50, 100, 1000), c('10', '50', '100', '1000')),
        columnDefs = list(
          list(visible = FALSE, targets = c(1:5)),
          list(width = '150px', targets = c(2,7))

        # Fechar options
        ))

      # Fechar renderDataTable
      ))

      ##########
      ### CONTAGEM DE N. DE PERFIS NA SELECAO

      output$experts_count <- renderText({

        # Filtra a base principal
        experts_count <- experts %>%
          filter(followers_count > input$followers,
                 str_detect(filtro_tipo_genero, paste(c(input$type, input$gender), collapse = "|")),
                 str_detect(main_general_field_pt, paste(input$field, collapse = "|")))

        if(input$verified == "TRUE"){

          experts_count <- experts_count %>%
            filter(verified == "TRUE")

        }

        # Conta o n. de perfis
        experts_count <- experts_count %>%
          count()

        # Printa a mensagem
        paste("Há um total de", experts_count$n, "perfis do Twitter nesta seleção do nosso banco de dados.")

      })

      ##########
      ### ENCONTRO NOVOS ESPECIALISTAS
      #   Uma amostra aleatoria e estratificada de 5 dos perfis com menos seguidores na nossa base

      output$new_users <- renderTable({

        experts %>%
          # Seleciona somente aqueles com seguidores abaixo da mediana de followers_count
          filter(below_median == T) %>%
          # Cria uma variavel para estratifiar a base: nao-homem e nao-branco
          mutate(estratificacao = paste(not_male, not_white)) %>%
          # Sorteia um tweet por cada categoria
          group_by(estratificacao) %>%
          sample_n(size = case_when(
            estratificacao == "TRUE TRUE"   ~ 1,  # Nao-homem, nao-branco
            estratificacao == "FALSE TRUE"  ~ 1,  # Homem, nao-branco
            estratificacao == "TRUE FALSE"  ~ 1,  # Nao-homem, branco
            estratificacao == "FALSE FALSE" ~ 1,  # Homem, branco
            estratificacao == "NA NA"       ~ 1)  # Instituicao
            ) %>%
          ungroup() %>%
          # Ordena para aparecer as nao-homens nao-brancos primeiro
          arrange(desc(estratificacao)) %>%
          select(name, description, screen_name) %>%
          mutate(value_display = paste0("<h3 style='margin-top:0'><p><b>", name, "</b></p>",
                                        "@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #d91c5c'>",
                                        screen_name, "</a></h3>",
                                        "<p style='margin-botom:10px'><em>", description, "</em></p>")) %>%
          select(value_display) %>%
          rename(" " = value_display)

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

  # Fecha modulo
  }

# Fecha server
)}


###################################################################################################

### UI

mod_profile_ui <- function(id){

  ns <- NS(id)

  tagList(

    ### TITULO E DESCRICAO DA PAGINA
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),
             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("QUEM SEGUIMOS NO TWITTER", style = "text-align:center"),
                      tags$p("Nesta página é possível buscar pelos perfis de cientistas, experts, universidades, organizações e iniciativas que são núcleo do",
                             tags$b("Science Pulse."), "Você pode baixar esta tabela",
                      tags$a(href="https://docs.google.com/spreadsheets/d/11W4Sw3M4pJ12yolY03UsU_lUDjnuaOYLYcldy4xZPfg/edit?usp=sharing",
                             target="_blank", "aqui."),
                      tags$br()
                      ))),

    ### COLUNAS A ESQUERDA: filtros e novos especialistas
    tags$div(class="filters",
             column(3,

                    # Tipo
                    selectInput(inputId = ns("type"),
                                label = tags$div(icon("university", class = "icons"),
                                                 'Filtrar por tipo'),
                                choices = c("Instituição", "Pessoa"),
                                selected = "Instituição",
                                multiple = T),

                    # Genero
                    conditionalPanel(
                      condition = "input.type.indexOf('Pessoa') > -1", ns = ns,

                      selectInput(inputId = ns("gender"),
                                  label = tags$div(icon("user", class = "icons"),
                                                   'Filtrar por gênero'),
                                  choices = c("Mulher", "Homem", "Outro/a", "Não identificado"),
                                  multiple = T)

                      ),

                    # Grande area
                    selectInput(inputId = ns("field"),
                                label = tags$div(icon("graduation-cap", class = "icons"),
                                                 'Filtrar por grande área'),
                                choices = c("Ciências Biológicas", "Ciências Exatas",
                                            "Ciências Humanas", "Não especificado"),
                                selected = c("Ciências Biológicas", "Ciências Exatas",
                                             "Ciências Humanas", "Não especificado"),
                                multiple = T),

                    # Verificado
                    selectInput(inputId = ns("verified"),
                                label = tags$div(icon("certificate", class = "icons"),
                                                 'Filtrar por perfis verificados pelo Twitter'),
                                choices = c("Todos os perfis",
                                            "Somente verificados" = "TRUE")),

                    # N. minimo de seguidores
                    numericInput(inputId = ns("followers"),
                                 label = tags$div(icon("users", class = "icons"),
                                                  'Mínimo de seguidores'),
                                 value = 0, step = NA,
                                 min = 0, max = NA,
                                 width = "50px"),

                    # Encontre novos especialistas
                    tags$br(),
                    tags$p(tags$b("Encontre novos especialistas"), style = "text-align:center"),
                    include_spinner_thin_column(ns("new_users"))
                    )
             ),

    ### TABELA PRINCIPAL
    column(5,
           include_spinner_small(ns("experts_count")),
           tags$br(),
           include_spinner_tables(ns("profiles"))
           )

  )
}

# =========================================================
# DASHBOARD DE EVASÃO ACADÊMICA — UFES CCE
# =========================================================
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(bsicons)
library(data.table)
library(shinydashboard)
library(rsconnect)

# =========================================================
# 1. Carregar dados
# =========================================================
dados <- read_delim("quantidade_alunos_evadidos_x_sexo.csv", 
                    delim = ";", escape_double = FALSE, 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE, show_col_types = FALSE)
dados <- subset(dados, select = -QTDE)

dados <- dados %>%
  mutate(
    FORMA_EVASAO_limpa = case_when(
      # ---- Desligamentos ----
      grepl("Desligamento", FORMA_EVASAO, ignore.case = TRUE) ~ "Desligamento",
      grepl("Abandono", FORMA_EVASAO, ignore.case = TRUE) ~ "Desligamento",
      grepl("Nulidade", FORMA_EVASAO, ignore.case = TRUE) ~ "Desligamento",
      grepl("desativada", FORMA_EVASAO, ignore.case = TRUE) ~ "Desligamento",
      
      # ---- Desistências ----
      grepl("Desist", FORMA_EVASAO, ignore.case = TRUE) ~ "Desistência",
      grepl("Aband", FORMA_EVASAO, ignore.case = TRUE) ~ "Desistência",
      
      # ---- Transferências (exceto interna) ----
      grepl("SISU", FORMA_EVASAO, ignore.case = TRUE) ~ "Transferência",
      grepl("Transfer", FORMA_EVASAO, ignore.case = TRUE) & 
        !grepl("Interna", FORMA_EVASAO, ignore.case = TRUE) ~ "Transferência",
      
      # ---- Transferência interna ----
      grepl("Reopção", FORMA_EVASAO, ignore.case = TRUE) ~ "Transferência Interna",
      grepl("Interna", FORMA_EVASAO, ignore.case = TRUE) ~ "Transferência Interna",
      
      # ---- Formado ----
      grepl("Formado", FORMA_EVASAO, ignore.case = TRUE) ~ "Formado",
      grepl("Conclusão", FORMA_EVASAO, ignore.case = TRUE) ~ "Formado",
      
      grepl("Adaptação Curricular", FORMA_EVASAO, ignore.case = TRUE) ~ "Adaptação Curricular",
      
      # ---- Não informado ----
      grepl("Não Informado", FORMA_EVASAO, ignore.case = TRUE) ~ "Não Informado",
      
      # ---- Outras situações ----
      TRUE ~ "Outros"
    )
  )

dados <- dados %>%
  pivot_longer(
    cols = c(ET_BRANCA, ET_PRETA, ET_AMARELA, ET_PARDA, ET_INDIGINA, ET_NAO_INFORMADO, ET_NAO_DECLARADA),
    names_to = "ETNIA",
    values_to = "QTDE"
  )

setDT(dados)

dados[, ETNIA := gsub("^ET_", "", ETNIA)]
dados[, ETNIA := gsub("_", " ", ETNIA)]  
dados[, ETNIA := tools::toTitleCase(tolower(ETNIA))] # primeira maiúscula, resto minúsculo

dados <- dados %>%
  mutate(
    SEXO_limpo = case_when(
      # ---- Desligamentos ----
      grepl("M", SEXO, ignore.case = TRUE) ~ "Masculino",
      grepl("F", SEXO, ignore.case = TRUE) ~ "Feminino"
    )
)

dados <- dados %>%
  rename_with(~ gsub("\\s+", "_", .x)) %>%
  mutate(
    COTISTA = as.factor(COTISTA),
    SEXO = as.factor(SEXO),
    ANO_EVASAO = as.numeric(ANO_EVASAO),
    PERIODO_EVASAO = as.factor(PERIODO_EVASAO),
    FORMA_EVASAO = FORMA_EVASAO_limpa,
    SEXO = SEXO_limpo
  )

# =========================================================
# 2. Interface
# =========================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Dashboard Evasão"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "visao_geral", icon = icon("chart-bar")),
      menuItem("Por Curso", tabName = "por_curso", icon = icon("chart-bar")),
      menuItem("Perfil dos Evadidos", tabName = "perfil", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "visao_geral",
              fluidRow(
                box(width = 2,
                    selectInput(inputId = "curso", label = "Selecione o curso", 
                                choices = c("Todos", sort(unique(dados$NOME_CURSO))), 
                                selected = 1)),
                box(width = 2,
                    selectInput(inputId = "sexo", label = "Selecione o sexo", 
                                choices = c("Todos", sort(unique(dados$SEXO))), 
                                selected = 1)),
                valueBoxOutput(width = 2, outputId = "card_total"),
                valueBoxOutput(width = 2, outputId = "card_cotistas"),
                valueBoxOutput(width = 2, outputId = "card_mulheres"),
                valueBoxOutput(width = 2, outputId = "forma_pico")
              ),
              fluidRow(box(sliderInput("ano_range", "Período de Evasão (anos):",
                                       min = min(dados$ANO_EVASAO, na.rm = TRUE),
                                       max = max(dados$ANO_EVASAO, na.rm = TRUE),
                                       value = c(min(dados$ANO_EVASAO, na.rm = TRUE),
                                                 max(dados$ANO_EVASAO, na.rm = TRUE)),
                                       sep = "")),
                       valueBoxOutput(width = 2, outputId = "card_total_evadido"),
                       valueBoxOutput(width = 2, outputId = "card_total_formado"),
                       valueBoxOutput(width = 2, outputId = "card_ratio_evasao_formado")
                       ),
              fluidRow(
                box(width = 12, 
                    plotlyOutput(outputId = "plot_evasao_ano"))),
              fluidRow(
                box(width = 12, 
                    plotlyOutput(outputId = "plot_evasao_ano_linhas"))),
              fluidRow(
                box(
                  plotlyOutput(outputId = "plot_forma_evasao")),
                box(
                  plotlyOutput(outputId = "plot_evasao_semestre"))
                
              )
      ),
      tabItem(
        tabName = "por_curso",
        fluidRow(
          box(width = 2,
              selectInput(inputId = "curso", label = "Selecione o curso", 
                          choices = c("Todos", sort(unique(dados$NOME_CURSO))), 
                          selected = 1)),
          box(width = 2,
              selectInput(inputId = "sexo", label = "Selecione o sexo", 
                          choices = c("Todos", sort(unique(dados$SEXO))), 
                          selected = 1)),
          valueBoxOutput(width = 2, outputId = "card_total1"),
          valueBoxOutput(width = 2, outputId = "card_cotistas1"),
          valueBoxOutput(width = 2, outputId = "card_mulheres1"),
          valueBoxOutput(width = 2, outputId = "forma_pico1")
        ),
        fluidRow(box(sliderInput("ano_range", "Período de Evasão (anos):",
                                 min = min(dados$ANO_EVASAO, na.rm = TRUE),
                                 max = max(dados$ANO_EVASAO, na.rm = TRUE),
                                 value = c(min(dados$ANO_EVASAO, na.rm = TRUE),
                                           max(dados$ANO_EVASAO, na.rm = TRUE)),
                                 sep = "")),
                 valueBoxOutput(width = 2, outputId = "card_total_evadido1"),
                 valueBoxOutput(width = 2, outputId = "card_total_formado1"),
                 valueBoxOutput(width = 2, outputId = "card_ratio_evasao_formado1")
        ),
        fluidRow(
          box(width = 12, 
            plotlyOutput(outputId = "plot_evasao_curso"))
        ),
        fluidRow(
          box(width = 12,
              plotlyOutput(outputId = "plot_total_por_curso"))
        ),
        fluidRow(
          box(width = 12,
              plotlyOutput(outputId = "plot_fomados_por_curso"))
        )
      ),
      tabItem(
        tabName = "perfil",
        fluidRow(
          box(width = 2,
              selectInput(inputId = "curso", label = "Selecione o curso", 
                          choices = c("Todos", sort(unique(dados$NOME_CURSO))), 
                          selected = 1)),
          box(width = 2,
              selectInput(inputId = "sexo", label = "Selecione o sexo", 
                          choices = c("Todos", sort(unique(dados$SEXO))), 
                          selected = 1)),
          valueBoxOutput(width = 2, outputId = "card_total2"),
          valueBoxOutput(width = 2, outputId = "card_cotistas2"),
          valueBoxOutput(width = 2, outputId = "card_mulheres2"),
          valueBoxOutput(width = 2, outputId = "forma_pico2")
        ),
        fluidRow(box(sliderInput("ano_range", "Período de Evasão (anos):",
                                 min = min(dados$ANO_EVASAO, na.rm = TRUE),
                                 max = max(dados$ANO_EVASAO, na.rm = TRUE),
                                 value = c(min(dados$ANO_EVASAO, na.rm = TRUE),
                                           max(dados$ANO_EVASAO, na.rm = TRUE)),
                                 sep = "")),
                 valueBoxOutput(width = 2, outputId = "card_total_evadido2"),
                 valueBoxOutput(width = 2, outputId = "card_total_formado2"),
                 valueBoxOutput(width = 2, outputId = "card_ratio_evasao_formado2")
        ),
        fluidRow(
          box(
            plotlyOutput(outputId = "plot_sexo")),
          box(
            plotlyOutput(outputId = "plot_cotista"))
        ),
        fluidRow(
          box(width = 5,
            plotlyOutput(outputId = "plot_evasao_etnia")),
          box(width = 7, 
            plotlyOutput(outputId = "plot_evasao_ano_sexo")))
      )
    )
  )
)




# =========================================================
# 3. Servidor
# =========================================================
server <- function(input, output, session) {
  
  # Dados filtrados com segurança
  dados_filtrados <- reactive({
    req(dados)
    d <- dados
    if (!is.null(input$curso) && input$curso != "Todos") d <- d %>% filter(NOME_CURSO == input$curso)
    if (!is.null(input$sexo) && input$sexo != "Todos") d <- d %>% filter(SEXO == input$sexo)
    if (!is.null(input$ano_range) && length(input$ano_range) == 2) {
      d <- d %>% filter(ANO_EVASAO >= input$ano_range[1],
                        ANO_EVASAO <= input$ano_range[2])}
    if (!is.null(input$somente_cotistas) && isTRUE(input$somente_cotistas)) d <- d %>% filter(COTISTA == "S")
    if (!is.null(input$somente_ampla) && isTRUE(input$somente_ampla)) d <- d %>% filter(COTISTA == "N")
    d
  })
  
  #  ------------------ Cards ------------------
  # Página - Geral
  output$card_ratio_evasao_formado <- renderValueBox({
    
    d <- dados_filtrados()
    coluna_categoria <- "FORMA_EVASAO" 
    
    categorias_excluir <- c("Formado", "Adaptação Curricular")
    
    d_evadidos <- d[!(d[[coluna_categoria]] %in% categorias_excluir), ]
    
    total_evadido <- sum(d_evadidos$QTDE, na.rm = TRUE)
    
    d_formado <- d[d[[coluna_categoria]] == "Formado", ]
    
    total_formado <- sum(d_formado$QTDE, na.rm = TRUE)
    
    if (total_formado > 0 && total_evadido > 0) {
      ratio <-  100 * total_formado / total_evadido
      valor_display <- ratio
      subtitulo <- "Formados / Evadidos"
    } else {
      valor_display <- 0
      subtitulo <- "Razão (0 Formados)"
    }
    
    valueBox(
      paste0(round(valor_display, 2), "%"), # O número calculado
      subtitulo,                            # Texto abaixo do número
      icon = icon("balance-scale-right"),   # Ícone sugerido para razão
      color = "purple"                      # Cor diferente
    )
  })

  output$card_total_evadido <- renderValueBox({
    d <- dados_filtrados()
    
    categorias_excluir <- c("Formado", "Adaptação Curricular")
    
    d_evadidos <- d[!(d$FORMA_EVASAO %in% categorias_excluir), ]
    
    total <- sum(d_evadidos$QTDE, na.rm = TRUE)
    
    valueBox(
      format(total, big.mark = ".", decimal.mark = ","), 
      "Alunos Evadidos (Real)", # Título sugerido
      icon = icon("user-minus"),    # Ícone sugerido para evasão
      color = "yellow"              # Cor sugerida para alerta (evasão)
    )
  })
  
  output$card_total <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), # número formatado
    "Alunos Evadidos",                            # texto abaixo do número
    icon = icon("boxes"),                              # ícone opcional (pode trocar)
    color = "green"                                    # cor do box
  )
})

  output$card_total_formado <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE[d$FORMA_EVASAO == "Formado"], na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), # número formatado
    "Alunos Formados",                            # texto abaixo do número
    icon = icon("user-graduate"),                              # ícone opcional (pode trocar)
    color = "green"                                    # cor do box
  )
})

  output$card_cotistas <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  perc_cotistas <- if (total == 0) 0 else 100 * sum(d$QTDE[d$COTISTA == "S"], na.rm = TRUE) / total
  
  valueBox(
    paste0(round(perc_cotistas, 1), "%"),
    "Cotistas",
    icon = icon("user-friends"),
    color = "blue"
  )
})

  output$card_mulheres <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  perc_mulheres <- if (total == 0) 0 else 100 * sum(d$QTDE[d$SEXO == "Feminino"], na.rm = TRUE) / total
  
  valueBox(
    paste0(round(perc_mulheres, 1), "%"),
    "Mulheres",
    icon = icon("venus"),
    color = "purple"
  )
})

  output$forma_pico <- renderValueBox({
  df <- dados_filtrados() %>%
    group_by(FORMA_EVASAO) %>%
    summarise(Total = sum(QTDE)) %>%
    arrange(desc(Total))
  
  forma_principal <- if (nrow(df) > 0) df$FORMA_EVASAO[1] else "Sem dados"
  
  valueBox(
    tagList(
      tags$h3(style = "font-size:20px; line-height:1.1; margin:0;", forma_principal)
    ),
    subtitle = "Forma de Evasão Mais Frequente",  # aqui é o segundo argumento
    icon = icon("chart-line"),
    color = "yellow"
  )
})

  # Página - Por curso

  output$card_total1 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), # número formatado
    "Alunos Evadidos",                            # texto abaixo do número
    icon = icon("boxes"),                              # ícone opcional (pode trocar)
    color = "green"                                    # cor do box
  )
})
  
  output$card_total_formado1 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE[d$FORMA_EVASAO == "Formado"], na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), # número formatado
    "Alunos Formados",                            # texto abaixo do número
    icon = icon("user-graduate"),                              # ícone opcional (pode trocar)
    color = "green"                                    # cor do box
  )
})
  
  output$card_ratio_evasao_formado1 <- renderValueBox({
  
  d <- dados_filtrados()
  coluna_categoria <- "FORMA_EVASAO" 
  
  categorias_excluir <- c("Formado", "Adaptação Curricular")
  
  d_evadidos <- d[!(d[[coluna_categoria]] %in% categorias_excluir), ]
  
  total_evadido <- sum(d_evadidos$QTDE, na.rm = TRUE)
  
  d_formado <- d[d[[coluna_categoria]] == "Formado", ]
  
  total_formado <- sum(d_formado$QTDE, na.rm = TRUE)
  
  if (total_formado > 0 && total_evadido > 0) {
    ratio <-  100 * total_formado / total_evadido
    valor_display <- ratio
    subtitulo <- "Formados / Evadidos"
  } else {
    valor_display <- 0
    subtitulo <- "Razão (0 Formados)"
  }
  
  valueBox(
    paste0(round(valor_display, 2), "%"), # O número calculado
    subtitulo,                            # Texto abaixo do número
    icon = icon("balance-scale-right"),   # Ícone sugerido para razão
    color = "purple"                      # Cor diferente
  )
})

  output$card_total_evadido1 <- renderValueBox({
  d <- dados_filtrados()
  
  categorias_excluir <- c("Formado", "Adaptação Curricular")
  
  d_evadidos <- d[!(d$FORMA_EVASAO %in% categorias_excluir), ]
  
  total <- sum(d_evadidos$QTDE, na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), 
    "Alunos Evadidos (Real)", # Título sugerido
    icon = icon("user-minus"),    # Ícone sugerido para evasão
    color = "yellow"              # Cor sugerida para alerta (evasão)
  )
})

  output$card_cotistas1 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  perc_cotistas <- if (total == 0) 0 else 100 * sum(d$QTDE[d$COTISTA == "S"], na.rm = TRUE) / total
  
  valueBox(
    paste0(round(perc_cotistas, 1), "%"),
    "Cotistas",
    icon = icon("user-friends"),
    color = "blue"
  )
})

  output$card_mulheres1 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  perc_mulheres <- if (total == 0) 0 else 100 * sum(d$QTDE[d$SEXO == "Feminino"], na.rm = TRUE) / total
  
  valueBox(
    paste0(round(perc_mulheres, 1), "%"),
    "Mulheres",
    icon = icon("venus"),
    color = "purple"
  )
})

  output$forma_pico1 <- renderValueBox({
  df <- dados_filtrados() %>%
    group_by(FORMA_EVASAO) %>%
    summarise(Total = sum(QTDE)) %>%
    arrange(desc(Total))
  
  forma_principal <- if (nrow(df) > 0) df$FORMA_EVASAO[1] else "Sem dados"
  
  valueBox(
    tagList(
      tags$h3(style = "font-size:20px; line-height:1.1; margin:0;", forma_principal)
    ),
    subtitle = "Forma de Evasão Mais Frequente",  # aqui é o segundo argumento
    icon = icon("chart-line"),
    color = "yellow"
  )
})
  
  # Página - Pefil dos Evadidos

  output$card_total2 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), # número formatado
    "Alunos Evadidos",                            # texto abaixo do número
    icon = icon("boxes"),                              # ícone opcional (pode trocar)
    color = "green"                                    # cor do box
  )
})
  
  output$card_total_formado2 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE[d$FORMA_EVASAO == "Formado"], na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), # número formatado
    "Alunos Formados",                            # texto abaixo do número
    icon = icon("user-graduate"),                              # ícone opcional (pode trocar)
    color = "green"                                    # cor do box
  )
})

  output$card_ratio_evasao_formado2 <- renderValueBox({
  
  d <- dados_filtrados()
  coluna_categoria <- "FORMA_EVASAO" 
  
  categorias_excluir <- c("Formado", "Adaptação Curricular")
  
  d_evadidos <- d[!(d[[coluna_categoria]] %in% categorias_excluir), ]
  
  total_evadido <- sum(d_evadidos$QTDE, na.rm = TRUE)
  
  d_formado <- d[d[[coluna_categoria]] == "Formado", ]
  
  total_formado <- sum(d_formado$QTDE, na.rm = TRUE)
  
  if (total_formado > 0 && total_evadido > 0) {
    ratio <-  100 * total_formado / total_evadido
    valor_display <- ratio
    subtitulo <- "Formados / Evadidos"
  } else {
    valor_display <- 0
    subtitulo <- "Razão (0 Formados)"
  }
  
  valueBox(
    paste0(round(valor_display, 2), "%"), # O número calculado
    subtitulo,                            # Texto abaixo do número
    icon = icon("balance-scale-right"),   # Ícone sugerido para razão
    color = "purple"                      # Cor diferente
  )
})

  output$card_total_evadido2 <- renderValueBox({
  d <- dados_filtrados()
  
  categorias_excluir <- c("Formado", "Adaptação Curricular")
  
  d_evadidos <- d[!(d$FORMA_EVASAO %in% categorias_excluir), ]
  
  total <- sum(d_evadidos$QTDE, na.rm = TRUE)
  
  valueBox(
    format(total, big.mark = ".", decimal.mark = ","), 
    "Alunos Evadidos (Real)", # Título sugerido
    icon = icon("user-minus"),    # Ícone sugerido para evasão
    color = "yellow"              # Cor sugerida para alerta (evasão)
  )
})

  output$card_cotistas2 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  perc_cotistas <- if (total == 0) 0 else 100 * sum(d$QTDE[d$COTISTA == "S"], na.rm = TRUE) / total
  
  valueBox(
    paste0(round(perc_cotistas, 1), "%"),
    "Cotistas",
    icon = icon("user-friends"),
    color = "blue"
  )
})

  output$card_mulheres2 <- renderValueBox({
  d <- dados_filtrados()
  total <- sum(d$QTDE, na.rm = TRUE)
  
  perc_mulheres <- if (total == 0) 0 else 100 * sum(d$QTDE[d$SEXO == "Feminino"], na.rm = TRUE) / total
  
  valueBox(
    paste0(round(perc_mulheres, 1), "%"),
    "Mulheres",
    icon = icon("venus"),
    color = "purple"
  )
})

  output$forma_pico2 <- renderValueBox({
  df <- dados_filtrados() %>%
    group_by(FORMA_EVASAO) %>%
    summarise(Total = sum(QTDE)) %>%
    arrange(desc(Total))
  
  forma_principal <- if (nrow(df) > 0) df$FORMA_EVASAO[1] else "Sem dados"
  
  valueBox(
    tagList(
      tags$h3(style = "font-size:20px; line-height:1.1; margin:0;", forma_principal)
    ),
    subtitle = "Forma de Evasão Mais Frequente",  # aqui é o segundo argumento
    icon = icon("chart-line"),
    color = "yellow"
  )
})

  #  ------------------ Gráficos ------------------
  # Página - Geral
  output$plot_evasao_ano <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(ANO_EVASAO) %>%
      summarise(total = sum(QTDE)) %>%
      ggplot(aes(x = ANO_EVASAO, y = total)) +
      geom_line(linewidth = 1.2, color = "#2E86AB") +
      geom_point(size = 2, color = "#117A65") +
      labs(title = "Evasão ao longo dos anos", x = "Ano", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_forma_evasao <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(FORMA_EVASAO) %>%
      summarise(total = sum(QTDE)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(FORMA_EVASAO, total), y = total, fill = FORMA_EVASAO)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Formas de evasão", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_evasao_ano_linhas <- renderPlotly({
    
    # --- 1. Assunções Importantes ---
    # !! AJUSTE AQUI: Troque "FORMA_EVASAO" pelo nome exato da sua coluna
    coluna_categoria <- "FORMA_EVASAO" 
    
    # !! AJUSTE AQUI: Troque "ANO_EVASAO" pelo nome exato da sua coluna de ano
    coluna_ano <- "ANO_EVASAO"
    
    # --- 2. Preparação dos Dados ---
    dados_para_grafico <- dados_filtrados() %>%
      # Agrupa por ano
      group_by(!!sym(coluna_ano)) %>%
      
      # Sumariza criando TRÊS colunas:
      summarise(
        # 1. Apenas "Formado"
        Formados = sum(QTDE[!!sym(coluna_categoria) == "Formado"], na.rm = TRUE),
        
        # 2. "Evadidos (Real)" -> Exclui "Formado" e "Adaptação Curricular"
        `Evadidos (Real)` = sum(QTDE[!(!!sym(coluna_categoria) %in% c("Formado", "Adaptação Curricular"))], na.rm = TRUE),
        
        # 3. "Evadidos (Total)" -> Exclui APENAS "Formado"
        `Evadidos (Total)` = sum(QTDE, na.rm = TRUE)
      ) %>%
      
      # Converte de formato "largo" para "longo"
      tidyr::pivot_longer(
        cols = c("Formados", "Evadidos (Real)", "Evadidos (Total)"), # As colunas que virarão linhas
        names_to = "Categoria",          # Nova coluna com os nomes
        values_to = "total"              # Nova coluna com os valores
      )
    
    # --- 3. Criação do Gráfico ---
    p <- ggplot(dados_para_grafico, aes(x = !!sym(coluna_ano), y = total, color = Categoria)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      
      # Define cores manuais para as três categorias
      scale_color_manual(values = c(
        "Formados" = "#28A745",          # Verde (Sucesso)
        "Evadidos (Real)" = "#FFC107",  # Amarelo (Alerta filtrado)
        "Evadidos (Total)" = "#DC3545"  # Vermelho (Alerta total)
      )) +
      
      labs(
        title = "Formados vs. Evadidos ao longo dos anos", 
        x = "Ano", 
        y = "Quantidade de Alunos",
        color = "Legenda" # Altera o título da legenda
      ) +
      theme_minimal(base_size = 14)
    
    # Converte para Plotly
    ggplotly(p)
  })
  
  output$plot_evasao_semestre <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(PERIODO_EVASAO) %>%
      summarise(total = sum(QTDE)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(PERIODO_EVASAO, total), y = total, fill = PERIODO_EVASAO)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Evasão por Semestre", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  # Página - Por curso
  
  output$plot_evasao_curso <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(NOME_CURSO) %>%
      summarise(total = sum(QTDE)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(NOME_CURSO, total), y = total, fill = NOME_CURSO)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Evasão por Curso", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_total_por_curso <- renderPlotly({
    
    # --- 1. Assunções Importantes ---
    coluna_ano <- "ANO_EVASAO"
    coluna_curso <- "NOME_CURSO" 
    coluna_categoria <- "FORMA_EVASAO" 
    
    # --- 2. Preparação dos Dados ---
    dados_para_grafico <- dados_filtrados() %>%
      group_by(!!sym(coluna_ano), !!sym(coluna_curso)) %>%
      summarise(total_geral = sum(QTDE[!(!!sym(coluna_categoria) %in% c("Formado", "Adaptação Curricular"))], na.rm = TRUE)) %>%
      ungroup() 
    
    # --- 3. Criação do Gráfico ---
    p <- ggplot(dados_para_grafico, aes(
      x = !!sym(coluna_ano), 
      y = total_geral, 
      color = !!sym(coluna_curso),
      
      group = !!sym(coluna_curso), 
      
      text = paste("Curso:", !!sym(coluna_curso),
                   "<br>Ano:", !!sym(coluna_ano),
                   "<br>Total:", total_geral)
    )) +
      geom_line(linewidth = 1.2) + 
      geom_point(size = 2) +
      
      labs(
        title = "Evasão por Curso ao longo dos anos", 
        x = "Ano", 
        y = "Quantidade Total (Geral)",
        color = "Curso" 
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })

  output$plot_fomados_por_curso <- renderPlotly({
    
    # --- 1. Assunções Importantes ---
    coluna_ano <- "ANO_EVASAO"
    coluna_curso <- "NOME_CURSO" 
    coluna_categoria <- "FORMA_EVASAO" 
    
    # --- 2. Preparação dos Dados ---
    dados_para_grafico <- dados_filtrados() %>%
      group_by(!!sym(coluna_ano), !!sym(coluna_curso)) %>%
      summarise(total_geral = sum(QTDE[!!sym(coluna_categoria) == "Formado"], na.rm = TRUE)) %>%
      ungroup() 
    
    # --- 3. Criação do Gráfico ---
    p <- ggplot(dados_para_grafico, aes(
      x = !!sym(coluna_ano), 
      y = total_geral, 
      color = !!sym(coluna_curso),
      
      group = !!sym(coluna_curso), 
      
      text = paste("Curso:", !!sym(coluna_curso),
                   "<br>Ano:", !!sym(coluna_ano),
                   "<br>Total:", total_geral)
    )) +
      geom_line(linewidth = 1.2) + 
      geom_point(size = 2) +
      
      labs(
        title = "Formados por Curso ao longo dos anos", 
        x = "Ano", 
        y = "Quantidade Total (Geral)",
        color = "Curso" 
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Página - Pefil dos Evadidos
  
  output$plot_sexo <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(SEXO) %>%
      summarise(total = sum(QTDE)) %>%
      ggplot(aes(x = SEXO, y = total, fill = SEXO)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Distribuição por sexo", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_cotista <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(COTISTA) %>%
      summarise(total = sum(QTDE)) %>%
      ggplot(aes(x = COTISTA, y = total, fill = COTISTA)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Cotistas vs Não Cotistas", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_evasao_etnia <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(ETNIA) %>%
      summarise(total = sum(QTDE)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(ETNIA, total), y = total, fill = ETNIA)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Distribuição por grupo étnico", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_evasao_ano_sexo <- renderPlotly({
    
    # --- 1. Assunções Importantes ---
    # !! AJUSTE AQUI: Troque "FORMA_EVASAO" pelo nome exato da sua coluna
    coluna_categoria <- "FORMA_EVASAO" 
    
    # !! AJUSTE AQUI: Troque "ANO_EVASAO" pelo nome exato da sua coluna de ano
    coluna_ano <- "ANO_EVASAO"
    coluna_sexo <- "SEXO"
    
    # --- 2. Preparação dos Dados ---
    dados_para_grafico <- dados_filtrados() %>%
      # Agrupa por ano
      group_by(!!sym(coluna_ano)) %>%
      
      # Sumariza criando TRÊS colunas:
      summarise(
        # 1. Apenas "Formado"
        Feminino = sum(QTDE[!!sym(coluna_sexo) == "Feminino"], na.rm = TRUE),
        
        # 2. "Evadidos (Real)" -> Exclui "Formado" e "Adaptação Curricular"
        Masculino = sum(QTDE[!!sym(coluna_sexo) == "Masculino"], na.rm = TRUE),
        
        # 3. "Evadidos (Total)" -> Exclui APENAS "Formado"
        `Evadidos (Total)` = sum(QTDE, na.rm = TRUE)
      ) %>%
      
      # Converte de formato "largo" para "longo"
      tidyr::pivot_longer(
        cols = c("Feminino", "Masculino", "Evadidos (Total)"), # As colunas que virarão linhas
        names_to = "Categoria",          # Nova coluna com os nomes
        values_to = "total"              # Nova coluna com os valores
      )
    
    # --- 3. Criação do Gráfico ---
    p <- ggplot(dados_para_grafico, aes(x = !!sym(coluna_ano), y = total, color = Categoria)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      
      # Define cores manuais para as três categorias
      scale_color_manual(values = c(
        "Feminino" = "#28A745",          # Verde (Sucesso)
        "Masculino" = "#FFC107",  # Amarelo (Alerta filtrado)
        "Evadidos (Total)" = "#DC3545"  # Vermelho (Alerta total)
      )) +
      
      labs(
        title = "Evasão por sexo ao longo dos anos", 
        x = "Ano", 
        y = "Quantidade de Alunos",
        color = "Legenda" # Altera o título da legenda
      ) +
      theme_minimal(base_size = 14)
    
    # Converte para Plotly
    ggplotly(p)
  })
  
}

# =========================================================
# 4. Executar App
# =========================================================
shinyApp(ui, server)



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

# =========================================================
# 1. Carregar dados
# =========================================================
dados <- read_delim("quantidade_alunos_evadidos_x_curso2011_2025.csv", 
                    delim = ";", escape_double = FALSE, 
                    locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE, show_col_types = FALSE)
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
  rename_with(~ gsub("\\s+", "_", .x)) %>%
  mutate(
    COTISTA = as.factor(COTISTA),
    SEXO = as.factor(SEXO),
    ANO_EVASAO = as.numeric(ANO_EVASAO),
    PERIODO_EVASAO = as.factor(PERIODO_EVASAO),
    FORMA_EVASAO = FORMA_EVASAO_limpa
  )

dados_etnia <- dados %>%
  pivot_longer(cols = starts_with("ET_"), 
               names_to = "Etnia", 
               values_to = "Qtd_Evadidos") %>%
  filter(Qtd_Evadidos > 0)

# =========================================================
# 2. Interface
# =========================================================
ui <- page_fillable(
  theme = bs_theme(
    base_font = font_google("Poppins"),
    heading_font = font_google("Montserrat")
  ),
  
  titlePanel("Evasão UFES-CCE"),
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      selectInput("area", "Área de Conhecimento:", 
                  choices = c("Todas", sort(unique(dados$AREA_CONHECIMENTO)))),
      
      selectInput("curso", "Curso:", 
                  choices = c("Todos", sort(unique(dados$NOME_CURSO)))),
      
      sliderInput("ano_range", "Período de Evasão (anos):",
                  min = min(dados$ANO_EVASAO, na.rm = TRUE),
                  max = max(dados$ANO_EVASAO, na.rm = TRUE),
                  value = c(min(dados$ANO_EVASAO, na.rm = TRUE),
                            max(dados$ANO_EVASAO, na.rm = TRUE)),
                  sep = ""),
      hr(),
      checkboxGroupInput("cotas",
                         "Selecione a cota:",
                         choices = c("Cotista" = "S",
                                     "Ampla Concorrência" = "N"),
                         selected = c("S", "N")),
      hr(),
      checkboxGroupInput("sexo",
                         "Selecione o Genêro",
                         choices = c("Homem" = "M",
                                     "Mulher" = "F",
                                     "Não Identificado" = "N"),
                         selected = c("M", "F")),
      # hr(),
      checkboxGroupInput("etnias", 
                         "Selecione as Etnias", 
                         choices = c("Branca" = "ET_BRANCA", 
                                     "Preta" = "ET_PRETA", 
                                     "Amarela" = "ET_AMARELA", 
                                     "Parda" = "ET_PARDA", 
                                     "Indígena" = "ET_INDIGINA", 
                                     "Não Informado" = "ET_NAO_INFORMADO", 
                                     "Não Declarada" = "ET_NAO_DECLARADA"), 
                         selected = c("ET_BRANCA", "ET_PRETA", "ET_AMARELA", "ET_PARDA", "ET_INDIGINA")),
      hr(),
      helpText("Use os filtros acima para atualizar as visualizações.")
    ),
    page_navbar(
      nav_panel(title = "Visão Geral", 
                layout_column_wrap(
                  width = "250px",
                  fill = FALSE,
                  value_box(
                    title = h5("Total de Evadidos"),
                    value = h3(textOutput("card_total")),
                    showcase = bsicons::bs_icon("people-fill")
                  ),
                  value_box(
                    title = h5("% Cotistas"),
                    value = h3(textOutput("card_cotistas")),
                    showcase = bsicons::bs_icon("person-standing")
                  ),
                  value_box(
                    title = h5("% Mulheres"),
                    value = h3(textOutput("card_mulheres")),
                    showcase = bsicons::bs_icon("person-standing-dress")
                  ),
                  value_box(
                    title = h5("Principal Forma de Evasão"),
                    value = h3(textOutput("forma_pico")),
                    showcase = bsicons::bs_icon("box-arrow-right")
                  ),
                ),
                    plotlyOutput("plot_evasao_ano"),
                    plotlyOutput("plot_forma_evasao")
      ),
      nav_panel(title = "Por Curso", 
                plotlyOutput("plot_evasao_curso", height = "600px")
                ),
      nav_panel(title = "Perfil dos Evadidos", 
                fluidRow(
                  column(6, plotlyOutput("plot_sexo")),
                  column(6, plotlyOutput("plot_cotista"))
                ),
                plotlyOutput("plot_etnia", height = "500px")
                ),
      nav_panel(title = "Tabela Detalhada", 
                DTOutput("tabela")),
      nav_panel(title = "Dicionário",
                textOutput("asd")),
      nav_spacer(),
      nav_menu(
        title = "Links",
      )
    )
  )
)

# =========================================================
# 3. Servidor
# =========================================================
server <- function(input, output, session) {
  
  # Atualiza lista de cursos dinamicamente
  observe({
    cursos <- dados %>%
      filter(AREA_CONHECIMENTO == input$area | input$area == "Todas") %>%
      pull(NOME_CURSO) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "curso", choices = c("Todos", cursos))
  })
  
  dados_filtrados <- reactive({
    req(dados_etnia)
    d <- dados_etnia
    
    # Área
    if (input$area != "Todas") {
      d <- d %>% filter(AREA_CONHECIMENTO == input$area)
    }
    
    # Curso
    if (input$curso != "Todos") {
      d <- d %>% filter(NOME_CURSO == input$curso)
    }
    
    # Intervalo de anos
    d <- d %>%
      filter(ANO_EVASAO >= input$ano_range[1],
             ANO_EVASAO <= input$ano_range[2])
    
    if (!is.null(input$cotas)) {
      d <- d %>% filter(COTISTA %in% input$cotas)
    }
    
    # Sexo
    if (!is.null(input$sexo)) {
      d <- d %>% filter(SEXO %in% input$sexo)
    }
    
    # Etnias
    if (!is.null(input$etnias)) {
      d <- d %>% filter(Etnia %in% input$etnias)
    }
    
    d
  })
  
  

  output$card_total <- renderText({
    d <- dados_filtrados()
    format(sum(d$Qtd_Evadidos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  
  output$card_cotistas <- renderText({
    d <- dados_filtrados()
    total <- sum(d$Qtd_Evadidos, na.rm = TRUE)
    if (total == 0) return("0%")
    cot <- sum(d$Qtd_Evadidos[d$COTISTA == "S"], na.rm = TRUE)
    paste0(round(100 * cot / total, 1), "%")
  })
  
  output$card_mulheres <- renderText({
    d <- dados_filtrados()
    total <- sum(d$Qtd_Evadidos, na.rm = TRUE)
    if (total == 0) return("0%")
    fem <- sum(d$Qtd_Evadidos[d$SEXO == "F"], na.rm = TRUE)
    paste0(round(100 * fem / total, 1), "%")
  })
  
  output$forma_pico <- renderText({
    df <- dados_filtrados() %>%
      group_by(FORMA_EVASAO) %>%
      summarise(Total = sum(Qtd_Evadidos)) %>%
      arrange(desc(Total))
    
    # Retorna o nome da primeira linha
    df$FORMA_EVASAO[1]
  })
  
  
  # ---- Gráficos ----
  output$plot_evasao_ano <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(ANO_EVASAO) %>%
      summarise(total = sum(Qtd_Evadidos)) %>%
      ggplot(aes(x = ANO_EVASAO, y = total)) +
      geom_line(linewidth = 1.2, color = "#2E86AB") +
      geom_point(size = 2, color = "#117A65") +
      labs(title = "Evasão ao longo dos anos", x = "Ano", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_forma_evasao <- renderPlot({
    ggplot(data = dados_filtrados, )
  })
  
  output$plot_forma_evasao <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(FORMA_EVASAO) %>%
      summarise(total = sum(Qtd_Evadidos)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(FORMA_EVASAO, total), y = total, fill = FORMA_EVASAO)) +
      geom_col(show.legend = FALSE, ) +
      coord_flip() +
      labs(title = "Formas de evasão", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_evasao_curso <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(NOME_CURSO) %>%
      summarise(total = sum(Qtd_Evadidos)) %>%
      arrange(desc(total)) %>%
      ggplot(aes(x = reorder(NOME_CURSO, total), y = total, fill = NOME_CURSO)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_sexo <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(SEXO) %>%
      summarise(total = sum(Qtd_Evadidos)) %>%
      ggplot(aes(x = SEXO, y = total, fill = SEXO)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Distribuição por sexo", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_cotista <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(COTISTA) %>%
      summarise(total = sum(Qtd_Evadidos)) %>%
      ggplot(aes(x = COTISTA, y = total, fill = COTISTA)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Cotistas vs Não Cotistas", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_etnia <- renderPlotly({
    etnias <- dados_filtrados() %>%
      select(starts_with("ET_"), Qtd_Evadidos) %>%
      summarise(across(starts_with("ET_"), sum)) %>%
      pivot_longer(cols = everything(), names_to = "Etnia", values_to = "Total")
    
    p <- etnias %>%
      ggplot(aes(x = reorder(Etnia, Total), y = Total, fill = Etnia)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Distribuição por grupo étnico", x = "", y = "Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$tabela <- renderDT({
    datatable(dados_filtrados(),
              extensions = "Buttons",
              options = list(
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel", "pdf", "print"),
                pageLength = 10
              ))
  })
}

# =========================================================
# 4. Executar App
# =========================================================
shinyApp(ui, server)
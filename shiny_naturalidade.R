# =========================================================
# DASHBOARD DE EVASÃO ACADÊMICA — UFES CCE (2015–2025)
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
library(purrr)

# =========================================================
# 1. Carregar e unir os dados (2015–2025)
# =========================================================

dados <- read.csv("base_evasao_final.csv")

# =========================================================
# 2. Função auxiliar para gerar abas de cursos
# =========================================================
aba_curso <- function(nome_curso) {
  nav_panel(
    title = nome_curso,
    layout_column_wrap(
      width = "250px",
      fill = FALSE,
      value_box(
        title = h5("Total de Evadidos"),
        value = h3(textOutput(paste0("card_total_", nome_curso))),
        showcase = bsicons::bs_icon("people-fill")
      ),
      # REMOVIDO: Value box de Formas de Evasão
      value_box(
        title = h5("Naturalidades Distintas"),
        value = h3(textOutput(paste0("card_nat_", nome_curso))),
        showcase = bsicons::bs_icon("geo-alt")
      )
    ),
    layout_column_wrap(
      width = 1/2,
      plotlyOutput(paste0("plot_ano_", nome_curso)),
      plotlyOutput(paste0("plot_forma_", nome_curso))  # MANTIDO: Gráfico de formas de evasão
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Top 5 Naturalidades por Forma de Evasão"),
        checkboxGroupInput(
          inputId = paste0("forma_evasao_", nome_curso),
          label = "Selecione as formas de evasão:",
          choices = unique(dados$FORMA_EVASAO),
          selected = unique(dados$FORMA_EVASAO)[1:min(3, length(unique(dados$FORMA_EVASAO)))],
          inline = TRUE  # JÁ ESTÁ DEFINIDO - deve aparecer na horizontal
        ),
        plotlyOutput(paste0("plot_nat_forma_", nome_curso), height = "500px")
      )
    )
  )
}

# =========================================================
# 3. Interface
# =========================================================
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "lux",
    base_font = font_google("Poppins"),
    heading_font = font_google("Montserrat")
  ),
  
  titlePanel("Dashboard de Evasão Acadêmica — UFES (2015–2025)"),
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      sliderInput("ano_range", "Período de Evasão (anos):",
                  min = min(dados$ANO_EVASAO, na.rm = TRUE),
                  max = max(dados$ANO_EVASAO, na.rm = TRUE),
                  value = c(min(dados$ANO_EVASAO, na.rm = TRUE),
                            max(dados$ANO_EVASAO, na.rm = TRUE)),
                  sep = ""),
      hr(),
      helpText("Use o filtro de anos para ajustar os gráficos.")
    ),
    
    page_navbar(
      nav_panel(title = "Geral",
                layout_column_wrap(
                  width = "250px",
                  fill = FALSE,
                  value_box(
                    title = h5("Total de Evadidos"),
                    value = h3(textOutput("card_total_geral")),
                    showcase = bsicons::bs_icon("people-fill")
                  ),
                  value_box(
                    title = h5("Cursos Incluídos"),
                    value = h3(textOutput("card_cursos_geral")),
                    showcase = bsicons::bs_icon("journal-bookmark")
                  ),
                  value_box(
                    title = h5("Formas de Evasão"),
                    value = h3(textOutput("card_formas_geral")),
                    showcase = bsicons::bs_icon("box-arrow-right")
                  )
                ),
                layout_column_wrap(
                  width = 1/2,
                  plotlyOutput("plot_evasao_ano_geral"),
                  plotlyOutput("plot_forma_evasao_geral")
                ),
                layout_column_wrap(
                  width = 1,
                  card(
                    card_header("Top 5 Naturalidades por Forma de Evasão (Geral)"),
                    checkboxGroupInput(
                      inputId = "forma_evasao_geral",
                      label = "Selecione as formas de evasão:",
                      choices = unique(dados$FORMA_EVASAO),
                      selected = unique(dados$FORMA_EVASAO)[1:min(3, length(unique(dados$FORMA_EVASAO)))],
                      inline = TRUE
                    ),
                    plotlyOutput("plot_nat_forma_geral", height = "500px")
                  )
                )
      ),
      
      # Abas dos cursos
      aba_curso("Estatística"),
      aba_curso("Física"),
      aba_curso("Química"),
      aba_curso("Matemática"),
      
      # Aba tabela detalhada
      nav_panel(title = "Tabela Detalhada", DTOutput("tabela")),
      
      # Aba dicionário
      nav_panel(title = "Dicionário",
                h4("Dicionário de Variáveis"),
                tags$table(
                  class = "table table-striped table-bordered",
                  tags$tr(tags$th("Variável"), tags$th("Descrição")),
                  tags$tr(tags$td("COD_CURSO"), tags$td("Código do curso conforme registro institucional")),
                  tags$tr(tags$td("NOME_CURSO"), tags$td("Nome do curso de graduação")),
                  tags$tr(tags$td("ANO_EVASAO"), tags$td("Ano de ocorrência da evasão")),
                  tags$tr(tags$td("PERIODO_EVASAO"), tags$td("Semestre ou período em que a evasão ocorreu")),
                  tags$tr(tags$td("FORMA_EVASAO"), tags$td("Motivo ou tipo de evasão do aluno")),
                  tags$tr(tags$td("NATURALIDADE"), tags$td("Cidade ou estado de origem do aluno")),
                  tags$tr(tags$td("QTDE"), tags$td("Quantidade de alunos evadidos com essa característica"))
                )
      )
    )
  )
)

# =========================================================
# 4. Servidor
# =========================================================
server <- function(input, output, session) {
  
  # Paleta de cores padronizada
  cores_padrao <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  
  # Filtragem base - excluindo NA de naturalidade
  dados_filtrados <- reactive({
    dados %>% 
      filter(ANO_EVASAO >= input$ano_range[1],
             ANO_EVASAO <= input$ano_range[2],
             !is.na(NATURALIDADE),
             NATURALIDADE != "NA",
             NATURALIDADE != "")
  })
  
  # ---- ABA GERAL ----
  output$card_total_geral <- renderText({
    format(sum(dados_filtrados()$QTDE, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  output$card_cursos_geral <- renderText({
    length(unique(dados_filtrados()$NOME_CURSO))
  })
  output$card_formas_geral <- renderText({
    length(unique(dados_filtrados()$FORMA_EVASAO))
  })
  
  output$plot_evasao_ano_geral <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(ANO_EVASAO) %>%
      summarise(total = sum(QTDE)) %>%
      ggplot(aes(x = ANO_EVASAO, y = total)) +
      geom_line(linewidth = 1.2, color = "#2E86AB") +
      geom_point(size = 2, color = "#117A65") +
      labs(title = "Evasão total ao longo dos anos", x = "Ano", y = "Total Evadidos") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_forma_evasao_geral <- renderPlotly({
    p <- dados_filtrados() %>%
      group_by(FORMA_EVASAO) %>%
      summarise(total = sum(QTDE)) %>%
      ggplot(aes(x = reorder(FORMA_EVASAO, total), y = total, fill = FORMA_EVASAO)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Distribuição das Formas de Evasão (Geral)", x = "", y = "Total Evadidos") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = cores_padrao)
    ggplotly(p)
  })
  
  # GRÁFICO ATUALIZADO: Top 5 Naturalidades com formas de evasão agrupadas
  output$plot_nat_forma_geral <- renderPlotly({
    req(input$forma_evasao_geral)
    
    dados_filt <- dados_filtrados() %>%
      filter(FORMA_EVASAO %in% input$forma_evasao_geral)
    
    if(nrow(dados_filt) == 0) {
      return(plotly_empty() %>% 
               layout(title = "Selecione pelo menos uma forma de evasão"))
    }
    
    # Encontrar as top 5 naturalidades totais (considerando TODOS os dados filtrados)
    top_naturalidades <- dados_filt %>%
      group_by(NATURALIDADE) %>%
      summarise(total_geral = sum(QTDE)) %>%
      arrange(desc(total_geral)) %>%
      slice_head(n = 5) %>%
      pull(NATURALIDADE)
    
    # Agregar dados apenas para as top 5 naturalidades
    dados_plot <- dados_filt %>%
      filter(NATURALIDADE %in% top_naturalidades) %>%
      group_by(NATURALIDADE, FORMA_EVASAO) %>%
      summarise(total = sum(QTDE), .groups = "drop") %>%
      mutate(NATURALIDADE = factor(NATURALIDADE, levels = rev(top_naturalidades)))
    
    # Criar paleta de cores para as formas de evasão selecionadas
    formas_selecionadas <- unique(dados_plot$FORMA_EVASAO)
    cores_formas <- cores_padrao[1:length(formas_selecionadas)]
    names(cores_formas) <- formas_selecionadas
    
    # Criar gráfico de barras agrupadas
    p <- ggplot(dados_plot, aes(x = NATURALIDADE, y = total, 
                                fill = FORMA_EVASAO,
                                text = paste("Naturalidade:", NATURALIDADE,
                                             "<br>Forma de Evasão:", FORMA_EVASAO,
                                             "<br>Total:", total,
                                             "<br>Percentual:", round(total/sum(dados_plot$total)*100, 1), "%"))) +
      geom_col(position = "stack", alpha = 0.9) +
      coord_flip() +
      labs(title = "Top 5 Naturalidades - Distribuição por Forma de Evasão", 
           x = "Naturalidade", 
           y = "Total de Evadidos",
           fill = "Forma de Evasão") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            axis.text.y = element_text(size = 12),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            legend.text = element_text(size = 10)) +
      scale_fill_manual(values = cores_formas)
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", 
                           x = 0.5, 
                           xanchor = "center",
                           y = -0.15,
                           yanchor = "top"))
  })
  
  # ---- ABAS DOS CURSOS ----
  cursos <- c("Estatística", "Física", "Química", "Matemática")
  
  for (curso in cursos) {
    local({
      nome <- curso
      
      # Atualizar choices do checkboxGroupInput dinamicamente
      observe({
        formas_curso <- dados_filtrados() %>% 
          filter(grepl(nome, NOME_CURSO, ignore.case = TRUE)) %>%
          pull(FORMA_EVASAO) %>%
          unique()
        
        updateCheckboxGroupInput(
          session = session,
          inputId = paste0("forma_evasao_", nome),
          choices = formas_curso,
          selected = formas_curso[1:min(3, length(formas_curso))],
          inline = TRUE  # GARANTINDO que fique na horizontal
        )
      })
      
      output[[paste0("card_total_", nome)]] <- renderText({
        d <- dados_filtrados() %>% filter(grepl(nome, NOME_CURSO, ignore.case = TRUE))
        format(sum(d$QTDE, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
      })
      
      # REMOVIDO: Card de formas de evasão dos cursos
      # output[[paste0("card_formas_", nome)]] <- renderText({
      #   d <- dados_filtrados() %>% filter(grepl(nome, NOME_CURSO, ignore.case = TRUE))
      #   length(unique(d$FORMA_EVASAO))
      # })
      
      output[[paste0("card_nat_", nome)]] <- renderText({
        d <- dados_filtrados() %>% filter(grepl(nome, NOME_CURSO, ignore.case = TRUE))
        length(unique(d$NATURALIDADE))
      })
      
      output[[paste0("plot_ano_", nome)]] <- renderPlotly({
        d <- dados_filtrados() %>% filter(grepl(nome, NOME_CURSO, ignore.case = TRUE))
        p <- d %>%
          group_by(ANO_EVASAO) %>%
          summarise(total = sum(QTDE)) %>%
          ggplot(aes(x = ANO_EVASAO, y = total)) +
          geom_line(linewidth = 1.2, color = "#2E86AB") +
          geom_point(size = 2, color = "#117A65") +
          labs(title = paste("Evasão em", nome, "ao longo dos anos"), x = "Ano", y = "Total Evadidos") +
          theme_minimal(base_size = 14)
        ggplotly(p)
      })
      
      # MANTIDO: Gráfico de formas de evasão por curso
      output[[paste0("plot_forma_", nome)]] <- renderPlotly({
        d <- dados_filtrados() %>% filter(grepl(nome, NOME_CURSO, ignore.case = TRUE))
        p <- d %>%
          group_by(FORMA_EVASAO) %>%
          summarise(total = sum(QTDE)) %>%
          ggplot(aes(x = reorder(FORMA_EVASAO, total), y = total, fill = FORMA_EVASAO)) +
          geom_col(show.legend = FALSE) +
          coord_flip() +
          labs(title = paste("Formas de Evasão —", nome), x = "", y = "Total") +
          theme_minimal(base_size = 14) +
          scale_fill_manual(values = cores_padrao)
        ggplotly(p)
      })
      
      # GRÁFICO ATUALIZADO: Top 5 Naturalidades com formas de evasão agrupadas (por curso)
      output[[paste0("plot_nat_forma_", nome)]] <- renderPlotly({
        req(input[[paste0("forma_evasao_", nome)]])
        
        d <- dados_filtrados() %>% 
          filter(grepl(nome, NOME_CURSO, ignore.case = TRUE),
                 FORMA_EVASAO %in% input[[paste0("forma_evasao_", nome)]])
        
        if(nrow(d) == 0) {
          return(plotly_empty() %>% 
                   layout(title = "Selecione pelo menos uma forma de evasão"))
        }
        
        # Encontrar as top 5 naturalidades totais (considerando TODOS os dados filtrados do curso)
        top_naturalidades <- d %>%
          group_by(NATURALIDADE) %>%
          summarise(total_geral = sum(QTDE)) %>%
          arrange(desc(total_geral)) %>%
          slice_head(n = 5) %>%
          pull(NATURALIDADE)
        
        # Agregar dados apenas para as top 5 naturalidades
        dados_plot <- d %>%
          filter(NATURALIDADE %in% top_naturalidades) %>%
          group_by(NATURALIDADE, FORMA_EVASAO) %>%
          summarise(total = sum(QTDE), .groups = "drop") %>%
          mutate(NATURALIDADE = factor(NATURALIDADE, levels = rev(top_naturalidades)))
        
        # Criar paleta de cores para as formas de evasão selecionadas
        formas_selecionadas <- unique(dados_plot$FORMA_EVASAO)
        cores_formas <- cores_padrao[1:length(formas_selecionadas)]
        names(cores_formas) <- formas_selecionadas
        
        # Criar gráfico de barras agrupadas
        p <- ggplot(dados_plot, aes(x = NATURALIDADE, y = total, 
                                    fill = FORMA_EVASAO,
                                    text = paste("Naturalidade:", NATURALIDADE,
                                                 "<br>Forma de Evasão:", FORMA_EVASAO,
                                                 "<br>Total:", total,
                                                 "<br>Percentual:", round(total/sum(dados_plot$total)*100, 1), "%"))) +
          geom_col(position = "stack", alpha = 0.9) +
          coord_flip() +
          labs(title = paste("Top 5 Naturalidades - Distribuição por Forma de Evasão —", nome), 
               x = "Naturalidade", 
               y = "Total de Evadidos",
               fill = "Forma de Evasão") +
          theme_minimal(base_size = 14) +
          theme(legend.position = "bottom",
                axis.text.y = element_text(size = 12),
                plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
                legend.text = element_text(size = 10)) +
          scale_fill_manual(values = cores_formas)
        
        ggplotly(p, tooltip = "text") %>% 
          layout(legend = list(orientation = "h", 
                               x = 0.5, 
                               xanchor = "center",
                               y = -0.15,
                               yanchor = "top"))
      })
    })
  }
  
  # ---- TABELA ----
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
# 5. Executar App
# =========================================================
shinyApp(ui, server)
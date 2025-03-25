
wq_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "WQ",
    tags$style(HTML("#Performance * { font-size: 18px; } #validation_message { color: red; font-size: 25px; }")),
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        h4("Instructions for Use:"),
        # instructions list omitted for brevity
        downloadButton(ns("downloadData"), "Download CSV Template"),
        actionButton(ns("emc_link"), "Get help with EMCs"),
        fileInput(ns("wqfile"), "Upload CSV File", accept = c(".csv")),
        textInput(ns("pollutant_name"), "Pollutant Name/Unit (optional, e.g. Copper, µg/L)", value = ""),
        numericInput(ns("threshold"), "Threshold (must be same unit as EMC data)", value = 1, min = 0, step = 0.1),
        verbatimTextOutput(ns("validation_message")),
        tags$img(src = "intepretation-slide.png", height = "100%", width = "100%"),
        width = 6
      ),
      mainPanel(
        uiOutput(ns("wq_ui_blocks")),
        width = 6
      )
    )
  )
}

wq_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$emc_link, {
      shinyjs::runjs('window.open("https://sccwrp.shinyapps.io/FWC_EMC_Calculator/", "_blank");')
    })
    
    valid_input <- reactiveVal(TRUE)
    
    observe({
      messages <- c()
      
      if (nchar(input$pollutant_name) > 30) {
        messages <- c(messages, "Pollutant Name exceeds the 30-character limit.")
      } else if (!grepl("^[A-Za-z0-9, µg/L/ ]*$", input$pollutant_name)) {
        messages <- c(messages, "Pollutant Name contains invalid characters.")
      }
      
      if (!is.null(input$threshold) && grepl("\\.\\d{2,}", as.character(input$threshold))) {
        messages <- c(messages, "Threshold must have at most one decimal place.")
      }
      
      if (length(messages) > 0) {
        valid_input(FALSE)
        output$validation_message <- renderText(paste(messages, collapse = "\n"))
      } else {
        valid_input(TRUE)
        output$validation_message <- renderText("")
      }
    })
    
    output$wqDataUploaded <- reactive({ !is.null(input$wqfile$datapath) })
    outputOptions(output, "wqDataUploaded", suspendWhenHidden = FALSE)
    
    processed_wqdata <- reactive({
      req(input$wqfile)
      req(input$threshold > 0)
      
      df <- read.csv(input$wqfile$datapath)
      
      df <- df %>%
        mutate(`inf/thresh` = influent / input$threshold,
               `eff/thresh` = effluent / input$threshold) %>%
        mutate(quadrant2 = case_when(
          (`eff/thresh` < 1 & `inf/thresh` > 1) ~ "Success",
          (`eff/thresh` < `inf/thresh` & `inf/thresh` <= 1) ~ "Excess",
          (`eff/thresh` >= `inf/thresh` & `eff/thresh` < 1) ~ "Marginal",
          (`eff/thresh` >= 1 & `eff/thresh` >= `inf/thresh`) ~ "Failure",
          (`eff/thresh` >= 1 & `eff/thresh` < `inf/thresh`) ~ "Insufficient"
        )) %>%
        mutate(quadrant2 = factor(quadrant2, levels = c("Success", "Excess", "Marginal", "Insufficient", "Failure")))
      
      df
    })
    
    effinf_plot <- reactive({
      df <- processed_wqdata()
      df <- df %>% mutate(`inf/thresh` = round(`inf/thresh`, 1),
                          `eff/thresh` = round(`eff/thresh`, 1))
      
      ggplot(df, aes(x = `inf/thresh`, y = `eff/thresh`, color = quadrant2, shape = quadrant2)) +
        geom_point(size = 3) +
        labs(
          x = "Influent / Threshold",
          y = "Effluent / Threshold",
          colour = "Performance",
          shape = "Performance",
          title = ifelse(input$pollutant_name == "",
                         paste("Threshold:", input$threshold),
                         paste(input$pollutant_name, "- Threshold:", input$threshold))
        ) +
        theme_minimal(base_size = 16) +
        scale_color_manual(values = perf_colors) +
        scale_shape_manual(values = perf_shapes) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        coord_fixed() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = "black")
        )
    })
    
    
    output$effinf_plot <- plotly::renderPlotly({
      ggplotly(effinf_plot())
    })
    
    gauge_plot <- reactive({
      req(processed_wqdata())
      df <- processed_wqdata()
      scr <- get.composite.score(df, performance_col = 'quadrant2')
      get.composite.gauge(scr)
    })
    
    output$score.gauge <- plotly::renderPlotly({
      gauge_plot()
    })
    
    summary_dat <- reactive({
      summary.table(processed_wqdata(), threshold = input$threshold, performance_col = 'quadrant2')
    })
    
    output$gauge.table <- DT::renderDataTable({
      dat <- summary_dat() %>% 
        rename(
          Success = num_success,
          Excess = num_excess,
          Marginal = num_marginal,
          Insufficient = num_insufficient,
          Failure = num_failure
        )
      datatable(dat, rownames = FALSE, options = list(dom = 'ft'))
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { "sample_influent_effluent.csv" },
      content = function(file) {
        file.copy("demo/sample_influent_effluent.csv", file)
      }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function() { "Performance_Index_Plot.png" },
      content = function(file) {
        ggsave(file, plot = effinf_plot(), device = "png", width = 12, height = 8, dpi = 300)
      }
    )
    
    output$downloadTable <- downloadHandler(
      filename = function() { "Performance_Index_Summary.csv" },
      content = function(file) {
        write.csv(summary_dat(), file, row.names = FALSE)
      }
    )
    
    
    output$wq_ui_blocks <- renderUI({
      if (is.null(input$wqfile)) {
        tags$img(src = "placeholder-eff-plot.png", height = "95%", width = "95%")
      } else {
      
        tagList(
          fluidRow(
            column(12,
                   h4("Performance Index Plot"),
                   downloadButton(ns("downloadPlot"), "Download Plot"),
                   actionButton(ns("read_me"), "Read Me", class = "btn-info"),
                   shinycssloaders::withSpinner(plotly::plotlyOutput(ns("effinf_plot")))
            )
          ),
          fluidRow(
            column(12,
                   h4("Performance Index Score"),
                   h5("The graphic is not available for download"),
                   div(style = "display: flex; justify-content: center; padding-top: 10px; padding-bottom: 10px;",
                       plotly::plotlyOutput(ns("score.gauge"), height = "320px"))
            )
          ),
          fluidRow(
            column(12,
                   h4("Performance Index Summary Table"),
                   downloadButton(ns("downloadTable"), "Download Summary Table"),
                   DT::dataTableOutput(ns("gauge.table"))
            )
          )
        )
      }
    })
    
    
    
    
  })
}
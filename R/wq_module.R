# R/wq_module.R

# WQ UI (module ---------------------------------------------------------------------------------------------------------
wq_ui <- function(id) {
  ns <- NS(id)
  
  ## The WQ UI is an individual tab item and is rendered in app.R
  tabItem(
    tabName = "WQ",
    tags$head(
      includeCSS("www/css/plottabs.css")
    ),
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        h4("Instructions for Use:"),
        wq_instructions, # in global.R
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


# WQ Server (module ---------------------------------------------------------------------------------------------------------
wq_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Link to the FWC EMC Calculator app so they can calculate EMC's ----
    observeEvent(input$emc_link, {
      shinyjs::runjs('window.open("https://sccwrp.shinyapps.io/FWC_EMC_Calculator/", "_blank");')
    })
    
    
    ## Data checks ------------------------------------------------------------------------------------------------------------------------
    ### Boolean to check whether the data they uploaded is valid or not ----
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
    
    ## Boolean to check whether or not they uploaded any data  ---------------------------------------------------------------------------------------------------------
    output$wqDataUploaded <- reactive({ !is.null(input$wqfile$datapath) })
    outputOptions(output, "wqDataUploaded", suspendWhenHidden = FALSE)
    
    
    ## Function that returns the main water quality dataframe that is used for analysis  ---------------------------------------------------------------------------------------------------------
    processed_wqdata <- reactive({
      req(input$wqfile)
      req(input$threshold > 0)
      
      df <- read.csv(input$wqfile$datapath)
      
      
      # This is code that has been adapted/copy/pasted over the course of 5-6 years now (its 2025 at the time of this comment)
      # There are certain reasons why the different iterations of authoring this code resulted in "quadrant2" being the category name
      # If I change it to something else here, it may break the code in many other places, so I will be leaving it as "quadrant2"
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
      
      # Return  
      df
      
    })
    
    
    ## Function that makes the main WQ Plot  ---------------------------------------------------------------------------------------------------------
    # *Variables here that are difficult to tell where they are defined are most likely found in global.R 
    # (For example, the shapes and colors vectors)
    effinf_plot <- reactive({
      
      req(getAxisLimit())
      
      plot_limit <- getAxisLimit()
      plot_width <- ifelse(is.na(plot_limit), 1, plot_limit / 5)
      
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
        scale_x_continuous(
          limits = c(0, plot_limit ),
          labels = format_axes(1),
          breaks = scales::breaks_width(plot_width)
        ) +
        scale_y_continuous(
          limits = c(0, plot_limit ),
          labels = format_axes(1),
          breaks = scales::breaks_width(plot_width)
        ) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        geom_vline(xintercept = 1, linetype = "dashed") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        # coord_fixed() +
        theme(
          text = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          legend.position = 'bottom',
          legend.margin = margin(-5, 0, -5, 0),
          legend.box = 'horizontal',
          legend.box.just = 'top',
          panel.grid.minor = element_blank()
        )
      
    })
    
    ## Plotly-ify the above graph (I think mainly for the sake of having tooltips)
    ### NOTE it seems to mess up the dashed y = 1 and x = 1 lines that were originally intended to be on the plot
    output$effinf_plot <- plotly::renderPlotly({
      ggplotly(effinf_plot()) 
    })
    
    # Commented out for now since I'm trying to have the user have the ability to change the plot scale
    ### Warning message if some data points are omitted from the plot ----
    output$wqPlotWarningMessage <- renderText({
      req(input$wqfile, input$axisLimit)
      
      df <- processed_wqdata()
      axis_limit <- input$axisLimit
      
      # Check if any data points exceed the selected axis range
      if (any(df$`inf/thresh` > axis_limit | df$`eff/thresh` > axis_limit)) {
        HTML("
      <p style='color: red;'>
        <strong>Note</strong>: Some data points are outside the current axis limit and are not shown in the plot.<br>
        Use the slider to adjust the axis range and reveal hidden points.
      </p>
    ")
      } else {
        NULL
      }
    })
    
    ### Reactive function to get the axis limit for the hydrology plot ----
    getAxisLimit <- reactive({
      if (is.null(input$axisLimit) || length(input$axisLimit) != 1 || is.na(input$axisLimit)) {
        return(3)  # fallback
      }
      input$axisLimit
    })
    
    ### Observe changes in the data to update the slider input ----
    observe({
      df <- processed_wqdata()
      max_val <- ceiling(max(c(df$`inf/thresh`, df$`eff/thresh`), na.rm = TRUE))
      max_slider_val <- max(max_val, 3)
      
      updateSliderInput(session, "axisLimit",
                        min = 1,
                        max = max_slider_val,
                        value = max_slider_val)
    })
    
    
    
    ## WQ Gauge  ---------------------------------------------------------------------------------------------------------
    gauge_plot <- reactive({
      req(processed_wqdata())
      df <- processed_wqdata()
      scr <- get.composite.score(df, performance_col = 'quadrant2')
      get.composite.gauge(scr)
    })
    
    # The gauge is a plotly object
    output$score.gauge <- plotly::renderPlotly({
      gauge_plot()
    })
    
    
    ## WQ Index Summary Table  ---------------------------------------------------------------------------------------------------------
    summary_dat <- reactive({
      summary.table(processed_wqdata(), threshold = input$threshold, performance_col = 'quadrant2')
    })
    
    # Render the summary table 
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
    
    
    ## Download Handlers ---------------------------------------------------------------------------------------------------------
    ### Download template/example data ----
    output$downloadData <- downloadHandler(
      filename = function() { "sample_influent_effluent.csv" },
      content = function(file) {
        file.copy("demo/sample_influent_effluent.csv", file)
      }
    )
    
    ### plot download button ----
    output$downloadPlot <- downloadHandler(
      filename = function() { "Performance_Index_Plot.png" },
      content = function(file) {
        ggsave(file, plot = effinf_plot(), device = "png", width = 12, height = 8, dpi = 300)
      }
    )
    
    ### Download the summary table ----
    output$downloadTable <- downloadHandler(
      filename = function() { "Performance_Index_Summary.csv" },
      content = function(file) {
        write.csv(summary_dat(), file, row.names = FALSE)
      }
    )
    
    ## Plot/graphs UI ----
    # This is the UI that gets generated when they upload their data
    # A scatter plot, gauge, and summary table display on the right side, along with respective download buttons (if applicable)
    # If no file is uploaded, it will display the example JPG that explains the plot and how to interpret it
    output$wq_ui_blocks <- renderUI({
      if (is.null(input$wqfile)) {
        tags$img(src = "WQIndexOverviewPlot.jpg", height = "95%", width = "95%")
      } else {
      
        tagList(
          fluidRow(
            column(12,
                   h4("Performance Index Plot"),
                   downloadButton(ns("downloadPlot"), "Download Plot"),
                   actionButton(ns("read_me"), "Read Me", class = "btn-info"),
                   #shinycssloaders::withSpinner(plotly::plotlyOutput(ns("effinf_plot")))
                   shinycssloaders::withSpinner(
                     plotly::plotlyOutput(ns("effinf_plot"), width = "100%")
                   ),
                   sliderInput(ns("axisLimit"), "Set axis limits:",
                               min = 0, max = 100, value = 10, step = 1),
                   htmlOutput(ns("wqPlotWarningMessage"))
                   
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
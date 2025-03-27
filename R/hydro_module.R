# R/hydro_module.R

# Hydro UI (module ---------------------------------------------------------------------------------------------------------
hydro_ui <- function(id) {
  ns <- NS(id)
  
  ## The Hydro UI is an individual tab item and is rendered in app.R
  tabItem(
    tabName = "Hydro",
    tags$style(HTML("#Performance * { font-size: 18px; } #validation_message { color: red; font-size: 25px; }")),
    
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        h4("Instructions for Use:"),
        hydro_instructions, # in global.R
        downloadButton(ns("downloadHydroData"), "Download CSV Template"),
        fileInput(ns("hydrofile"), "Upload CSV File", accept = c(".csv")),
        numericInput(ns("designstormdepth"), "Design Storm Depth", value = 0.7, min = 0, step = 0.1),
        numericInput(ns("designvolume"), "Design Volume", value = 26535, min = 0, step = 0.1),
        tags$img(src = "HydroIndexInterpretation.jpg", height = "100%", width = "100%"),
        
        width = 6
      ),
      
      mainPanel(
        uiOutput(ns("hydro_ui_blocks")),
        width = 6
      )
      
      
    )
    
  )
}

# Hydro Server (module ---------------------------------------------------------------------------------------------------------
hydro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Uncertainty Buffer "global" variable ---------------------------------------------------------------------------------------------------------
    # Uncertainty Buffer is 20% according to Elizabeth
    UNCERTAINTY_BUFFER <- 0.2
    
    ## HydroDataUploaded Boolean Variable ---------------------------------------------------------------------------------------------------------
    # Simple Boolean to tell us whether the data has been uploaded or not
    # Used to conditionally render the plots, etc.
    output$hydroDataUploaded <- reactive({ 
      !is.null(input$hydrofile$datapath)
    })
    outputOptions(output, "hydroDataUploaded", suspendWhenHidden = FALSE)
    
    
    ## Function that returns the main Hydrology data ---------------------------------------------------------------------------------------------------------
    processed_hydrodata <- reactive({
      req(input$hydrofile)
      
      df <- read.csv(input$hydrofile$datapath)
      
      df <- df %>%
        filter(!is.na(precipitationdepth), !is.na(inflow), !is.na(outflow)) %>%
        mutate(
          inflow = as.numeric(inflow),
          outflow = as.numeric(outflow),
          bypass = as.numeric(bypass),
          precipitationdepth = as.numeric(precipitationdepth),
          `Bypass occurred` = if_else(!(is.na(bypass) | bypass == 0), "Bypass", "No Bypass"),
          `precip/design` = round2(precipitationdepth / input$designstormdepth, 2),
          `volreduc/design` = round2((inflow - outflow) / input$designvolume, 2),
          quadrant = case_when(
            ((`volreduc/design` > (1 + UNCERTAINTY_BUFFER)) & (`precip/design` < 1)) | ((inflow - outflow) < 0) ~ "Check Data",
            (`volreduc/design` > (1 + UNCERTAINTY_BUFFER)) & (`precip/design` >= 1) ~ "Excess",
            (`volreduc/design` < (1 - UNCERTAINTY_BUFFER)) & (`precip/design` >= 1) ~ "Failure",
            ((`volreduc/design` < (1 - UNCERTAINTY_BUFFER)) & (`precip/design` < 1)) & `Bypass occurred` == 'Bypass' ~ "Small Storm With Bypass",
            TRUE ~ "Success"
          ),
          bypass_fraction = bypass / (bypass + inflow)
        ) %>%
        mutate(quadrant = factor(quadrant, levels = c("Success", "Excess", "Check Data", "Failure", "Small Storm With Bypass")))
      
      df
    })
    
    ## Function that generates the main Hydrology plot -----------------------------------------------------------------------------------------
    # *Variables here that are difficult to tell where they are defined are most likely found in global.R 
    # (For example, the shapes and colors vectors)
    hydroplot <- reactive({
      dat <- processed_hydrodata()
      max_plot_vals <- max(c(dat$`precip/design`, dat$`volreduc/design`), na.rm = TRUE)
      plot_width <- ceiling(max_plot_vals) / 5
      
      ggplot(dat, aes(`precip/design`, `volreduc/design`)) +
        geom_point(aes(colour = quadrant, shape = quadrant), size = 4) +
        geom_segment(x = 1, y = -Inf, xend = 1, yend = 0.8, linetype = "dashed", linewidth = 1) +
        geom_segment(x = 1, y = Inf, xend = 1, yend = 1.2, linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = 1 + UNCERTAINTY_BUFFER, linetype = 'dashed', linewidth = 1) +
        geom_hline(yintercept = 1 - UNCERTAINTY_BUFFER, linetype = 'dashed', linewidth = 1) +
        scale_shape_manual(values = hydro_shapes) +
        scale_colour_manual(values = designplot_colors) +
        scale_x_continuous(
          limits = c(0, max(c(max_plot_vals, 3))),
          labels = format_axes(1),
          breaks = scales::breaks_width(plot_width)
        ) +
        scale_y_continuous(
          limits = c(0, max(c(max_plot_vals, 3))),
          labels = format_axes(1),
          breaks = scales::breaks_width(plot_width)
        ) +
        labs(
          x = expression(paste(frac(`Precipitation Depth`, `Design Storm Depth`))), 
          y = expression(paste(frac(`Volume Retained`, `Design Volume`))), 
          colour = "Performance",
          shape = "Performance"
        ) +
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
        ) +
        guides(
          colour = guide_legend(override.aes = list(size = 5), nrow = 3, byrow = TRUE),
          shape = guide_legend(override.aes = list(size = 5), nrow = 3, byrow = TRUE)
        )
    })
    
    # Simply call that above reactive and render the plot
    output$hydroplot <- renderPlot({
      hydroplot()
    })
    
    
    
    
    ## Generate the hydrology data summary table  ---------------------------------------------------------------------------------------------------------
    # This one is also used for the gauge
    hydrotable <- reactive({
      dat <- processed_hydrodata()
      dat %>%
        group_by(quadrant) %>%
        summarise(
          Count = n(),
          `%` = round2(100 * Count / nrow(dat), 2),
          bypass_fraction_average = round2(mean(bypass_fraction, na.rm = TRUE), 2)
        ) %>%
        rename(Performance = quadrant)
    })
    
    # Simply call that above function and render it
    output$hydro.gauge.table <- DT::renderDataTable({
      hydrotable()
    })
    
    
    ## Hydrology Gauge  ---------------------------------------------------------------------------------------------------------
    # get.composite.hydro.gauge function definition found in gauge.R
    output$hydro.score.gauge <- renderPlotly({
      get.composite.hydro.gauge(hydrotable())
    })
    
    
    
    
    ## Download Handlers   ---------------------------------------------------------------------------------------------------------
    ### Hydrology data download (example/template) ----
    output$downloadHydroData <- downloadHandler(
      filename = function() { "sample_hydrology_data.csv" },
      content = function(file) {
        file.copy("demo/sample_hydrology_data.csv", file)
      }
    )
    
    ### Hydrology plot download ----
    output$downloadHydroPlot <- downloadHandler(
      filename = function() { "Hydrology_Performance_Plot.png" },
      content = function(file) {
        ggsave(file, plot = hydroplot(), device = "png", width = 13.33, height = 7.33, dpi = 300)
      }
    )
    
    ### Hydrology summary table download ----
    output$downloadHydroTable <- downloadHandler(
      filename = function() { "Performance_Index_Summary.csv" },
      content = function(file) {
        write.csv(hydrotable(), file, row.names = FALSE)
      }
    )
    
    ## README Action button ----
    # Simple instructions explaining the plot downloads
    observeEvent(input$read_me, {
      showModal(modalDialog(
        title = "Instructions",
        tags$div(style = "font-size: 14px;",
                 "When you click the Download Plot button, a .png file formatted for reports or other use cases will be downloaded.",
                 tags$br(), tags$br(),
                 "To download the interactive plot as it appears on screen, use the camera icon in the plot toolbar."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    
    ## Plot/graphs UI ----
    # This is the UI that gets generated when they upload their data
    # A scatter plot, gauge, and summary table display on the right side, along with respective download buttons (if applicable)
    # If no file is uploaded, it will display the example JPG that explains the plot and how to interpret it
    output$hydro_ui_blocks <- renderUI({
      if (is.null(input$hydrofile)) {
        tags$img(src = "HydroIndexPlot.jpg", height = "95%", width = "95%")
      } else {
        tagList(
          fluidRow(
            conditionalPanel(
              condition = paste0("input.", ns("hydrofile"), " !== null"),
              column(12,
                     h4("Performance Index Plot"),
                     downloadButton(ns("downloadHydroPlot"), "Download Plot"),
                     actionButton(ns("read_me"), "Read Me", class = "btn-info"),
                     #shinycssloaders::withSpinner(plotOutput(ns("hydroplot")))
                     shinycssloaders::withSpinner(
                       div(style = "position: relative; width: 100%; padding-bottom: 75%;",
                           div(style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                               plotOutput(ns("hydroplot"), width = "100%", height = "100%")
                           )
                       )
                     )
                     
                     
                     
              )
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = paste0("input.", ns("hydrofile"), " !== null"),
              column(12,
                     h4("Performance Index Score"),
                     h5("The graphic is not available for download"),
                     div(style = "display: flex; justify-content: center; padding-top: 10px; padding-bottom: 20px;",
                         plotly::plotlyOutput(ns("hydro.score.gauge"), height = "320px"))
              )
            )
          ),
          fluidRow(
            conditionalPanel(
              condition = paste0("input.", ns("hydrofile"), " !== null"),
              column(12,
                     h4("Performance Index Summary Table"),
                     downloadButton(ns("downloadHydroTable"), "Download Summary Table"),
                     DT::dataTableOutput(ns("hydro.gauge.table"))
              )
            )
          )
        )
      }
    })
    
  })
}
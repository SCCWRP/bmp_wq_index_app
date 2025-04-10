# R/hydro_module.R

# Hydro UI (module ---------------------------------------------------------------------------------------------------------
hydro_ui <- function(id) {
  ns <- NS(id)
  
  ## The Hydro UI is an individual tab item and is rendered in app.R
  tabItem(
    tabName = "Hydro",
    tags$head(
      includeCSS("www/css/plottabs.css")
    ),
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
      
      # if the uploaded file is a csv, then input$hydrofile$type will be "text/csv"
      # if it is an xlsx file, then input$hydrofile$type will be "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      # if it is an xls, then input$hydrofile$type will be "application/vnd.ms-excel"
      req(input$hydrofile)
      print("input$hydrofile")
      print(input$hydrofile$type)
      
      ### File type check ----
      # ACCEPTABLE_FILETYPES is defined in global.R
      acceptable.file <- input$hydrofile$type %in% ACCEPTABLE_FILETYPES
      
      #### to show the modal
      if (!acceptable.file) showModal(modalDialog(
          title = "Unrecognized file type",
          paste0(
            "Unrecognized filetype: ", input$hydrofile$type,
            ". Please upload a file of one of the following types: ",
            paste(ACCEPTABLE_FILETYPE_COMMON_NAMES, collapse = ", ")
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      
      #### prevent remaining code in function from executing
      validate(need(acceptable.file,paste0(
            "Unrecognized filetype: ", input$hydrofile$type,
            ". Please upload a file of type: ",
            paste(ACCEPTABLE_FILETYPE_COMMON_NAMES, collapse = ", ")
          )
        )
      )
      
      
      ### Ingest hydrology data ----
      df <- tryCatch({
        suppressWarnings(read.csv(input$hydrofile$datapath))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error Reading File",
          paste("There was a problem reading your file:", e$message),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      })
      validate(need(!is.null(df), "File could not be read"))
      
      ### Check for empty dataframe
      empty.df <- nrow(df) == 0
      if (empty.df) {
        showModal(modalDialog(
          title = "Error Processing File",
          "No data found in this file",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
      validate(need(!empty.df, "No data found"))
      
      
      ### Check column headers ----
      required_cols <- c("precipitationdepth", "inflow", "outflow", "bypass")
      missing_cols <- setdiff(required_cols, names(df))
      has.all.columns <- length(missing_cols) == 0
      if (!has.all.columns) {
        showModal(modalDialog(
          title = "Error Processing File",
          paste("Data is missing required columns: ", paste(missing_cols, collapse = ', ')),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
      # Prevent further execution of the code
      validate( need(has.all.columns, paste("Missing columns:", paste(missing_cols, collapse = ", ")) ) )
      
      
      ### Check column data types ----
      non_numeric_cols <- required_cols[!sapply(df[required_cols], function(col) {
        # Remove NA values and check if all remaining can be coerced to numeric
        col_no_na <- col[!is.na(col)]
        all(!is.na(suppressWarnings(as.numeric(col_no_na))))
      })]
      
      is.all.numeric <- length(non_numeric_cols) == 0
      
      if (!is.all.numeric) {
        showModal(modalDialog(
          title = "Invalid Data Format",
          paste("The following columns contain non-numeric values:", paste(non_numeric_cols, collapse = ", ")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
      validate(need(is.all.numeric, paste("Non-numeric data in columns:", paste(non_numeric_cols, collapse = ", "))))
      
      
      
      
      ### Process hydrology data ----
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
    
    
    # Commented out for now since I'm trying to have the user have the ability to change the plot scale
    ### Function that filters the main Hydrology data if the data will throw off the scaling of the graph ------------------------------------------------------------
    # filtered_hydrodata <- reactive({
    #   df <- processed_hydrodata()
    #   max_x <- df$`precip/design` %>% max()
    #   max_y <- df$`volreduc/design` %>% max()
    #   
    # })
    
    ## Function that generates the main Hydrology plot -----------------------------------------------------------------------------------------
    # *Variables here that are difficult to tell where they are defined are most likely found in global.R 
    # (For example, the shapes and colors vectors)
    hydroplot <- reactive({
      
      req(getAxisLimit())
      
      dat <- processed_hydrodata()
      
      plot_limit <- getAxisLimit() %>% as.numeric
      plot_width <- ifelse(is.na(plot_limit), 1, plot_limit / 5)
      
      message(paste("plot_limit =", plot_limit, "plot_width =", plot_width))
      
      
      ggplot(dat, aes(`precip/design`, `volreduc/design`)) +
        geom_point(aes(colour = quadrant, shape = quadrant), size = 5) +
        geom_segment(x = 1, y = -Inf, xend = 1, yend = 0.8, linetype = "dashed", linewidth = 1) +
        geom_segment(x = 1, y = Inf, xend = 1, yend = 1.2, linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = 1 + UNCERTAINTY_BUFFER, linetype = 'dashed', linewidth = 1) +
        geom_hline(yintercept = 1 - UNCERTAINTY_BUFFER, linetype = 'dashed', linewidth = 1) +
        scale_shape_manual(values = hydro_shapes) +
        scale_colour_manual(values = designplot_colors) +
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
    
    ### Simply call that above reactive and render the plot ----
    output$hydroplot <- renderPlot({
      req(input$hydrofile, input$axisLimit)  # Ensures file + slider exist
      hydroplot()
    })
    
    
    # Commented out for now since I'm trying to have the user have the ability to change the plot scale
    ### Warning message if some data points are omitted from the plot ----
    output$hydroPlotWarningMessage <- renderText({
      req(input$hydrofile, input$axisLimit)
      
      df <- processed_hydrodata()
      axis_limit <- input$axisLimit
      
      # Check if any data points exceed the selected axis range
      if (any(df$`precip/design` > axis_limit | df$`volreduc/design` > axis_limit)) {
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
      df <- processed_hydrodata()
      max_val <- ceiling(max(c(df$`precip/design`, df$`volreduc/design`), na.rm = TRUE))
      max_slider_val <- max(max_val, 3)
      
      shinyWidgets::updateSliderTextInput(
        session,
        "axisLimit",
        choices = as.character(1:max_slider_val),
        selected = as.character(max_slider_val)
      )
      
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
    
    ### Processed Hydrology data download (used for the graph) ----
    output$downloadProcessedHydroData <- downloadHandler(
      filename = function() { "hydrology_data_from_plot.csv" },
      content = function(file) {
        write.csv(processed_hydrodata(), file, row.names = FALSE)
      }
    )
    
    ### Hydrology plot download ----
    output$downloadHydroPlot <- downloadHandler(
      filename = function() { "Hydrology_Performance_Plot.png" },
      content = function(file) {
        ggsave(file, plot = hydroplot(), device = "png", width = 13.33, height = 7.33, dpi = 300, bg = "white")
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
                     downloadButton(ns("downloadProcessedHydroData"), "Download Data"),
                     actionButton(ns("read_me"), "Read Me", class = "btn-info"),
                     #shinycssloaders::withSpinner(plotOutput(ns("hydroplot")))
                     shinycssloaders::withSpinner(
                       div(style = "position: relative; width: 100%; padding-bottom: 75%;",
                           div(style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                               plotOutput(ns("hydroplot"), width = "100%", height = "100%")
                           )
                       )
                     ),
                     shinyWidgets::sliderTextInput(
                       ns("axisLimit"), "Set axis limits:",
                       choices = as.character(1:5),
                       selected = as.character(5),
                       grid = TRUE
                     ),
                     htmlOutput(ns("hydroPlotWarningMessage"))
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
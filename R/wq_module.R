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
    div(style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-left: 2px solid #007bff; border-radius: 4px;",
        p(
          style = "margin: 0; font-style: italic; color: #6c757d;",
          HTML('
            <p>A detailed description, including example applications is described in the open source publication:</p>
            <a href="https://doi.org/10.1016/j.watres.2025.123769" style = "text-decoration: none; display: block;" target="_blank">
                <p>
                    Fassman-Beck, E.A., Tiernan, E.D., Cheng, K.L., Schiff K.C. (2025)<br>
                    A data-driven index for evaluating BMP water quality performance<br>
                    <i>Water Research</i>, 282(2025) 123769<br>
                    https://doi.org/10.1016/j.watres.2025.123769
                </p>
            </a>
        '))
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
        fluidRow(
          column(6,
                 numericInput(ns("threshold"), "Threshold", value = 1, min = 0, step = 0.1)
          ),
          column(6,
                 selectInput(ns("threshold_units"), "Units", choices = c('mg/L','μg/L','cfu/100mL'))
          )
        ),
        verbatimTextOutput(ns("validation_message")),
        tags$img(src = "intepretation-slide.png", height = "100%", width = "100%"),
        width = 5
      ),
      mainPanel(
        uiOutput(ns("wq_ui_blocks")),
        width = 7
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
      
      ### File type check ----
      acceptable.file <- (input$wqfile$type %in% ACCEPTABLE_FILETYPES) ||
        tolower(tools::file_ext(input$wqfile$name)) == "csv" # The second part handles an edge case where the MIME type for a csv is incorrectly reported (Firefox)
      
      
      if (!acceptable.file) {
        showModal(modalDialog(
          title = "Unrecognized file type",
          paste0(
            "Unrecognized filetype: ", input$wqfile$type,
            ". Please upload a file of one of the following types: ",
            paste(ACCEPTABLE_FILETYPE_COMMON_NAMES, collapse = ", ")
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
      validate(need(acceptable.file, paste0(
        "Unrecognized filetype: ", input$wqfile$type,
        ". Please upload a file of type: ",
        paste(ACCEPTABLE_FILETYPE_COMMON_NAMES, collapse = ", ")
      )))
      
      ### Ingest data ----
      df <- tryCatch({
        suppressWarnings(read.csv(input$wqfile$datapath))
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
      
      ### Check for empty dataframe ----
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
      required_cols <- c("influent", "effluent")
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
      validate(need(has.all.columns, paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      
      ### Check column data types ----
      non_numeric_cols <- required_cols[!sapply(df[required_cols], function(col) {
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
      
      ### Process data ----
      df <- df %>%
        filter(!is.na(influent), !is.na(effluent)) %>% # labmp performance index filters the data this way as well
        mutate(
          
          # round half up is a function from the library called "janitor"
          # it is not from base R
          # it is used to circumvent R's non-traditional rounding method (IEEE 754) AKA banker's rounding
          # Round half up uses the traditional rounding method we learned in elementary school
          # So if the next decimal place is a 5, it will round it up rather than going to the nearest even number
          # Rounding to 2 decimal place convention here is following that of the performance index application that was given to LACPW
          # rounding is performed after taking the ratio of inflow and outflow to the user-defined threshold
          
          influent = as.numeric(influent) ,
          effluent = as.numeric(effluent) ,
          `inf/thresh` = ( influent / input$threshold ) %>% round_half_up(2),
          `eff/thresh` = ( effluent / input$threshold ) %>% round_half_up(2)
        ) %>%
        mutate(quadrant = case_when(
          (`eff/thresh` < 1 & `inf/thresh` > 1) ~ "Success",
          (`eff/thresh` < `inf/thresh` & `inf/thresh` <= 1) ~ "Excess",
          (`eff/thresh` >= `inf/thresh` & `eff/thresh` < 1) ~ "Marginal",
          (`eff/thresh` >= 1 & `eff/thresh` >= `inf/thresh`) ~ "Failure",
          (`eff/thresh` >= 1 & `eff/thresh` < `inf/thresh`) ~ "Insufficient"
        )) %>%
        mutate(quadrant = factor(quadrant, levels = c("Success", "Excess", "Marginal", "Insufficient", "Failure")))
      
      return(df)
    })
    
    
    
    ## Function that makes the main WQ Plot  ---------------------------------------------------------------------------------------------------------
    # *Variables here that are difficult to tell where they are defined are most likely found in global.R 
    # (For example, the shapes and colors vectors)
    effinf_plot <- reactive({
      
      req(getAxisLimit())
      
      plot_limit <- getAxisLimit() %>% as.numeric
      plot_width <- ifelse(is.na(plot_limit), 1, plot_limit / 5)
      
      df <- processed_wqdata()
      
      # paste defaults to having a single space as a separator
      thresh_string <- paste('Threshold:', input$threshold, input$threshold_units)
      
      ggplot(df, aes(x = `inf/thresh`, y = `eff/thresh`, color = quadrant, shape = quadrant)) +
        geom_point(size = 5) +
        geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) +
        geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
        scale_shape_manual(values = perf_shapes) +
        scale_color_manual(values = perf_colors) +
        scale_x_continuous(
          limits = c(0, plot_limit),
          labels = format_axes(1),
          breaks = scales::breaks_width(plot_width)
        ) +
        scale_y_continuous(
          limits = c(0, plot_limit),
          labels = format_axes(1),
          breaks = scales::breaks_width(plot_width)
        ) +
        labs(
          x = "Influent / Threshold",
          y = "Effluent / Threshold",
          colour = "Performance",
          shape = "Performance",
          title = ifelse(
            input$pollutant_name == "",
            thresh_string,
            paste(input$pollutant_name, thresh_string, sep = " - ")
          )
        ) +
        theme(
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
          panel.grid.major = element_line(color = "gray90", size = 0.5),
          panel.grid.minor = element_line(color = "gray95", size = 0.3),
          text = element_text(size = 18),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          legend.position = 'bottom',
          legend.margin = margin(-5, 0, -5, 0),
          legend.box = 'horizontal',
          legend.box.just = 'top'
        ) +
        guides(
          colour = guide_legend(override.aes = list(size = 5), nrow = 1, byrow = TRUE),
          shape = guide_legend(override.aes = list(size = 5), nrow = 1, byrow = TRUE)
        )
    })
    
    
    ## Plotly-ify the above graph (I think mainly for the sake of having tooltips)
    ### NOTE it seems to mess up the dashed y = 1 and x = 1 lines that were originally intended to be on the plot
    # output$effinf_plot <- plotly::renderPlotly({
    output$effinf_plot <- renderPlot({
      # ggplotly(effinf_plot())
      effinf_plot()
    })
    
    # Commented out for now since I'm trying to have the user have the ability to change the plot scale
    ### Warning message if some data points are omitted from the plot ----
    output$wqPlotWarningMessage <- renderText({
      req(input$wqfile, input$axisLimit)
      
      df <- processed_wqdata()
      axis_limit <- input$axisLimit
      
      # Check if any data points exceed the selected axis range
      if (any( na.omit(df$`inf/thresh` > axis_limit | df$`eff/thresh` > axis_limit) ) ) {
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
      
      shinyWidgets::updateSliderTextInput(
        session,
        "axisLimit",
        choices = as.character(1:max_slider_val),
        selected = as.character(max_slider_val)
      )
      
      
    })
    
    
    
    ## WQ Gauge  ---------------------------------------------------------------------------------------------------------
    gauge_plot <- reactive({
      req(processed_wqdata())
      df <- processed_wqdata()
      scr <- get.composite.score(df, performance_col = 'quadrant')
      get.composite.gauge(scr)
    })
    
    # The gauge is a plotly object
    output$score.gauge <- plotly::renderPlotly({
      gauge_plot()
    })
    
    
    ## WQ Index Summary Table  ---------------------------------------------------------------------------------------------------------
    summary_dat <- reactive({
      summary.table(processed_wqdata(), threshold = input$threshold, performance_col = 'quadrant')
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
    
    ### Processed WQ data download (used for the graph) ----
    output$downloadProcessedWQData <- downloadHandler(
      filename = function() { "wq_data_from_plot.csv" },
      content = function(file) {
        write.csv(processed_wqdata(), file, row.names = FALSE)
      }
    )
    
    ### plot download button ----
    output$downloadPlot <- downloadHandler(
      filename = function() { "Performance_Index_Plot.png" },
      content = function(file) {
        ggsave(file, plot = effinf_plot(), device = "png", width = 12, height = 8, dpi = 300, bg = "white")
      }
    )
    
    ### Download the summary table ----
    output$downloadTable <- downloadHandler(
      filename = function() { "Performance_Index_Summary.csv" },
      content = function(file) {
        write.csv(summary_dat(), file, row.names = FALSE)
      }
    )
    
    
    ## Download Info Action button ----
    # Simple instructions explaining the plot downloads
    observeEvent(input$download_info, {
      showModal(modalDialog(
        title = "ⓘ Data and Plot Download Information",
        tags$div(style = "font-size: 14px;",
                 "If you click 'Download Data' above the plot, your data will be downloaded with 3 additional columns:",
                 tags$li("inf/thresh (ratio of influent concentration to the user-defined threshold"),
                 tags$li("eff/thresh (ratio of effluent concentration to the user-defined threshold"),
                 tags$li("quadrant (the category of the BMP's performance at that event)"),
                 tags$br(), tags$br(),
                 "When you click the Download Plot button, a .png file formatted for reports or other use cases will be downloaded."
                 
                 # Old instructions from when a plotly object was rendered in the UI
                 # "To download the interactive plot as it appears on screen, use the camera icon in the plot toolbar."
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    
    ## Plot/graphs UI ----
    # This is the UI that gets generated when they upload their data
    # A scatter plot, gauge, and summary table display on the right side, along with respective download buttons (if applicable)
    # If no file is uploaded, it will display the example JPG that explains the plot and how to interpret it
    output$wq_ui_blocks <- renderUI({
      
      # This chunk needs to be executed before the "req" so that the placeholder image can display if no data are uploaded
      if (is.null(input$wqfile)) {
        # it is necessary to explicitly return, otherwise the code will continue executing
        return(tags$img(src = "WQIndexOverviewPlot.jpg", height = "95%", width = "95%"))
      }
      
      # If no valid data found, prevent rendering of this entire set of UI components.
      # It almost feels like we shouldn't need this line due to the above line, but it's better to be safe
      # I think there is a reason why this line was added, but I can't exactly remember why
      # If you are taking over this project, I recommend leaving this line below
      req(processed_wqdata())
      
      # The UI output with all their plots and data analysis
      tagList(
        fluidRow(
          column(12,
                 h4("Performance Index Plot"),
                 downloadButton(ns("downloadPlot"), "Download Plot"),
                 downloadButton(ns("downloadProcessedWQData"), "Download Data"),
                 actionButton(ns("download_info"), "ⓘ Info", class = "btn-info"),
                 br(), br(),
                 shinycssloaders::withSpinner(
                   div(style = "position: relative; width: 100%; padding-bottom: 75%;",
                       div(style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                           plotOutput(ns("effinf_plot"), width = "100%", height = "100%")
                       )
                   )
                 ),
                 br(),
                 shinyWidgets::sliderTextInput(
                   ns("axisLimit"), "Set axis limits:",
                   choices = as.character(1:5),
                   selected = as.character(5),
                   grid = TRUE
                 ),
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
    })
  })
}

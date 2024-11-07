library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(reticulate)
library(shinydashboard)

#py_install(c('plotly', 'kaleido'), pip = TRUE)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Main Panel"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
      menuItem("How is my BMP Performing?", tabName = "Performance", icon = icon("check-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Welcome",
              h1("Welcome to the BMP Performance App", align = 'center'),
              br(),
              box(status = "primary", width = 12,
                  fluidRow(
                    column(width = 12,
                           h3("Adaptive Management App for Storm Water Managers", align = "center"),
                           p("This application allows watershed managers to analyze the performance..."),
                           p("The BMP app supports monitoring designs that collect BMP influent..."),
                           br(),
                           p("This web app has three tabs (starting with the navigation pane on the left):..."),
                           br(),
                           p("Enjoy! Version 1, updated: 3-21-23")
                    )
                  ),
                  box(status = "primary", width = 12, 
                      h3("Contributors", align = "center"), 
                      p(align = "center", a(href = "https://www.sccwrp.org/about/staff/elizabeth-fassman-beck/", 'Dr. Elizabeth Fassman-Beck'),", Southern California Coastal Water Research Project"),
                      p(align = "center", a(href = "https://www.sccwrp.org/about/staff/ken-schiff/", 'Ken Schiff'),", Southern California Coastal Water Research Project"),
                      p(align = "center", a(href = "https://www.sccwrp.org/about/staff/dr-edward-tiernan/", 'Dr. Edward Tiernan'),", Southern California Coastal Water Research Project"),
                      p(align = "center", a(href = "https://www.sccwrp.org/about/staff/robert-butler/", 'Robert Butler'),", Southern California Coastal Water Research Project"),
                      p(align = "center", a(href = "https://www.sccwrp.org/about/staff/duy-nguyen/", 'Duy Nguyen'),", Southern California Coastal Water Research Project")
                  )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Performance",
              sidebarLayout(
                sidebarPanel(
                  h4("Instructions for Use:"),
                  tags$ol(
                    tags$li("Download csv template."),
                    tags$li("Populate template with BMP monitoring data.", 
                            tags$ol(type = "a",
                                    tags$li("Data must be event mean concentrations (EMCs) from paired influent-effluent sampling."),
                                    tags$li("All EMC data must be from same pollutant with same unit (e.g., TSS in mg/L)."),
                                    tags$li("Data limit is 5Mb.")
                            )
                    ),
                    tags$li("Upload data template to generate the Performance Index Plot, Score, and Summary Table."),
                    tags$li("Identify a relevant threshold for the pollutant of interest.",
                            tags$ol(type = "a",
                                    tags$li("Units must be consistent with the data template.")
                            )
                    ),
                    tags$li("Interpret BMP Performance from Index Score - is the BMP working according to expectations?",
                            tags$ol(type = "a",
                                    tags$li("Recommended data queries"),
                                    tags$li("Suggested remedial actions")
                            )
                    ),
                    tags$li("Optional:",
                            tags$ol(type = "a",
                                    tags$li("Download Performance Index Plot, Score, and Summary Table."),
                                    tags$li("Adjust threshold to see how Performance Index changes."),
                                    tags$li("Upload new data to see how Performance Index varies across pollutant class and BMP types.")
                            )
                    )
                  ),
                  downloadButton("downloadData", "Download CSV Template"),
                  fileInput("file", "Upload CSV File", accept = c(".csv")),
                  numericInput("threshold", "Threshold", value = 1, min = 0),
                  tags$img(src = "intepretation-slide.png", height = "100%", width = "100%"), width = 6
                ),
                mainPanel(
                  fluidRow(
                    column(12,
                           conditionalPanel(
                             condition = "output.dataUploaded == true", 
                             h4("Performance Index Plot"),
                             downloadButton("downloadPlot", "Download Plot")
                           ),
                           shinycssloaders::withSpinner(uiOutput("effinf_output"))
                    )
                  ),
                  fluidRow(
                    column(12,
                           conditionalPanel(
                             condition = "output.dataUploaded == true",
                             h4("Performance Index Score"),
                             downloadButton("downloadGauge", "Download Gauge (might take up to a minute)")
                           ),
                           div(style = "display: flex; justify-content: center;",
                               plotly::plotlyOutput("score.gauge", width = "700px", height = "300px")
                           )
                    )
                  ),
                  fluidRow(
                    column(12,
                           conditionalPanel(
                             condition = "output.dataUploaded == true",
                             h4("Performance Index Summary Table"),
                             downloadButton("downloadTable", "Download Summary Table")
                           ),
                           DT::dataTableOutput("gauge.table")
                    )
                  ),
                  width = 6
                )
              )
      )
    )
  )
)




# Server
server <- function(input, output, session) {
  
  # Define required columns
  required_columns <- c("influent", "effluent")
  
  output$dataUploaded <- reactive({
    !is.null(input$file$datapath)
  })
  
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  # Reactive to load and process the data when file is uploaded or threshold is updated
  processed_data <- reactive({
    req(input$file)
    req(input$threshold > 0)
    
    # Load the CSV data
    df <- tryCatch(
      {
        read.csv(input$file$datapath)
      },
      error = function(e) {
        NULL  # Return NULL if there's an error reading the file
      }
    )
    
    # Validate if the file was read successfully (not NULL) and is not empty
    validate(
      need(!is.null(df), "The uploaded file is either empty or invalid. Please upload a valid CSV file."),
      need(nrow(df) > 0, "The uploaded file is either empty or invalid. Please upload a valid CSV file.")
    )
    
    # Use Shiny's validate() and need() to check if required columns are present
    validate(
      need(all(required_columns %in% colnames(df)),
           paste("The following required columns are missing:", 
                 paste(setdiff(required_columns, colnames(df)), collapse = ", "))
      )
    )
    
    validate(
      need(all(!is.na(df$influent) & !is.null(df$influent)), "'influent' column contains NA or NULL values. Please provide valid data."),
      need(all(!is.na(df$effluent) & !is.null(df$effluent)), "'effluent' column contains NA or NULL values. Please provide valid data."),
      need(is.numeric(df$influent), "'influent' column contains non-numeric values. Please ensure all values are numeric."),
      need(is.numeric(df$effluent), "'effluent' column contains non-numeric values. Please ensure all values are numeric.")
    )
    
    
    # Calculate inf/thresh and eff/thresh
    df <- df %>%
      mutate(
        `inf/thresh` = influent / input$threshold,
        `eff/thresh` = effluent / input$threshold
      ) %>%
      mutate(
        quadrant2 = case_when(
          (`eff/thresh` < 1 & `inf/thresh` > 1) ~ "Success",
          (`eff/thresh` < `inf/thresh` & `inf/thresh` <= 1) ~ "Excess",
          (`eff/thresh` >= `inf/thresh` & `eff/thresh` < 1) ~ "Marginal",
          (`eff/thresh` >= 1 & `eff/thresh` >= `inf/thresh`) ~ "Failure",
          (`eff/thresh` >= 1 & `eff/thresh` < `inf/thresh`) ~ "Insufficient"
        )
      ) %>%
      mutate(quadrant2 = factor(quadrant2, levels = c("Success", "Excess", "Marginal", "Insufficient", "Failure")))
    
    return(df)
  })
  
  output$effinf_output <- renderUI({
    if (is.null(input$file$datapath)) {
      tags$img(src = "placeholder-eff-plot.png", height = "70%", width = "70%")
    } else {
      plotOutput("effinf_plot")
    }
  })
  
  # Plot output
  
  
  effinf_plot <- reactive({
    req(processed_data())
    df <- processed_data()
    
    ggplot(df, aes(x = `inf/thresh`, y = `eff/thresh`, color = quadrant2)) +
      geom_point(size = 3) +
      labs(
        x = "Influent / Threshold",
        y = "Effluent / Threshold",
        colour = "Performance"
      ) +
      theme_minimal(base_size = 16) +
      scale_color_manual(values = c(
        "Success" = "#117733",
        "Excess" = "#0072B2",
        "Marginal" = "#F0E442",
        "Insufficient" = "#E69F00",
        "Failure" = "#661100"
      )) +
      geom_hline(yintercept = 1, linetype = "dashed") +
      annotate("segment", x = 1, xend = 1, y = min(df$`eff/thresh`), yend = 1, linetype = "dashed", color = "black") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      coord_fixed() +
      # Explicitly set the background color to white
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })
  
  output$effinf_plot <- renderPlot({
    
    effinf_plot()

  })
  
  # Gauge output
  # Reactive expression for the gauge plot
  gauge_plot <- reactive({
    req(processed_data())
    df <- processed_data()
    scr <- get.composite.score(df, performance_col = 'quadrant2')
    get.composite.gauge(scr)  # Return the gauge plotly object
  })
  output$score.gauge <- plotly::renderPlotly({
    p <- gauge_plot()  # Use the reactive gauge plot
    # export(p, file = "test.png")
    # p
  })
  
  
  summary_dat <- reactive({
    summary_dat <- summary.table(processed_data(), threshold = input$threshold, performance_col = 'quadrant2')
    summary_dat <- summary_dat %>%
      dplyr::rename(
        Index = `Performance.Index`,  
        `# Success` = num_success,
        `# Excess` = num_excess,
        `# Marginal` = num_marginal,
        `# Insufficient` = num_insufficient,
        `# Failure` = num_failure
      )
    summary_dat
    
  })
  output$gauge.table <- DT::renderDataTable({
    dat <- summary_dat()
    
    # Render the datatable with centered text alignment
    datatable(
      dat,
      rownames = FALSE, 
      selection = 'none',
      options = list(
        dom = 'ft', 
        scrollX = TRUE, 
        ordering = FALSE, 
        searching = FALSE, 
        lengthChange = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))  # Center all columns
      ),
      escape = F
    ) %>%
      DT::formatStyle(
        columns = colnames(dat),    # Apply to all columns
        textAlign = 'center',               # Center-align text
        fontWeight = 'normal'               # Optional: make sure font weight is consistent
      )
  })
  
  # Download static sample data
  output$downloadData <- downloadHandler(
    filename = function() {
      "sample_influent_effluent.csv"
    },
    content = function(file) {
      file.copy("demo/sample_influent_effluent.csv", file)
    }
  )
  
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "Performance_Index_Plot.png"
    },
    content = function(file) {
      # Save the plot to the specified file using ggsave
      ggsave(file, plot = effinf_plot(), device = "png", width = 13.33333333333333, height = 7.33333333333333, dpi = 300)
    }
  )
  
  output$downloadGauge <- downloadHandler(
    filename = function() {
      "Performance_Index_Gauge.png"
    },
    content = function(file) {
      p <- gauge_plot()  # Get the reactive gauge plot

      #orca(p, "plot.png")      
      # Use normalizePath to convert to forward slashes
      temp_file <- normalizePath(tempfile(fileext = ".png"), winslash = "/")

      plotly::save_image(p, file = temp_file, format = "png", scale = 3)  # Save the image to temp location
      file.copy(temp_file, file)  # Copy it to the location needed for download
    }
  )
  
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      "Performance_Index_Summary.csv"
    },
    content = function(file) {
      write.csv(summary_dat(), file, row.names = FALSE)
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


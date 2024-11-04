library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("BMP Water Quality Performance Index"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions for Use:"),
      tags$ol(
        tags$li("Download csv template."),
        tags$li("Populate template with BMP monitoring data.",
                tags$ol(type = "a",  # Sub-list using letters
                        tags$li("Data must be event mean concentrations (EMCs) from paired influent-effluent sampling."),
                        tags$li("Only one pollutant may be assessed at a time.")
                )
        ),
        tags$li("Upload data template to generate the Performance Index Plot, Score, and Summary Table."),
        tags$li("Identify a relevant threshold for the pollutant of interest.",
                tags$ol(type = "a",  # Sub-list using letters
                        tags$li("Units must be consistent with the data template.")
                )
        ),
        tags$li("Optional:",
                tags$ol(type = "a",  # Sub-list using letters
                        tags$li("Download Performance Index Plot, Score, and Summary Table."),
                        tags$li("Adjust threshold to see how Performance Index changes."),
                        tags$li("Upload new data to see how Performance Index varies across pollutant class and BMP types.")
                )
        )
      ),
      downloadButton("downloadData", "Download CSV Template"),
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      numericInput("threshold", "Threshold", value = 1, min = 0),
     
      tags$img(src = "intepretation-slide.png", height = "300px", width = "100%")
    ),
    
    mainPanel(
      fluidRow(
        column(12,
               h4("Performance Index Plot"),
               shinycssloaders::withSpinner(plotOutput("effinf_plot"))
        )
      ),
      
      fluidRow(
        column(12,
               h4("Performance Index Score"),
               div(
                 style = "display: flex; justify-content: center;",  # Center the gauge output
                 shinycssloaders::withSpinner(plotly::plotlyOutput("score.gauge", width = "700px", height = "300px"))
               )
        )
      ),
      
      fluidRow(
        column(12,
               h4("Performance Index Summary Table"),
               shinycssloaders::withSpinner(DT::dataTableOutput("gauge.table"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Define required columns
  required_columns <- c("influent", "effluent")
  
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
  
  # Plot output
  output$effinf_plot <- renderPlot({
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
      geom_vline(xintercept = 1, linetype = "dashed") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      coord_fixed() # Ensure square plot
  })
  
  # Gauge output
  output$score.gauge <- plotly::renderPlotly({
    req(processed_data())
    df <- processed_data()
    scr <- get.composite.score(df, performance_col = 'quadrant2')
    get.composite.gauge(scr)
  })
  
  output$gauge.table <- DT::renderDataTable({
    summary_dat <- summary.table(processed_data(), threshold = input$threshold, performance_col = 'quadrant2') 
    summary_dat <- summary_dat %>%
      dplyr::rename(
        Index = `Performance.Index`,  
        `# Success` = `X..Success`
      ) %>%
      datatable(
        rowname = FALSE,
        options = list(
          dom = 't',       
          paging = FALSE,    
          ordering = FALSE
        )
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
}

# Run the application
shinyApp(ui = ui, server = server)


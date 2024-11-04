library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("BMP Water Quality Performance Index"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      tags$ol(
        tags$li("Download csv template."),
        tags$li("Populate template with BMP monitoring data.",
                tags$ul(
                  tags$li("Data must be event mean concentrations (EMCs) from paired influent-effluent sampling."),
                  tags$li("Only one pollutant may be assessed at a time.")
                )
        ),
        tags$li("Upload data template to app."),
        tags$li("Identify a relevant threshold for the pollutant of interest.",
                tags$ul(
                  tags$li("Units must be consistent with the data template.")
                )
        ),
        tags$li("Click 'Calculate Index' to generate the Performance Index Plot, Score, and Summary Table."),
        tags$li("Optional:",
                tags$ul(
                  tags$li("Download Performance Index Plot, Score, and Summary Table."),
                  tags$li("Adjust threshold to see how Performance Index changes."),
                  tags$li("Upload new data to see how Performance Index varies across pollutant class and BMP types.")
                )
        )
      ),
      
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      numericInput("threshold", "Threshold", value = 1, min = 0),
      actionButton("update", "Calculate Index"),
      downloadButton("downloadData", "Download CSV Template"),
      # Display the image
      tags$img(src = "intepretation-slide.png", height = "300px", width = "100%")
    ),
    
    # Separate elements using fluidRow and column for better visibility
    mainPanel(
      # First row: Plot
      fluidRow(
        column(12,
               h4("Performance Index Plot"),
               align = "center",
               plotOutput("effinf_plot")
        )
      ),
      
      # Second row: Gauge
      fluidRow(
        column(12,
               h4("Performance Index Score"),
               align = "center", 
               plotly::plotlyOutput("score.gauge", width="700px", height = "300px")
        )
      ),
      
      # Third row: Table
      fluidRow(
        column(12,
               align = "center",
               h4("Performance Index Summary Table"),
               DT::dataTableOutput("gauge.table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive to load and process the data when file is uploaded or threshold is updated
  processed_data <- eventReactive(input$update, {
    req(input$file)
    req(input$threshold > 0)
    
    # Load the CSV data
    df <- read.csv(input$file$datapath)
    
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
        #title = "BMP Water Quality Performance Index Plot"
      ) +
      theme_minimal(base_size = 16) + # Larger font sizes
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
  
  # gauge output
  output$score.gauge <- plotly::renderPlotly({
    req(processed_data())
    df <- processed_data()
    print(df)
    scr <- get.composite.score(df, performance_col = 'quadrant2')
    print('score')
    print(scr)
    get.composite.gauge(scr)
  })
  
  output$gauge.table <- DT::renderDataTable({
    summary_dat <- summary.table(processed_data(), threshold = input$threshold, performance_col = 'quadrant2') 
    print(colnames(summary_dat))
    summary_dat <- summary_dat %>%
      dplyr::rename(       # Replace with actual column name in summary.table output
        Index = `Performance.Index`,    # Replace with actual column name
        `# Success` = `X..Success`
      ) %>%
      datatable(
        rowname = FALSE,
        options = list(
          dom = 't',        # This hides the search box, the show entries dropdown, and other controls
          paging = FALSE,    # Disable pagination (optional, based on your needs),
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

library(shiny)
library(dplyr)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("BMP Water Quality Performance Index Plot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      numericInput("threshold", "Threshold", value = 1, min = 0),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      plotOutput("effinf_plot")
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
        colour = "Performance",
        title = "BMP Water Quality Performance Index Plot"
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
}

# Run the application
shinyApp(ui = ui, server = server)

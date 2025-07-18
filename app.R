# app.R

# Load global settings and all modules
source("global.R")

# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "BMP Performance Index Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
      menuItem("Water Quality Performance", tabName = "WQ", icon = icon("check-circle")),
      menuItem("Hydrology Performance", tabName = "Hydro", icon = icon("check-circle")),
      menuItem("Contact", tabName = "Contact", icon = icon("info-circle"))
    )
  ),
  
  ## Main dashboard body ----
  ## welcome tab and contact tab are defined in global.R
  ## The "wq_ui" and "hydro_ui" are modules defined in their respective files in the R folder
  dashboardBody(
    tabItems(
      welcome_tab,
      wq_ui("wq"),
      hydro_ui("hydro"),
      contact_tab
    )
  )
)

# Server ----
server <- function(input, output, session) {
  wq_server("wq")
  hydro_server("hydro")
  
  # Hydro Management Implications Table
  output$hydroManagementTable <- renderUI({
    table_data <- data.frame(
      Category = c("Success", "Excess", "Check Data", "Relative to Bypass Fraction", "Failure"),
      Indication = c(
        "Good example for future projects",
        "Might indicate: Overdesign, Oversimplified design approach, Extra drainage area being managed.",
        "QA Monitoring Program",
        "If bypass/inflow is large, might indicate back-to-back storms, construction or maintenance issue, insufficient inlet size, etc.",
        "Might indicate construction, maintenance or design issue"
      ),
      stringsAsFactors = FALSE
    )
    
    tags$table(
      style = "margin: 0 auto; width: 60%; border-collapse: collapse;",
      tags$thead(
        tags$tr(
          tags$th(style = "border: 1px solid #ddd; padding: 12px; text-align: left; background-color: #f2f2f2; font-weight: bold;", "Category"),
          tags$th(style = "border: 1px solid #ddd; padding: 12px; text-align: left; background-color: #f2f2f2; font-weight: bold;", "Indication")
        )
      ),
      tags$tbody(
        lapply(1:nrow(table_data), function(i) {
          tags$tr(
            style = ifelse(i %% 2 == 0, "background-color: #f9f9f9;", ""),
            tags$td(style = "border: 1px solid #ddd; padding: 12px; text-align: left;", table_data$Category[i]),
            tags$td(style = "border: 1px solid #ddd; padding: 12px; text-align: left;", table_data$Indication[i])
          )
        })
      )
    )
  })
}

# Entry point ----
shinyApp(ui, server)

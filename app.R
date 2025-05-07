# app.R

# Load global settings and all modules
source("global.R")

# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "BMP Performance Index Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
      #menuItem("Water Quality Performance", tabName = "WQ", icon = icon("check-circle")),
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
      #wq_ui("wq"),
      hydro_ui("hydro"),
      contact_tab
    )
  )
)

# Server ----
server <- function(input, output, session) {
  #wq_server("wq")
  hydro_server("hydro")
}

# Entry point ----
shinyApp(ui, server)

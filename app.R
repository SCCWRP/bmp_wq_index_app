# app.R

# Load global settings and all modules
source("global.R")

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
  dashboardBody(
    tabItems(
      welcome_tab,
      wq_ui("wq"),
      hydro_ui("hydro"),
      contact_tab
    )
  )
)

server <- function(input, output, session) {
  wq_server("wq")
  hydro_server("hydro")
}

shinyApp(ui, server)

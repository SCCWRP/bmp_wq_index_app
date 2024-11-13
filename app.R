library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
library(plotly)

css <- '
    .centerImage {
      display:block; 
      margin-left:auto; 
      margin-right:auto; 
      height: 60%; 
      width: 60%
    }
    
    .reference {
      padding-left: 2em;
      text-indent:-2em;
    }
    
    h3, h4 {
      color:#4679B2
    }
    
    figcaption {
      color:#4679B2;
      font-size:12px;
      font-style: italic;
      padding:1em
    }
    
    #body_div {
      padding-left: 12vw;
      padding-right: 12vw;
    }
    
    #toc_container {
      position: sticky;
      top: 20px;
      background: #f9f9f9 none repeat scroll 0 0;
      border: 1px solid #aaa;
      display: inline-table;
      float: left;
      font-size: 95%;
      margin-bottom: 1em;
      margin-left: -12vw;
      padding: 0.5em;
      width: 10vw;
      z-index: 100
    }
    
    .toc_title {
      font-weight: 700;
      text-align: left;
    }
    
    #toc_container li,
    #toc_container ul,
    #toc_container ul li {
      list-style: none;
      padding-left: 0.5em;
      text-indent: -0.5em;
    }
    '
# UI
ui <- dashboardPage(
  dashboardHeader(title = "BMP Performance Index Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
      menuItem("How is my BMP Performing?", tabName = "Performance", icon = icon("check-circle")),
      menuItem("Contact", tabName = "Contact", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "Welcome",
        h1("BMP Water Quality Performance Index Calculator", align = "center"),
        h2(HTML("This application was developed to implement the analysis Fassman-Beck et al. (2025) – A Data-Driven Index for Evaluating BMP Water Quality Performance <a href='https://www.sccwrp.org/' target='_blank'>DOI link forth coming</a>"), align = "center"),
        br(),
        div(
          style = "height: calc(100vh - 150px); overflow-y: auto; padding-right: 10px;font-size: 18px;", 
          tags$style(HTML("
            figcaption {
              font-size: 18px; /* Adjust this size as needed */
            }
          ")),
          box(
            status = "primary", width = 12,
            fluidRow(
              column(
                width = 12,
                tagList(
                  tags$head(
                    tags$style(HTML(css))
                  ),
                  # use Tex markdown in page
                  tags$div(HTML("<script type='text/x-mathjax-config' >
                    MathJax.Hub.Config({
                    tex2jax: {inlineMath: [['$','$']]},
                    tex: {tags: 'ams'}
                    });
                    </script >
                    ")),
                  # table of contents
                  tags$div(
                    id = "body_div",
                    height = "90vh",
                    tags$div(
                      id = "toc_container",
                      tags$p(
                        class = "toc_title",
                        "Contents"
                      ),
                      tags$ul(
                        class = "toc_list",
                        tags$li(
                          tags$a(href = "#Overview", "Overview"),
                        ),
                        tags$li(
                          tags$a(href = "#Framework", "Framework"),
                          
                          # tags$ul(
                          #   class = "toc_list",
                          #   tags$li(
                          #     tags$a(href = "#Water_Quality_Performance_Index-Water_Quality_Index_Score", "Water Quality Performance Index Score")
                          #   )
                          # )
                        ),
                        tags$li(
                          tags$a(href = "#Scoring", "Scoring"),
                        ),
                        tags$li(
                          tags$a(href = "#Acknowledgement", "Acknowledgement")
                        )
                      )
                    ),
                    # main overview text
                    tags$div(
                      style = "text-align: justify",
                      h3("Overview", id = "Overview"),
                      p(
                        "
                      The Water Quality Performance Index for structural Best Management Practices (BMPs) is a method to organize and interpret quantitative water quality monitoring data to help answer the question:
                      "
                      ),
                      tags$ul(
                        tags$li("Is the BMP helping to achieve receiving water quality goals?")
                      ),
                      p("
                      The BMP Water Quality Performance Index benchmarks achievements towards site-specific water quality goals while considering storm-to-storm operating conditions. The framework enables objective, unbiased assessments across a range of pollutant types, water quality goals, and/or implementations across a wide variety of landscape settings. It can be used to investigate performance for a single BMP, a group of BMPs, or to compare amongst BMPs, regardless of location, design basis, or storm conditions.  
                    "),
                      h3("Framework", id = "Framework"),
                      p(em("Does the BMP help to achieve receiving water quality objectives?", .noWS = "outside")),
                      p("The Water Quality BMP Performance Index applies a user-defined water quality threshold to partition paired influent-effluent pollutant event mean concentration data into five distinct categories. Figure WQ1 illustrates the conceptual design of the Water Quality BMP Performance Index as the relationship between paired influent-effluent data and the user-defined water quality threshold. Normalizing measured event mean concentrations by the water quality threshold benchmarks runoff as “clean” or “dirty,” and enables the user to combine data from multiple studies or BMPs for comparative evaluation."),
                      p("The basic structure and conceptual design of the Performance Index is illustrated through 
                        an application using total suspended solids (TSS) (n=9 data pairs, Figure WQ1a-d from 18 bioretention cells extracted from the International Stormwater BMP Database. Each data point represents paired influent-effluent EMC data for an individual storm event monitored at a single bioretention BMP. The same data are represented in each plot. Figures WQ1a-c illustrate three distinct ways that the event pairs can be dissected, while Figure WQ1d labels the regions that emerge from the superposition of these dividing lines in terms of their contribution to BMP treatment objectives."),
                      tags$figure(
                        tags$img(src = "figure-wq-1.png",width = '70%', height = '70%', class = "centerImage"),
                        tags$figcaption("
                            Figure WQ1. Water Quality BMP Performance Index conceptual design using total suspended solids from 18 bioretention 
                                        BMPs in the ", a(href = "https://bmpdatabase.org/", target = "_blank", "International Stormwater BMP Database", style = "color: #0000EE;", .noWS = "outside"), ": (a) Does influent require treatment 
                                        to meet water quality thresholds?; (b) Does effluent exceed a water quality threshold?; (c) Are pollutants removed or exported?; 
                                        and (d) Translating numerical data into narrative outcomes.  N=309 total site-events. 69 Success data points and 10 Insufficient data points not shown to maintain figure clarity.
                                        ")
                      ),
                      p("The Water Quality Performance Index investigates paired influent-effluent pollutant flow-weighted event mean concentrations. 
                        The focus on event mean concentrations rather than mass loads yields critical information on whether a BMP successfully provides some 
                        treatment mechanism(s) to transform or sequester pollutants, such as filtration, sorption, sedimentation, degradation, or volatilization. 
                        Runoff retention is not considered Water Quality Performance Index."),
                      p("Figure WQ1d contains categorical performance determinations of the paired event data based on their positions relative to the water 
                        quality threshold dividing lines depicted in Figures 1a-c. 
                        The Water Quality BMP Performance Index identifies results among five descriptive categories as follows in order of their managerial 
                        preference (Fig. WQ2):"),
                      tags$ul(
                        tags$li("Success – The most preferred outcome, the runoff entering the BMP is “dirty” for the pollutant of interest yet treated effluent is at least as “clean” as the water quality threshold. The BMP provides meaningful pollutant removal mechanism(s)."),
                        tags$li("Excess – The influent is considered “clean,” for the pollutant of interest.  Treatment is achieved by the BMP but not strictly needed because the concentration is already below water quality threshold. "),
                        tags$li("Marginal – The influent concentration is considered “clean” for the pollutant of interest, but the BMP contributes the pollutant rather than removes it. Exported pollutants degrade overall water quality but not enough to create failure conditions because the concentration is below water quality threshold."),
                        tags$li("Insufficient – The runoff entering the BMP is “dirty” for the pollutant of interest. The BMP provides some mechanism(s) to treat the runoff, but additional treatment is needed to meet water quality objectives."),
                        tags$li("Failure – The BMP exports pollutants at concentrations greater than water quality threshold, potentially exacerbating downstream water quality conditions. Corrective action is required."),
                      ),
                      p("Data points falling on dividing lines are assigned to the more protective category. Data pairs categorized as Marginal or Excess may incorporate relatively more noise or uncertainty associated with detection of low concentrations. A BMP that exports pollutants (effluent concentration exceeds influent concentration) is considered a Failure for any influent concentration unless the water quality threshold is not exceeded (i.e., Marginal). Insufficient BMPs are considered less desirable than Marginal BMPs due to the continued elevated pollutant contribution to the receiving waters; resources should be allocated towards additional treatment before rehabilitating Marginal BMPs. "),
                      tags$figure(
                        style = "text-align: center;",
                        tags$img(src = "placeholder-eff-plot.png", style = "width:60%; height:60%;"),
                        tags$figcaption("Figure WQ2. Water Quality Performance Index Framework")
                      ),
                      
                      h3("Scoring", id = "Scoring"),
                      p("The categorical outcome introduced above provides a useful determination of whether a BMP is contributing towards achieving downstream receiving water goals, or if corrective actions are needed for a pollutant type, or for tracking performance by individual storm. A key design criterion for the BMP Performance Index is the ability to capture long-term trends in the data, whether as temporal patterns or comparing amongst BMPs (an individual BMP or amongst a type of BMP) or pollutants directly. To this end, managers need a way to distill the narrative behavior of a suite of monitored BMPs into a single value that can be tracked over time or compared between BMP datasets. In other words, a method to collate storm-by-storm categorical outcomes into a single descriptor of performance is of interest."),
                      p("The Water Quality Index “Score” uses a weighted average to evaluate the overall performance, wherein each category has an intrinsic weight (level of importance) associated with the managerial preference of that outcome. The weighted average is the sum of the proportion of data in each category multiplied by the weighting factor of that category as in Equation 1: "),
                      HTML("$$\\begin{equation} S = \\sum_{i=1}^nw_iX_i \\tag{1} \\end{equation}$$") |> withMathJax(),
                      p("where $S$ is the weighted average (i.e., the Score), $w_i$ is the weighting factor for each category, $i$, and $X_i$ is the proportion of paired influent-effluent event mean concentration data in each category."),
                      p("The value of the weighting factor per category is derived from the managerial implication of the Score. The final index score is intended to be: transparent and quantitative, reflect the underlying distribution of the categorical outcomes, strongly penalize the failure conditions that conclusively signal BMP performance disruption, and contribute to clear management decision making."),
                      p("Many alternatives to the category weights were considered. The categorical weighting scheme that best fit the criteria listed above breaks down as:"),
                      tags$ul(
                        tags$li("Success = 0"),
                        tags$li("Excess = 1"),
                        tags$li("Marginal = 3"),
                        tags$li("Insufficient = 4"),
                        tags$li("Failure = 10"),
                      ),
                      tags$figure(
                        style = "text-align: center;",
                        tags$img(src = "placeholder-eff-plot-archived-2.png", style = "width:60%; height:60%;"),
                        tags$figcaption("Figure WQ3. Managerial implications if all data were to fall into a single category")
                      ),
                      p("
                      This weighting scheme (i.e., set of wi weighting factors in Eq. 1) reflects the underlying preference of resource allocation to BMPs presenting in each category (Figure WQ2). Excess BMPs are not substantively different from Success BMPs in the eyes of a stormwater infrastructure manager (neither requires intervention), so they receive similar scores. However, if all data were to be found in the Excess category, it indicates the pollutant of interest is not of concern at the specific location and potentially calls for better source identification in future applications (i.e., are BMP siting criteria appropriate with respect to pollutant loading assumptions?). Marginal and Insufficient BMPs are both acknowledged to be worse outcomes than Success or Excess. Marginal BMPs may suffer from a design, construction, or maintenance deficiency but not to an extent that corrective action is immediately required. It is acknowledged that Marginal and Excess BMPs are characterized by low concentrations in influent and effluent, relative to the water quality threshold; there may be some influence of noise in this data as low concentrations are more difficult to measure accurately. Insufficient-category BMPs are generally considered deserving of additional resource allocation due to the continued elevated pollutant contributions to the receiving water. Additional treatment would be needed to ultimately achieve receiving water goals, which could be achieved by introducing additional pollutant removal mechanisms, either through a treatment train, retrofitting the existing BMP, or treating runoff elsewhere in the watershed. BMPs exhibiting water quality outcomes in the Failure condition received a high penalty in the mash-up score to indicate that these sites clearly indicate unacceptable performance that require near-term corrective action. A systematic error in design or construction may lead to a BMP type determined as Failure for a particular pollutant.
                    "),
                      p("
                    Figure WQ4 illustrates the transition points and how the Index Score contributes to decision making for stormwater managers. Additional lines of inquiry are included for technical designers and planners to further investigate the broad management action zones. The Index Score reflects the weighted average across all the BMP site-events available in the dataset. Low scores are associated with the best / most preferred outcomes. Where at least half of the data fall into a single category, the suggested management action reflects the implication of that category.  
                    "),
                      tags$figure(
                        style = "text-align: center;",
                        tags$img(src = "intepretation-slide.png", style = "width:60%; height:60%;"),
                        tags$figcaption("Figure WQ4. Water Quality Performance Index Score color bar with managerial interpretation of the score. A framework for data queries to investigate the Score more deeply is provided.")
                      ),
                      
                      p("
                      For Index Scores between 0.0 – 2.0, there are no recommended management actions as the set of BMPs assessed, or individual BMP, is performing well as a whole, though technical personnel may break this category down further. An Index Score of 0.0 – 0.5 indicates a high prevalence of Success and Excess outcomes suggesting the BMPs are successfully achieving treatment goals. An Index Score in the 0.5 – 1.0 range may hint that untreated runoff is not a significant source of the pollutant of concern; most of the data will be in the Excess or Marginal conditions. Index Scores in the 1.0 – 2.0 range will have some fraction of Marginal or Insufficient outcomes; managers should consider watching this set of BMPs for a declining trend. In all cases where BMPs consistently do not achieve Success or Excess outcomes, potential improvements to the design, construction, or maintenance is warranted to maximize water quality benefits.
                      "),
                      
                      p("
                      Water Quality Performance Index Scores in the 2.0 – 5.0 range constitute BMP sets with ambiguous or mixed outcomes that require additional investigation to discern management implications. Index Scores in the range 2.0 – 3.5 should be investigated for a high prevalence of Insufficient site-events. If more than half of the dataset falls into the Insufficient category, additional treatment is needed. Alternatively, this Index Score might be achieved by mostly good effluent quality but a handful of Fails, which may not obviously point to any remedial action. Index Scores in the 3.5 – 5.0 range indicate that additional treatment and perhaps BMP remediation are needed, additional investigation into the prevalence of Insufficient and Failure condition BMPs is to determine where the additional treatment would be most beneficial. When the Water Quality Performance Index is used to evaluate a group of BMPs, the easiest query is to determine if most/all of the data of concern (i.e., Insufficient and Failure categories) are from a single BMP within the group. If a single BMP contains many of the negatively performing site-events, the Index Score for that slice of data will be correspondingly worse and fall into a different management action zone.
                      "),
                      
                      p("
                      Water Quality Performance Index Scores greater than 5.0 indicate a significant fraction of Failures. 
                      Individual or types of BMPs that fall into this category are consistently failing to meet water quality objectives while potentially 
                      exacerbating downstream water quality. Corrective actions such as performing BMP maintainance, and/or investigating design and/or construction are warranted. Design procedures, or sources of materials used to construct the BMP may require revision to prevent failures in future installations.
                      "),
                      
                      h3("Acknowledgement", id = "Acknowledgement"),
                      p(
                        "
                    Funding for the BMP Performance Index was provided by the County of Los Angeles Department of Public Works. Index development was conducted by the Southern California Coastal Water Research Project (SCCWRP) with input from a 6-person advisory group. The advisory group was composed of representatives from California’s regulatory agencies, watershed managers, and primary permittees, as well as industry, academia, and a non-governmental organization. Participation in the Advisory Committee does not reflect endorsement of the Performance Index by individuals or the organization they represent.
                    " 
                      ),
                      
                    )
                  )
                )
              )
            ),
            box(
              status = "primary", width = 12,
              fluidRow(
                column(
                  width = 6,
                  h3("Contributors", align = "center"),
                  div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    tags$ul(
                      style = "list-style-position: inside; text-align: left; padding: 0;",
                      tags$li("Dr. Elizabeth Fassman-Beck"),
                      tags$li("Ken Schiff"),
                      tags$li("Dr. Edward Tiernan"),
                      tags$li("Robert Butler"),
                      tags$li("Duy Nguyen")
                    ),
                    tags$img(src = "sccwrp-logo.png", width = "30%", height = "30%", style = "margin-top: 10px;")
                  )
                ),
                column(
                  width = 6,
                  h3("Los Angeles County Department of Public Works Project Manager: Frank Cheng", align = "left"),
                  h3("Advisory Group Members", align = "center"),
                  div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    tags$ul(
                      style = "list-style-position: inside; text-align: left; padding: 0;",
                      tags$li("Dr. Bridget Wadzuk, Villanova University"),
                      tags$li("Annelisa Moe, Heal the Bay"),
                      tags$li("Richard Boon, Riverside County Flood Control District"),
                      tags$li("Jane Clary, Wright Water"),
                      tags$li("Chris Beegan, California State Water Resources Control Board"),
                      tags$li("LB Nye, Los Angeles Regional Water Quality Control Board"),
                      tags$li("Bhaskar Joshi, Caltrans")
                    )
                  )
                )
              )
            )
            
            
            
          )
          
        )
        
      ),
      
      # Second tab content
      tabItem(
        tabName = "Performance",
        tags$style(HTML("
    #Performance * {
      font-size: 18px; /* Adjust the size as needed */
    }
  ")),
        sidebarLayout(
          sidebarPanel(
            
            h4("Instructions for Use:"),
            tags$ol(
              tags$li("Download csv template."),
              tags$li(
                "Populate template with BMP monitoring data.",
                tags$ol(
                  type = "a",
                  tags$li("Data must be event mean concentrations (EMCs) from paired influent-effluent sampling."),
                  tags$li("BMPs and/or specific events with full-capture (i.e., no effluent runoff) are treated as effluent EMC equals 0. Influent EMCs from full-capture events are still required."),
                  tags$li("All EMC data must be from the same pollutant type with consistent units (e.g., TSS in mg/L)."),
                  tags$li("Data limit is 5Mb.")
                )
              ),
              tags$li("Upload data template to generate the Performance Index Plot, Score, and Summary Table."),
              tags$li(
                "Identify a relevant threshold for the pollutant of interest.",
                tags$ol(
                  type = "a",
                  tags$li("Units must be consistent with the data template.")
                )
              ),
              tags$li(
                "Interpret BMP Performance from Index Score - is the BMP working according to expectations?",
                tags$ol(
                  type = "a",
                  tags$li("Recommended data queries and suggested remedial actions can be found in the project manuscript.")
                )
              ),
              tags$li(
                "Optional:",
                tags$ol(
                  type = "a",
                  tags$li("Download Performance Index Plot, Score, and Summary Table."),
                  tags$li("Adjust threshold to see how Performance Index changes."),
                  tags$li("Upload new data to see how Performance Index varies across pollutant class and BMP types.")
                )
              )
            ),
            downloadButton("downloadData", "Download CSV Template"),
            fileInput("file", "Upload CSV File", accept = c(".csv")),
            textInput("pollutant_name", "Pollutant Name/Unit (optional, e.g. Copper, µg/L)", value = ""),
            numericInput("threshold", "Threshold (must be same unit as EMC data)", value = 1, min = 0),
            tags$img(src = "intepretation-slide.png", height = "100%", width = "100%"),
            width = 6
          ),
          uiOutput("mainPanelUI"),
        )
      ),
      tabItem(
        tabName = "Contact",
        h1("Contact Us", align = "center"),
        br(),
        box(
          status = "primary", width = 12,
          p("For more information about the BMP Performance App, please contact: stormwater@sccwrp.org"),
          p(
            "We value your feedback! Please share any suggestions or questions by filling out our ",
            a(href = "https://forms.office.com/Pages/ResponsePage.aspx?id=PfKopOEaHEuZAuqhUwKBkPM1sDQYvi9Ogmxf5DwlQa9URElHMkpKNVdCSDc1NElHTkVaWEg5MVBGQS4u", "Feedback Form", target = "_blank"),
            "."
          )
        )
      )
    )
  )
)




# Server
server <- function(input, output, session) {
  
  
  observeEvent(input$file, {
 
    processed_data <- reactiveVal(NULL) # Reset processed_data to NULL on file change
  
  })
  
  # Conditionally render the mainPanel based on the value of 'threshold'
  output$mainPanelUI <- renderUI({
    
    # Check if threshold is NULL, negative, or empty
    if (is.null(input$threshold) || input$threshold == "" || input$threshold <= 0 || is.na(input$threshold)) {
      return(NULL)  # Hide mainPanel by returning NULL
    } else {
      # Show mainPanel if threshold is valid
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
                   #uiOutput("downloadButtonUI")
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
        
      )}
  })
  
  # buttonText <- reactiveValues(text = "Download Gauge (might take up to a minute)")
  # output$downloadButtonUI <- renderUI({
  #   downloadButton("downloadGauge", label = buttonText$text)
  # })
  
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
    

    
    df
    
  })
  
  output$effinf_output <- renderUI({
    if (is.null(input$file$datapath)) {
      tags$img(src = "placeholder-eff-plot.png", height = "95%", width = "95%")
    } else {
      plotly::plotlyOutput("effinf_plot")
    }
  })
  
  # Plot output
  
  
  effinf_plot <- reactive({
   
    df <- processed_data()

    ggplot(df, aes(x = `inf/thresh`, y = `eff/thresh`, color = quadrant2)) +
      geom_point(size = 3) +
      labs(
        x = "Influent / Threshold",
        y = "Effluent / Threshold",
        colour = "Performance",
        title = ifelse(
          input$pollutant_name == "",
          paste("Threshold:", input$threshold),
          paste(input$pollutant_name, "- Threshold:", input$threshold)
        )
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
        panel.background = element_rect(fill = "white", color = "black")
      )
  }) |> bindEvent(input$threshold, input$pollutant_name, input$file)
  
  output$effinf_plot <- plotly::renderPlotly({
    ggplotly(effinf_plot())  %>%
      layout(
        xaxis = list(
          tickmode = "auto",
          dtick = 0.1  # Adjust dtick value as per requirement for finer ticks
        ),
        yaxis = list(
          tickmode = "auto",
          dtick = 0.1  # Adjust dtick for finer control on y-axis
        )
      )
  }) |> bindEvent(input$file, input$threshold, input$pollutant_name)
  
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
        `Index Score` = `Performance.Index`,  
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
      
      buttonText$text <- "Downloading..."
      
      p <- gauge_plot()  # Get the reactive gauge plot
      
      #orca(p, "plot.png")      
      # Use normalizePath to convert to forward slashes
      temp_file <- normalizePath(tempfile(fileext = ".png"), winslash = "/")
      
      plotly::save_image(p, file = temp_file, format = "png", scale = 3)  # Save the image to temp location
      file.copy(temp_file, file)  # Copy it to the location needed for download
      
      # Reset button text after download completes
      buttonText$text <- "Download Gauge (might take up to a minute)"
      
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


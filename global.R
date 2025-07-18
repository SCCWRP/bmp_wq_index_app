# global.R

# Load libraries ---- --------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(plotly)
library(glue)
library(stringr)
library(readxl)

# janitor is imported basically just for the round_half_up function
# We "library" the entire package for the sake of being able to more easily read what the dependencies are for this shiny app
library(janitor) 

# Source helper functions and modules ------------------------------------------------------------------------------------------------------------------------
source("R/utils.R")
source("R/gauge.R")
source("R/wq_module.R")
source("R/hydro_module.R")

# Global UI settings ------------------------------------------------------------------------------------------------------------------------------------------
global_font_family <- "Arial"
global_point_size <- 4
UNCERTAINTY_BUFFER <- 0.2

# Plot color and shape settings --------------------------------------------------------------------------------------------------------------------------------
# For WQ Index Plot
perf_colors <- c(
  "Success" = "#117733",   
  "Excess" = "#0072B2",    
  "Marginal" = "#d6cf04",  
  "Insufficient" = "#E69F00",
  "Failure" = "#661100",   
  "Comparison BMP Success" = "#117733",     
  "Comparison BMP Excess" = "#0072B2",      
  "Comparison BMP Marginal" = "#d6cf04",    
  "Comparison BMP Insufficient" = "#E69F00",
  "Comparison BMP Failure" = "#661100"      
)

# For WQ Index Plot
perf_shapes <- c(
  "Success" = 16,
  "Excess" = 17,
  "Marginal" = 18,
  "Insufficient" = 8,
  "Failure" = 15,
  "Comparison BMP Success" = 1,
  "Comparison BMP Excess" = 2,
  "Comparison BMP Marginal" = 5,
  "Comparison BMP Insufficient" = 4,
  "Comparison BMP Failure" = 0
)

# For Hydrology Index Plot
hydro_colors <- c(
  "Success" = "#117733",
  "Excess" = "#0072B2",
  "Neutral" = "black",
  "Failure" = "#661100",
  "Small Storm With Bypass" = "purple"
)

# For Hydrology Index Plot
hydro_shapes <- c(
  "Success" = 16,       # Filled Circle
  "Excess" = 17,        # Filled Triangle
  "Check Data" = 8,      # Asterisk
  "Small Storm With Bypass" = 18,   # Filled Diamond
  "Failure" = 15       # Filled Square
)

# For Hydrology Index Plot
designplot_colors <- c(
  "Success" = "#117733",
  "Excess" = "#0072B2",
  "Check Data" = "#ff6f00",
  "Failure" = "#661100",
  "Small Storm With Bypass" = "purple"
)

# For Hydrology Index Plot
bypass_shapes <- c(
  "Bypass" = 1,
  "No Bypass" = 16 
)


# Acceptable input file types ------------------------------------------------------------------------------------------------------------------------------------
# for the upload routines

# if the uploaded file is a csv, then input$hydrofile$type will be "text/csv"
# if it is an xlsx file, then input$hydrofile$type will be "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
# if it is an xls, then input$hydrofile$type will be "application/vnd.ms-excel"
ACCEPTABLE_FILETYPES <- c(
  "text/csv"
  # , # csv "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",# xlsx
  # "application/vnd.ms-excel" # xls
)

# As of now we will only accept csv files, as it was originally intended as such (to only accept CSV's)
# Accepting only CSV's will keep it simple, unless we get requests for the application to accept excel files as well.
ACCEPTABLE_FILETYPE_COMMON_NAMES <- c(
  'csv'
  # ,'xlsx',
  # 'xls'
)



# Welcome tab content ---------------------------------------------------------------------------------------------------------------------------------------------
welcome_tab <- tabItem(
  tabName = "Welcome",
  tags$head(
    includeCSS("www/css/welcome.css")
  ),
  h1("BMP Water Quality Performance Index Calculator", align = "center"),
  div(style = "margin: 10px auto; max-width: 1200px; text-align: left; padding: 20px; background-color: #f8f9fa; border-radius: 4px; border-left: 2px solid #007bff;",
      h3(HTML("Fassman-Beck, E.A., Tiernan, E.D., Cheng, K.L., Schiff K.C. (2025)<br>A data-driven index for evaluating BMP water quality performance<br><i>Water Research</i>, 282(2025) 123769<br><a href='https://doi.org/10.1016/j.watres.2025.123769' target='_blank'>https://doi.org/10.1016/j.watres.2025.123769</a>"), style = "margin: 0;")
  ),
  br(),
  div(
    class = 'main-welcome-div',
    box(
      status = "primary", width = 12,
      fluidRow(
        column(
          width = 12,
          tagList(
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
                    tags$a(href = "#WQFramework", "WQ Index Framework")
                  ),
                  tags$li(
                    tags$a(href = "#WQScoring", "WQ Index Scoring"),
                  ),
                  tags$li(
                    tags$a(href = "#HydroFramework", "Hydrology Framework")
                  ),
                  tags$li(
                    tags$a(href = "#HydroScoring", "Hydrology Scoring")
                  ),
                  tags$li(
                    tags$a(href = "#Acknowledgement", "Acknowledgement")
                  )
                )
              ),
              ## main overview text ----
              tags$div(
               tags$p(h3(HTML("Here is a link to a <a href='https://forms.office.com/Pages/ResponsePage.aspx?id=PfKopOEaHEuZAuqhUwKBkLfF7RbFiCdKlqnmjECrBFxUM1BYMDlSQ0ZOQTY3NkNQVjZQUDZIRjZQUC4u'>feedback form</a>"))),
               tags$p(h3(HTML("Here is a link to the <a href='https://github.com/SCCWRP/bmp_wq_index_app/'>Github repository</a>")))
              ),
              tags$div(
                style = "text-align: justify",
                h2("Overview", id = "Overview"),
                tags$p("The BMP Performance Index is a method to organize and interpret quantitative water quality and 
           hydrologic monitoring data to help answer the questions:"),
                
                tags$ul(
                  tags$li("Is the BMP helping to achieve receiving water quality goals?"),
                  tags$li("Is the BMP capturing runoff as intended based on its design?")
                ),
                
                tags$p("The BMP Performance Index benchmarks achievements towards site-specific water quality and runoff capture goals while considering storm-to-storm operating conditions. The framework enables objective, unbiased assessments across a range of pollutant types, water quality goals, storm event magnitudes, design specifications, and/or implementation across a wide variety of landscape settings. It can be used to investigate performance for a single BMP, a group of BMPs, or to compare amongst BMPs, regardless of location, design basis, or storm conditions. "),
                
                tags$p("Evaluating performance based on what really happens in operation was an important motivation for developing this framework for analysis. Technical design approaches typically represent only a subset of anticipated field conditions, and simplify BMP operation. For example, BMP design is often based on static assumptions (e.g., the BMP fills up with water then drains), rather than the dynamic operation that occurs in practice (e.g., runoff flows into and out of the BMP dynamically during storms). Modifications from design “on paper” are common during construction, while the maintenance condition can influence system behavior and change over time. The implications of dynamic operation, as-constructed configuration and maintenance condition is implicitly captured in field monitoring data. As such, the BMP Performance Index was developed and tested based on field monitoring data. It is feasible to apply modeled or laboratory-derived data, based on the data structure of the Performance Index, but implications have not been investigated by the development team."),
                
                tags$p("Essential data inputs to apply the Water Quality Performance Index (WQPI) include paired BMP influent-effluent flow-weighted pollutant event mean concentrations, and user-defined thresholds for water quality goals. Multiple pollutants may be evaluated concurrently. The Hydrology Performance Index (HPI) requires field-measured precipitation, paired BMP influent-effluent, and bypass or overflow runoff volumes, as well as design metadata such as the depth of the design storm used to size the BMP and the design runoff capture volume. All data are expected to be checked against all relevant quality assurance and quality control protocols prior to applying it to the Performance Index."),
                
                ## Water Quality Section of Overview ----
                h2("Water Quality Index Framework", id = "WQFramework"),
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
                        Runoff retention is not considered within the WQPI. It is assessed within the HPI – see below."),
                p("Figure WQ1d contains categorical performance determinations of the paired event data based on their positions relative to the water 
                        quality threshold dividing lines depicted in Figures 1a-c. 
                        The WQPI identifies results among five descriptive categories as follows in order of their managerial 
                        preference (Fig. WQ2):"),
                tags$ul(
                  tags$li("Success – The most preferred outcome, the runoff entering the BMP is “dirty” for the pollutant of interest yet treated effluent is at least as “clean” as the water quality threshold. The BMP provides meaningful pollutant removal mechanism(s)."),
                  tags$li("Excess – The influent is considered “clean,” for the pollutant of interest.  Treatment is achieved by the BMP but not strictly needed because the concentration is already below water quality threshold. "),
                  tags$li("Marginal – The influent concentration is considered “clean” for the pollutant of interest, but the BMP contributes the pollutant rather than removes it. Exported pollutants degrade overall water quality but not enough to create failure conditions because the concentration is below water quality threshold."),
                  tags$li("Insufficient – The runoff entering the BMP is “dirty” for the pollutant of interest. The BMP provides some mechanism(s) to treat the runoff, but additional treatment is needed to meet water quality objectives."),
                  tags$li("Failure – The BMP exports pollutants at concentrations greater than water quality threshold, potentially exacerbating downstream water quality conditions. Corrective action is required."),
                ),
                p("Data points falling on dividing lines are assigned to the more protective category. Data pairs categorized as Marginal or Excess may incorporate relatively more noise or uncertainty associated with detection of low concentrations. A BMP that exports pollutants (effluent concentration exceeds influent concentration) is considered a Failure for any influent concentration unless the water quality threshold is not exceeded (i.e., Marginal). Insufficient BMPs are considered less desirable than Marginal BMPs due to the continued elevated pollutant contribution to the receiving waters; resources should be allocated towards additional treatment before rehabilitating Marginal BMPs. After the data are normalized, the values are rounded to two decimal places."),
                tags$figure(
                  style = "text-align: center;",
                  tags$img(src = "WQIndexOverviewPlot.jpg", style = "width:60%; height:60%;"),
                  tags$figcaption("Figure WQ2. Water Quality Performance Index Framework")
                ),
                
                h3("Water Quality Index Scoring", id = "WQScoring"),
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
                  tags$img(src = "intepretation-slide.png", style = "width:70%; height:70%;"),
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
                
                ## Hydrology Section of Overview ----
                h2("Hydrology Performance Index Framework", id = "HydroFramework"),
                tags$p(em("Does the BMP capture runoff as-intended according to design assumptions?")),
                
                tags$p("The BMP Hydrology Performance Index (HPI) uses the design parameters of the BMP and monitored rainfall and runoff volumes to establish whether the BMP is managing the expected amount of runoff. The HPI interacts with the WQPI in a causal fashion; the water needs to physically enter the BMP to receive treatment.  "),
                
                tags$p("Figure H1 illustrates the conceptual design of the HPI as the relationship between paired influent-effluent hydrographs (and includes bypass or overflow), the size of the monitored storm event, and the design parameters of the BMP. The vertical axis is determined by the volume retained (i.e., Volume In minus Volume Out) in the monitored storm event normalized by the BMP design volume (i.e., the volume of water the BMP should capture in a design storm event). Runoff volumes are calculated by integrating the field measured hydrographs with time. The horizontal axis is the total precipitation depth of the monitored event normalized by the total precipitation depth of the design storm event used to size the BMP."),
                
                tags$p("Narrative descriptions of how a BMP is expected to behave (for flow management) under operating conditions are given by Figures H1a-c. Every BMP is (or should be) uniquely sized to manage the runoff from its contributing drainage area. The size of a BMP is commonly determined through a series of technical calculations based on the rainfall-runoff relationship of a site. Normalizing field-monitored capture volume and precipitation depth by the design factor effectively evaluates a BMP’s performance relative to what the design engineer expected the BMP to do, at that site. It provides a benchmark for determining whether a monitored event is “big” or “small.” The normalization step enables the user to combine data from multiple studies or BMPs for comparative evaluation."),
                
                
                tags$figure(
                  tags$img(src = "FigureH1.png", class = "centerImage", style="height:600px;width:800px"),
                  tags$figcaption(
                    "Figure H1. BMP Hydrology Performance Index conceptual design illustrated using data from selected bioretention BMPs in the ",
                    a(href = "https://bmpdatabase.org/", target = "_blank", "International Stormwater BMP Database", style = "color: #0000EE;", .noWS = "outside"), ": (a) Should the BMP capture all runoff for the monitored storm, 
                    or should bypass/overflow be expected?; (b) Was the total volume captured greater or less than expectations according to the BMP’s design volume?; 
                    (c) A measurement tolerance is acknowledged in the volume retained axis due to measurement uncertainty"
                  )
                ),
                
                tags$p("Figure H1d contains categorical performance determinations of the paired hydrograph-storm event data normalized by the design parameters. The HPI identifies results among four descriptive categories as follows in order of their managerial preference: "),
                
                tags$ul(
                  tags$li(strong("Success"), " - The most preferred outcome, the BMP retained the expected volume of runoff for the size of the monitored storm event. Monitored storms with bypass or overflow can be considered successes, provided the rainfall exceeded the design storm depth (i.e., it was a large event), and BMP filled at least to its design volume."),
                  tags$li(strong("Excess"), " - During a large event, the BMP retained a greater volume than the design volume. This can happen when infiltration occurs concurrently with inflow (i.e. dynamic BMP operation) or other hydrodynamic process, and/or may indicate that the BMP is larger than necessary to manage the design storm event. Excess capture may also be measured if changes in the drainage area create more runoff than anticipated, but the BMP is able to manage the additional hydrologic load."),
                  tags$li(strong("Relative to Bypass Fraction (BF)"), " - When the BMP is full, some bypass may occur without cause for concern; e.g., for data points in the top right performance zone. Small storms should not produce bypass as the BMP is not expected to fill (the bottom left performance zone); however, a relatively large volume or duration of bypass measured relative to the inflow might indicate undersized or blocked inlets, poor grading, or additional flow from an unanticipated drainage area. Conversely, a small trickle or very short duration of bypass relative to the inflow is likely not a practical concern. The volume of bypass relative to the inflow volume is therefore an important consideration for small storms only. The ratio is defined herein as the bypass fraction (BF), and is determined as:")
                ),
                
                tags$figure(
                  tags$img(src = "BypassFractionSchematic.jpg", class = "centerImage", style="height:291px;width:346px;"), # exact dimensions of the jpg file
                  tags$figcaption(style = "text-align:center;", "Figure H2. Bypass fraction")
                ), 
                
                tags$p(HTML("$$\\text{BF} = \\frac{V_{\\text{Bypass}}}{V_{\\text{Total}}} \\qquad \\text{Eq. 2}$$")),
                
                tags$ul(
                  
                  tags$li(strong("Check Data"), " – During a relatively small event, the BMP retained a greater volume than the design volume. The design capture volume is established based on expected runoff from a design storm, thus any event smaller than the design storm should not fill up a BMP (i.e., physically this condition should not occur, but measurements might indicate otherwise). Datapoints in the Check Data category can indicate issues with monitoring equipment, anomalies in the monitoring configuration or a significant change in catchment land use or increased drainage area extent sending more runoff to the BMP than it was designed to manage."),
                  tags$li(strong("Failure"), " – The least preferred outcome, the BMP failed to retain a volume of runoff at least equivalent to the design volume, despite adequate runoff generated by a large storm. Failure conditions are typically associated with high bypass or overflow. Failing BMPs might indicate a problem with the maintenance condition, insufficiently sized inlets or other design or construction flaws."),
                  
                ),
                tags$p(
                  "Data points falling on horizontal dividing lines are considered success, since the dividing lines are introduced to account for measurement uncertainty. Data points falling on vertical dividing lines below the horizontal success band are assigned to the more protective category, in this case it is assigned a failure. Data points falling on the vertical line above the horizontal success band are assigned to the excess category, since the implications for the check data category are specific and unique. After the data are normalized, the values are rounded to two decimal places."),
                
                tags$figure(
                  tags$img(src = "HydroIndexOverviewPlot.jpg", class = "centerImage", style="height:612px; width:797px;"), # exact dimensions of the jpg file
                  tags$figcaption("Figure H3. Hydrology Performance Index Framework")
                ), 
                
                h3("Hydrology Performance Index Scoring", id="HydroScoring"),
                tags$p("The categorical outcome introduced above provides a useful determination of whether a BMP is capturing runoff as intended by design, or if corrective actions are needed. A key design criterion for the BMP Performance Index is the ability to capture long-term trends in the data, whether as temporal patterns or comparing amongst BMPs (an individual BMP or amongst a type of BMP). To this end, managers need a way to distill the narrative behavior of a suite of monitored BMPs into a single value that can be tracked over time or compared between BMP datasets. In other words, a method to collate storm-by-storm categorical outcomes into a single descriptor of performance is of interest."),
                tags$p("The HPI score uses a weighted average to determine the overall performance, wherein each category has an intrinsic weight (level of importance) associated with the managerial preference of that outcome. The weighted average is the sum of the proportion of data in each category multiplied by the weighting factor of that category as in Equation 3:"),
                
                tags$p(HTML("$$\\text{HPI} = \\sum_{i=1}^{n} w_i X_i \\qquad \\text{Eq. 3}$$")),
                
                tags$p("where HPI is the weighted average (i.e., the Hydrology Performance Index score), ",
                       HTML("$w_i$"), " is the weighting factor for each category, ",
                       HTML("$i$"), ", and ", HTML("$X_i$"), " is the proportion of paired data in each category."),
                
                tags$p("The value of the weighting factor per category is derived from the managerial implication of the HPI score.
           The HPI score is designed to: be transparent and quantitative, reflect the underlying distribution of the
           category preferences, strongly penalize the failure conditions that conclusively signal BMP performance
           disruption, and contribute to clear management decision making."),
                
                tags$p("The categorical weighting scheme that best fit the criteria listed above breaks down as:"),
                
                tags$ul(
                  tags$li(HTML("Success (<i>w<sub>success</sub></i>) = 0")),
                  tags$li(HTML("Excess (<i>w<sub>excess</sub></i>) = 1")),
                  tags$li(HTML("Relative to Bypass Fraction (<i>w<sub>BF</sub></i>) = see Eq. 4")),
                  tags$li(HTML("Check Data (<i>w<sub>CD</sub></i>) = 4")),
                  tags$li(HTML("Failure (<i>w<sub>failure</sub></i>) = 10"))
                ),
                
                tags$p('Data points in the Relative to BF performance zone are assigned a variable weighting score wBF derived
           from the magnitude of the BF (Eq. 4). The magnitude of BF is only relevant to weight performance in the
           bottom left performance zone, where "on paper", a BMP should not experience bypass or overflow. The
           weight of the data points in the Relative to BF zone is equal to the average BF value for the individual storm
           events, ', tags$em('n'), ' within that zone, scaled up by a factor of 10:'),
                
                # Render the equation using HTML and perhaps MathJax (or Katex)
                tags$p(HTML("$$w_{BF} = 10 \\times \\frac{\\sum_{i=1}^{n} BF_i}{n} \\qquad \\text{Eq. 4}$$")), # MathJax version
                
                tags$p(HTML("The Success and Failure category weights for individual events are maintained within the $w$<sub>BF</sub> calculation
           (Eq. 4). If there is no measured bypass for a small storm, BF = 0 by Eq. 4 and $w$<sub>BF</sub> = 0, which is equivalent
           to the Success weight. If/where the entire flow volume is measured as bypass (i.e. no runoff gets into the
           BMP), BF = 1 and $w$<sub>BF</sub> = 10, which is equivalent to a Failure. The factor of 10 in Eq. 5 is a scaling factor to
           render the BF relevant to the Success, Excess, Check Data, and Failure category weights. For example, a BF
           of 3% is arguably a negligible proportion of flow. It would result in $w$<sub>BF</sub> = 0.3, which is only marginally worse
           performance than Success. However, repeatedly measuring BF > 50% and the resultant $w$<sub>BF</sub> = 5.0 would
           likely indicate that the BMP far from meeting performance expectations during small storms.")),
                
                HTML("<p>
              This weighting scheme (i.e., set of w<sub>i</sub> weighting factors in Eq. 3) reflects the underlying preference of
              resource allocation to BMPs presenting in each category (Figure H3). Outcomes of Success or Excess
              indicate that no action is needed; the BMP is managing runoff at least as well as intended by design. A
              high proportion of data in the Excess category should signal to technical personnel that the BMP may be
              overdesigned, as it is frequently outperforming expectations in large storm events. This could be due to
              the design approach itself, or errors made during design. It may also indicate that the drainage area serving
              the BMP is larger than anticipated during the design process, which can arise from a range of factors in
              the field.
           </p>"),
                tags$p("As previously stated, datapoints in the Check Data category can indicate issues with monitoring
        equipment, anomalies in the monitoring configuration or a significant change in catchment land use or
        increased drainage area extent sending more runoff to the BMP than it was designed to manage. Failing
        BMPs are not capturing enough runoff, and it is unlikely the receiving water protection goals will be met.
        Failure can arise from deficiencies of initial design or deviations from design “on paper” during the
        construction process, or because of maintenance condition. Small storms that don’t fill up a BMP yet
        experience bypass or overflow are considered Failures (this should not happen), but may indicate
        circumstances such as back-to-back storms or maintenance deficiency. Corrective action is needed for
        Failures."),
                tags$figure(
                  tags$img(src = "H4HydroManagementImplications.png", class = "centerImage", style = "height:400px; width:400px;"),
                  tags$figcaption("Figure H4. Management implications for single-category outcomes.")
                ),
                
                tags$p("Figure H5 illustrates the transition points and how the HPI contributes to decision making for stormwater managers. Additional lines of inquiry are included for technical designers and planners to further investigate the broad management action zones. The HPI score reflects the weighted average across all the BMP site-events available in the dataset. Low scores are associated with the best / most preferred outcomes. Where at least half of the data fall into a single category, the suggested management action reflects the implication of that category (Figure H4)."),
                tags$figure(
                  style = "text-align: center;",
                  tags$img(src = "HydroIndexInterpretation.jpg", style = "width:70%; height:70%;"),
                  tags$figcaption("Figure H5. Hydrology Index Management Implications.")
                ),
                
                
                ## Acknowledgement Section of Overview ----
                
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
              class = "contributors-container",
              tags$ul(
                class = "contributors-list",
                tags$li("Dr. Elizabeth Fassman-Beck"),
                tags$li("Ken Schiff"),
                tags$li("Dr. Edward Tiernan"),
                tags$li("Robert Butler"),
                tags$li("Duy Nguyen")
              ),
              tags$img(
                src = "sccwrp-logo.png",
                class = "contributors-logo"
              )
            )
          ),
          column(
            width = 6,
            h3("Los Angeles County Department of Public Works Project Manager"),
            div(
              class = "manager-container",
              tags$ul(
                class = "manager-list",
                tags$li("Frank Cheng")
              )
            ),
            h3("Advisory Group Members"),
            div(
              class = "advisory-container",
              tags$ul(
                class = "advisory-list",
                tags$li("Dr. Bridget Wadzuk, Villanova University"),
                tags$li("Annalisa Moe, Heal the Bay"),
                tags$li("Richard Boon, Riverside County Flood Control and Water Conservation District"),
                tags$li("Jane Clary, Wright Water Engineers"),
                tags$li("Chris Beegan, California State Water Resources Control Board"),
                tags$li("LB Nye, Los Angeles Regional Water Quality Control Board"),
                tags$li("Bhaskar Joshi, Caltrans")
              )
            )
          )
        )
      ) # end "pseudo-footer" box
      
    )
  ) # end main-welcome-div
)

# Contact tab content -------------------------------------------------------------------------------------------------------------------------------------------------
contact_tab <- tabItem(
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


# WQ Tab Instructions ------------------------------------------------------------------------------------------------------------------------------------------------
wq_instructions <- tags$ol(
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
)

# Hydrology Tab Instructions ------------------------------------------------------------------------------------------------------------------------
hydro_instructions <- tagList(
  tags$ol(
    tags$li("Download csv template."),
    
    tags$li(
      "Populate template with BMP monitoring data.",
      tags$ol(
        type = "a",
        tags$li("Data must be runoff volumes from paired influent-effluent sampling. BMPs and/or specific events with full-capture (i.e., no effluent discharged from the BMP) are treated as effluent volume equals 0, i.e., influent volumes from full-capture events should be paired with an effluent volume of 0."),
        tags$li("Report bypass or overflow volumes. For the purposes of this index and calculator, bypass and overflow are considered similarly and are labeled as bypass for convenience. Bypass volumes may be entered for full-capture events."),
        tags$li(HTML("All volumes must have consistent units (e.g., L, m<sup>3</sup>, gal, or ft<sup>3</sup>).")),
        tags$li("Total precipitation depth must be reported for every monitored event (influent-effluent volumes)."),
        tags$li("All precipitation depths must have consistent units (e.g., in, mm, or cm)."),
        tags$li("Data limit is 5Mb.")
      )
    ),
    
    tags$li("Upload data template to generate the Hydrology Performance Index Plot, Score, and Summary Table."),
    
    tags$li(
      "Enter the design storm depth used to design the monitored BMP. This value should be obtained from design reports or other documentation. The design storm depth is a single value for a given BMP. It is not a subjective variable.",
      tags$ol(
        type = "a",
        tags$li("Units must be consistent with the monitored precipitation.")
      )
    ),
    
    tags$li(
      "Enter the design capture volume anticipated from the design of the monitored BMP. This value should be obtained from design reports or other documentation. The design capture volume is a single volume for a given BMP. It is not a subjective variable.",
      tags$ol(
        type = "a",
        tags$li("Units must be consistent with the monitored runoff volumes.")
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
        tags$li("Download Performance Index Plot, Score, and Summary Table.")
      )
    )
    ,br()
  )
)



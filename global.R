# global.R

# Load libraries ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(glue)
library(stringr)

# Source helper functions and modules ----
source("R/utils.R")
source("R/gauge.R")
source("R/wq_module.R")
source("R/hydro_module.R")

# Global UI settings ----
global_font_family <- "Arial"
global_point_size <- 4
UNCERTAINTY_BUFFER <- 0.2

# Plot color and shape settings ----
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

hydro_colors <- c(
  "Success" = "#117733",
  "Excess" = "#0072B2",
  "Neutral" = "black",
  "Failure" = "#661100",
  "Small Storm With Bypass" = "purple"
)

designplot_colors <- c(
  "Success" = "#117733",
  "Excess" = "#0072B2",
  "Check Data" = "#ff6f00",
  "Failure" = "#661100",
  "Small Storm With Bypass" = "purple"
)

bypass_shapes <- c(
  "Bypass" = 1,
  "No Bypass" = 16 
)


# CSS ----
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


# Welcome tab content ----
welcome_tab <- tabItem(
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
              #### main overview text ----
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
            
            #### Header for Project Manager ----
            h3("Los Angeles County Department of Public Works Project Manager"),
            
            #### List for Project Manager ----
            div(
              style = "display: flex; flex-direction: column; align-items: flex-start;",
              tags$ul(
                style = "list-style-position: inside; text-align: left; padding: 0;",
                tags$li("Frank Cheng")
              )
            ),
            
            #### Header for Advisory Group Members ----
            h3("Advisory Group Members"),
            
            #### List for Advisory Group Members ----
            div(
              style = "display: flex; flex-direction: column; align-items: flex-start;",
              tags$ul(
                style = "list-style-position: inside; text-align: left; padding: 0;",
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
      )
    )
  )
)

# Contact tab content ----
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


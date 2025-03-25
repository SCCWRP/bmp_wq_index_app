# rounding function to override R's default round-to-even behavior
# round up for >= 5, round down for < 5
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

# Get the score for the gauge
get.composite.score <- function(df, performance_col = 'Performance') {
  tmp <- df %>%
    dplyr::group_by(!!sym(performance_col)) %>%
    dplyr::summarize(
      proportion = n() / nrow(.)
    ) %>%
    mutate(
      category_score = dplyr::case_when(
        !!sym(performance_col) == 'Success'      ~ 0,
        !!sym(performance_col) == 'Excess'       ~ 1,
        !!sym(performance_col) == 'Marginal'     ~ 3,
        !!sym(performance_col) == 'Insufficient' ~ 4,
        !!sym(performance_col) == 'Failure'      ~ 10
      )
    )
  
  
  # Dot product of the category scores with their respective proportions of the data
  signif(tmp$category_score %*% tmp$proportion, 3)
}




# function to generate the gauge/dial plot using plotly
get.composite.gauge <- function(composite_score) {

  
  title_text <- glue::glue("Performance Index Score")
  top_offset <- 28
  plotly::plot_ly(
    height = 300,
    type = "indicator",
    mode = "gauge+number",
    value = composite_score,
    number = list(
      font = list(
        family = "Arial"
      ),
      valueformat = ".2f"
    ),
    # title = list(
    #   text = title_text,
    #   font = list(
    #     size = 23,
    #     color = "#202020",
    #     family = "Arial"
    #   )
    # ),
    gauge = list(
      borderwidth = 0,
      axis = list(
        range = list(0.0, 10), 
        ticks = '',
        tickwidth = 0.1, 
        tickcolor = "darkblue",
        tickvals=c(0.0, 1.0, 2.0, 3.5, 5.0, 7.5, 10.0),
        ticktext=c("0.0", "No action<br>needed", "2.0", "Query Data<br>3.5", "5.0", "Intervention<br>needed   ", "10.0"),
        tickangle=0,
        tickfont=list(size=16, family = "Arial")
      ),
      bar = list(color = "darkblue"),
      steps = list(
        list(range = c(5, 10), color = "red"),
        list(range = c(3.5, 5), color = "orange"),
        list(range = c(2, 3.5), color = "yellow"),
        list(range = c(0.00, 2), color = "green")
      )
    )
  ) %>%
    plotly::layout(
      margin=list(l=80, r=100, t=top_offset, b=0)
    ) %>% 
    plotly::config(
      displayModeBar = FALSE
    )
}


summary.table <- function(df, threshold, performance_col = 'Performance') {
  perf.index <- get.composite.score(df, performance_col = performance_col)
  
  n.success <- df %>% filter(!!sym(performance_col) == 'Success') %>% nrow()
  n.excess <- df %>% filter(!!sym(performance_col) == 'Excess') %>% nrow()
  n.marginal <- df %>% filter(!!sym(performance_col) == 'Marginal') %>% nrow()
  n.insufficient <- df %>% filter(!!sym(performance_col) == 'Insufficient') %>% nrow()
  n.failure <- df %>% filter(!!sym(performance_col) == 'Failure') %>% nrow()
  
  
  data.frame(
    Threshold = threshold, 
    `Performance Index` = perf.index, 
    num_success = paste0(n.success, ' (', round(n.success/nrow(df), 1), '%)'),
    num_excess = paste0(n.excess, ' (', round(n.excess/nrow(df), 1), '%)'),
    num_marginal = paste0(n.marginal, ' (', round(n.marginal/nrow(df), 1), '%)'),
    num_insufficient = paste0(n.insufficient, ' (', round(n.insufficient/nrow(df), 1), '%)'),
    num_failure = paste0(n.failure, ' (', round(n.failure/nrow(df), 1), '%)')
  )
  
}




# Composite gauge function ----
# generating the gauge/dial widget using plotly
get.composite.hydro.gauge <- function(df) {
  
  tmp <- df %>%
    mutate(
      category_score = case_when(
        Performance == "Success"    ~  0,
        Performance == "Excess"     ~  1,
        Performance == "Check Data" ~  4,
        Performance == "Failure"    ~ 10,
        
        # 10 is the weight, for the bypass fraction sliding index, we would multiply the weight with the bypass fraction average
        Performance == "Small Storm With Bypass" ~ 10 * bypass_fraction_average ,
        .default =  4
      ),
      proportion = `%`/100
    ) %>%
    select(-bypass_fraction_average)
  
  # dot product of the category scores with their respective proportions of the data
  composite_score <- signif(tmp$category_score %*% tmp$proportion, 3)
  
  top_offset <- 28
  title_text <- "Performance Index"
  
  # plotly widget object with formatting
  plotly::plot_ly(
    height = 300,
    type = "indicator",
    mode = "gauge+number",
    value = composite_score,
    number = list(
      font = list(
        family = "Arial"
      ),
      valueformat = ".2f"
    ),
    title = list(
      text = title_text, 
      font = list(
        size = 23,
        color = "#202020",
        family = "Arial"
      )
    ),
    # dial visualization
    gauge = list(
      borderwidth = 0,
      axis = list(
        range = list(0.0, 10), 
        ticks = '',
        tickwidth = 0.1, 
        tickcolor = "darkblue",
        tickvals=c(0.0, 0.5, 1.0, 3.0, 5.0, 7.5, 10.0),
        ticktext=c("0.0", 'No action   <br>needed   ', "1.0", "Query Data<br>3.0", "5.0", "Intervention<br>needed   ", "10.0"),
        tickangle=0,
        tickfont=list(size=16, family = "Arial")
      ),
      bar = list(color = "darkblue"),
      steps = list(
        list(range = c(5, 10), color = "red"),
        list(range = c(3.0, 5), color = "orange"),
        list(range = c(1, 3.0), color = "yellow"),
        list(range = c(0.00, 1), color = "green")
      )
    )
  ) %>%
    plotly::layout(
      margin = list(l = 85, r = 100, t = 80, b = 40)
    ) %>% 
    plotly::config(
      displayModeBar = FALSE
    )
}

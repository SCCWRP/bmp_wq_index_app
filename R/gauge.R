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
  
  
  print('score df')
  print(tmp)
  # Dot product of the category scores with their respective proportions of the data
  signif(tmp$category_score %*% tmp$proportion, 3)
}




# function to generate the gauge/dial plot using plotly
get.composite.gauge <- function(composite_score) {

  
  title_text <- glue::glue("Performance Index")
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
  
  data.frame(Threshold = threshold, `Performance Index` = perf.index, `# Success` = paste0(n.success, '(', round(n.success/nrow(df), 1), '%)') )
  
}

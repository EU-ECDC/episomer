  # Defining line chart from shiny app filters
  line_chart_from_filters <- function(topics, countries, period_type, period, with_retweets, location_type, alpha, alpha_outlier, k_decay, no_history, bonferroni_correction, same_weekday_baseline) {
    trend_line(
      topic = topics
      ,countries= if(length(countries) == 0) c(1) else as.integer(countries)
      ,date_type= period_type
      ,date_min = period[[1]]
      ,date_max = period[[2]]
      ,with_retweets= with_retweets
      # ,location_type = location_type
      ,alpha = alpha
      ,alpha_outlier = alpha_outlier
      ,k_decay = k_decay
      ,no_historic = no_history
      ,bonferroni_correction = bonferroni_correction
      ,same_weekday_baseline = same_weekday_baseline
    )
    
  }
  # Defining line chart from shiny app filters
  map_chart_from_filters <- function(topics, countries, period, with_retweets, location_type) {
    create_map(
      topic= topics
      ,countries= if(length(countries) == 0) c(1) else as.integer(countries)
      ,date_min = period[[1]]
      ,date_max = period[[2]]
      ,with_retweets= with_retweets
      ,location_type = location_type
      ,caption = conf$regions_disclaimer
      ,forplotly = TRUE 
    )
    
  }
  # Defining top words chart from shiny app filters
  top_chart_from_filters <- function(topics, serie, fcountries, period, with_retweets, location_type, top) {
    fcountries= if(length(fcountries) == 0 || 1 %in%fcountries) c(1) else as.integer(fcountries)
    regions <- get_country_items()
    countries <- Reduce(function(l1, l2) {unique(c(l1, l2))}, lapply(fcountries, function(i) unlist(regions[[i]]$codes)))
    create_topchart(
      topic= topics
      ,serie = serie     
      ,country_codes = countries
      ,date_min = period[[1]]
      ,date_max = period[[2]]
      ,with_retweets= with_retweets
      # ,location_type = location_type
      ,top
    )
    
  }
# Defining line chart from shiny app filters
line_chart_from_filters <- function(
  sms,
  topics,
  countries,
  period_type,
  period,
  with_quotes,
  alpha,
  alpha_outlier,
  k_decay,
  no_history,
  bonferroni_correction,
  same_weekday_baseline
) {
  trend_line(
    sms = sms,
    topic = topics,
    countries = if (length(countries) == 0) c(1) else as.integer(countries),
    date_type = period_type,
    date_min = period[[1]],
    date_max = period[[2]],
    with_quotes = with_quotes,
    alpha = alpha,
    alpha_outlier = alpha_outlier,
    k_decay = k_decay,
    no_historic = no_history,
    bonferroni_correction = bonferroni_correction,
    same_weekday_baseline = same_weekday_baseline
  )
}
# Defining line chart from shiny app filters
map_chart_from_filters <- function(
  sms,
  topics,
  countries,
  period,
  with_quotes
) {
  create_map(
    sms = sms,
    topic = topics,
    countries = if (length(countries) == 0) c(1) else as.integer(countries),
    date_min = period[[1]],
    date_max = period[[2]],
    with_quotes = with_quotes,
    caption = conf$regions_disclaimer,
    forplotly = TRUE
  )
}
# Defining top words chart from shiny app filters
top_chart_from_filters <- function(
  sms,
  topics,
  serie,
  fcountries,
  period,
  with_quotes,
  top
) {
  fcountries <- if (length(fcountries) == 0 || 1 %in% fcountries) {
    c(1)
  } else {
    as.integer(fcountries)
  }
  regions <- get_country_items()
  countries <- Reduce(
    function(l1, l2) {
      unique(c(l1, l2))
    },
    lapply(fcountries, function(i) unlist(regions[[i]]$codes))
  )
  create_topchart(
    sms = sms,
    topic = topics,
    serie = serie,
    country_codes = countries,
    date_min = period[[1]],
    date_max = period[[2]],
    with_quotes = with_quotes,
    top
  )
}

# Rmarkdown dasboard export bi writing the dashboard on the provided file
# it uses the markdown template inst/rmarkdown/dashboard.Rmd
# pdf_document writes the PDF on file, md_document writes a zip archive containing the md and its images
# Progress and error notification are handled by the caller (shiny download handler)
export_dashboard <- function(
  format,
  file,
  sms,
  topics,
  countries,
  period_type,
  period,
  with_quotes,
  location_type,
  alpha,
  alpha_outlier,
  k_decay,
  no_historic,
  bonferroni_correction,
  same_weekday_baseline,
  top_type1,
  top_type2
) {
  # rendering a copy of the template in a dedicated temp dir: intermediate files are not written into
  # the package install dir and md image links stay relative to the md file
  work_dir <- tempfile("episomer_dashboard_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  rmd <- file.path(work_dir, "episomer_dashboard.Rmd")
  file.copy(
    system.file("rmarkdown", "dashboard.Rmd", package = get_package_name()),
    rmd
  )
  output <- rmarkdown::render(
    rmd,
    output_format = format,
    params = list(
      "sms" = sms,
      "topics" = topics,
      "countries" = countries,
      "period_type" = period_type,
      "period" = period,
      "with_quotes" = with_quotes,
      "location_type" = location_type,
      "alert_alpha" = alpha,
      "alert_alpha_outlier" = alpha_outlier,
      "alert_k_decay" = k_decay,
      "alert_historic" = no_historic,
      "bonferroni_correction" = bonferroni_correction,
      "same_weekday_baseline" = same_weekday_baseline,
      "top_type1" = top_type1,
      "top_type2" = top_type2
    ),
    envir = new.env(parent = asNamespace(get_package_name())),
    quiet = TRUE
  )
  unlink(rmd)
  if (format == "md_document") {
    # md images are written in a side folder, shipping both in a zip
    zip::zip(file, files = list.files(work_dir), root = work_dir)
  } else {
    file.copy(output, file, overwrite = TRUE)
  }
  invisible(file)
}

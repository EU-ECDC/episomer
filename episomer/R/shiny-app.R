#' @title Run the episomer Shiny app
#' @description Open the episomer Shiny app, used to setup the Data collection & processing pipeline, the Requirements & alerts pipeline and to visualise the outputs.
#' @param data_dir Path to the 'data directory' containing application settings, models and collected posts.
#' If not provided the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, default: NA
#' @param host The host to run the Shiny app on, default: NULL (will run on 127.0.0.1)
#' @param port The port to run the Shiny app on, default: NULL (will run on a random port)
#' @param profile The profile to run the Shiny app on, default: "dashboard" (can be "dashboard" or "admin")
#' @return The Shiny server object containing the launched application
#' @details The episomer app is the user entry point to the episomer package. This application will help the user to setup the post collection process, manage all settings,
#' see the interactive dashboard visualisations, export them to Markdown or PDF, and setup the alert emails.
#'
#' All its functionality is described on the episomer vignette.
#' @examples
#' if(FALSE){
#'    #Running the episomer app
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    episomer_app()
#' }
#' @seealso
#'  \code{\link{search_loop}}
#'
#'  \code{\link{detect_loop}}
#' @rdname episomer_app
#' @export
#' @importFrom shiny fluidPage fluidRow column selectInput h4 conditionalPanel dateRangeInput radioButtons checkboxInput sliderInput numericInput downloadButton h3 htmlOutput actionButton span textInput textAreaInput h2 passwordInput h5 fileInput uiOutput navbarPage tabPanel observe updateSliderInput updateDateRangeInput downloadHandler invalidateLater renderText observeEvent renderUI validate need shinyApp
#' @importFrom plotly plotlyOutput renderPlotly ggplotly config layout
#' @importFrom DT dataTableOutput renderDataTable datatable formatPercentage replaceData dataTableProxy
#' @importFrom rmarkdown render
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_replace_all
#' @importFrom grDevices png
#' @importFrom ggplot2 ggsave
#' @importFrom dplyr select
#' @importFrom stats setNames
#' @importFrom utils write.csv head
episomer_app <- function(data_dir = NA, profile = c("dashboard", "admin"), host = NULL, port = NULL) {
  old <- options()
  on.exit(options(old))
  options(shiny.host = host, shiny.port = port)
  match.arg(profile, c("dashboard", "admin"))
  # Setting up configuration if not already done
  if (is.na(data_dir)) setup_config_if_not_already() else {
    setup_config(data_dir)
  }
  if (!file.exists(get_properties_path())) {
    save_config(data_dir = conf$data_dir, properties = TRUE, sm_topics = list())
    save_tasks(get_tasks())
  }
  # Loading data for dashboard and configuration
  d <- refresh_dashboard_data()
  if("admin" %in% profile) {
      cd <- refresh_config_data()
  }

  if("dashboard" %in% profile) {
      # Defining dashboard page UI
      ################################################
      ######### DASHBOARD PAGE #######################
      ################################################
      dashboard_page <-
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              3,
              ################################################
              ######### DASHBOARD FILTERS #####################
              ################################################
              shiny::actionButton("run_dashboard", "Run"),
              shiny::selectInput(
                "sms",
                label = shiny::h4("Social Media"),
                multiple = TRUE,
                choices = d$sms,
                selected = d$sms
              ),
              shiny::selectInput(
                "topics",
                label = shiny::h4("Topics"),
                multiple = FALSE,
                choices = d$topics
              ),
              shiny::selectInput(
                "countries",
                label = shiny::h4("Countries & regions"),
                multiple = TRUE,
                choices = d$countries
              ),
              shiny::selectInput(
                "fixed_period",
                label = shiny::h4("Period"),
                multiple = FALSE,
                choices = list(
                  "Last 7 days" = "last 7 days",
                  "Last 30 days" = "last 30 days",
                  "Last 60 days" = "last 60 days",
                  "Last 180 days" = "last 180 days",
                  "custom"
                ),
                selected = d$fixed_period
              ),
              shiny::conditionalPanel(
                condition = "input.fixed_period == 'custom'",
                shiny::dateRangeInput(
                  "period",
                  label = shiny::h4("Dates"),
                  start = d$date_start,
                  end = d$date_end,
                  min = d$date_min,
                  max = d$date_max,
                  format = "yyyy-mm-dd",
                  startview = "month"
                )
              ),
              shiny::radioButtons(
                "period_type",
                label = shiny::h4("Time unit"),
                choices = list(
                  "Days" = "created_date",
                  "Weeks" = "created_weeknum"
                ),
                selected = "created_date",
                inline = TRUE
              ),
              shiny::h4("Include quotes"),
              shiny::checkboxInput(
                "with_quotes",
                label = NULL,
                value = conf$alert_with_quotes
              ),
              shiny::sliderInput(
                "alpha_filter",
                label = shiny::h4("Signal false positive rate"),
                min = 0,
                max = 0.3,
                value = conf$alert_alpha,
                step = 0.005
              ),
              shiny::sliderInput(
                "alpha_outlier_filter",
                label = shiny::h4("Outlier false positive rate"),
                min = 0,
                max = 0.3,
                value = conf$alert_alpha_outlier,
                step = 0.005
              ),
              shiny::sliderInput(
                "k_decay_filter",
                label = shiny::h4("Outlier downweight strength"),
                min = 0,
                max = 10,
                value = conf$alert_k_decay,
                step = 0.5
              ),
              shiny::h4("Bonferroni correction"),
              shiny::checkboxInput(
                "bonferroni_correction",
                label = NULL,
                value = conf$alert_with_bonferroni_correction
              ),
              shiny::numericInput(
                "history_filter",
                label = shiny::h4("Days in baseline"),
                value = conf$alert_history
              ),
              shiny::h4("Same weekday baseline"),
              shiny::checkboxInput(
                "same_weekday_baseline",
                label = NULL,
                value = conf$alert_same_weekday_baseline
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::downloadButton("export_pdf", "PDF")),
                shiny::column(4, shiny::downloadButton("export_md", "Md"))
              )
            ),
            shiny::column(
              9,
              ################################################
              ######### DASHBOARD PLOTS #######################
              ################################################
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::fluidRow(
                    shiny::column(
                      2,
                      shiny::downloadButton("download_line_data", "Data")
                    ),
                    shiny::column(2, shiny::downloadButton("export_line", "image"))
                  ),
                  plotly::plotlyOutput("line_chart")
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::fluidRow(
                    shiny::column(
                      3,
                      shiny::downloadButton("download_map_data", "Data")
                    ),
                    shiny::column(3, shiny::downloadButton("export_map", "image"))
                  ),
                  plotly::plotlyOutput(
                    "map_chart"
                  )
                ),
                shiny::column(
                  6,
                  shiny::fluidRow(
                    shiny::column(
                      3,
                      shiny::downloadButton("download_top1_data", "Data")
                    ),
                    shiny::column(3, shiny::downloadButton("export_top1", "image")),
                    shiny::column(6,"")
                  ),
                  plotly::plotlyOutput("top_chart1")
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::fluidRow(
                    shiny::column(
                      3,
                      shiny::downloadButton("download_top2_data", "Data")
                    ),
                    shiny::column(3, shiny::downloadButton("export_top2", "image")),
                    shiny::column(6, "")
                  ),
                  plotly::plotlyOutput("top_chart2")
                ),
                shiny::column(
                  6,
                  shiny::fluidRow(
                    shiny::column(
                      3,
                      shiny::downloadButton("download_top3_data", "Data")
                    ),
                  ),
                  shiny::fluidRow(
                    shiny::column(12, shiny::htmlOutput("top_table_title"))
                  ),
                  shiny::fluidRow(
                    shiny::column(12, DT::dataTableOutput("top_table"))
                  ),
                  shiny::fluidRow(
                    shiny::column(12, shiny::htmlOutput("top_table_disc"))
                  )
                )
              )
            )
          )
        )
  }

  if("admin" %in% profile) {
      # Defining alerts page UI
      ################################################
      ######### ALERTS PAGE ##########################
      ################################################
      alerts_page <-
        shiny::fluidPage(
          shiny::h3("Find alerts"),
          ################################################
          ######### ALERTS FILTERS #######################
          ################################################
          shiny::fluidRow(
            shiny::column(3, shiny::h4("Detection date")),
            shiny::column(2, shiny::h4("Topics")),
            shiny::column(2, shiny::h4("Countries & regions")),
            shiny::column(2, shiny::h4("Display")),
            shiny::column(1, shiny::h4("Limit")),
          ),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::dateRangeInput(
                "alerts_period",
                label = "",
                start = d$date_end,
                end = d$date_end,
                min = d$date_min,
                max = d$date_max,
                format = "yyyy-mm-dd",
                startview = "month"
              )
            ),
            shiny::column(
              2,
              shiny::selectInput(
                "alerts_topics",
                label = NULL,
                multiple = TRUE,
                choices = d$topics[d$topics != ""]
              )
            ),
            shiny::column(
              2,
              shiny::selectInput(
                "alerts_countries",
                label = NULL,
                multiple = TRUE,
                choices = d$countries
              )
            ),
            shiny::column(
              2,
              shiny::radioButtons(
                "alerts_display",
                label = NULL,
                choices = list("Posts" = "posts", "Parameters" = "parameters"),
                selected = "parameters",
                inline = TRUE
              ),
            ),
            shiny::column(
              1,
              shiny::selectInput(
                "alerts_limit",
                label = NULL,
                multiple = FALSE,
                choices = list(
                  "None" = "0",
                  "10" = "10",
                  "50" = "50",
                  "100" = "100",
                  "500" = "500"
                )
              )
            ),
          ),
          shiny::fluidRow(
            ################################################
            ######### ALERTS FILTERS #######################
            ################################################
            shiny::column(
              2,
              shiny::actionButton("alerts_search", "Search alerts"),
            ),
            shiny::column(
              2,
              shiny::actionButton("alerts_close", "Hide search"),
              shiny::conditionalPanel(
                condition = "false",
                shiny::textInput(
                  "alerts_show_search",
                  value = "false",
                  label = NULL
                )
              )
            ),
            shiny::column(
              2,
              shiny::actionButton("alertsdb_add", "Add alerts to annotations")
            ),
            shiny::column(6)
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              ################################################
              ######### ALERTS TABLE #########################
              ################################################
              shiny::conditionalPanel(
                condition = "input.alerts_show_search == 'true'",
                DT::dataTableOutput("alerts_table"),
              )
            )
          ),
          shiny::h3("Alerts annotations"),
          shiny::fluidRow(
            shiny::column(
              2,
              shiny::actionButton("alertsdb_search", "Show annotations")
            ),
            shiny::column(
              2,
              shiny::actionButton("alertsdb_close", "Hide annotations")
            ),
            shiny::column(
              2,
              shiny::downloadButton("alertsdb_download", "Download annotations")
            ),
            shiny::column(
              4,
              shiny::fileInput(
                "alertsdb_upload",
                label = NULL,
                buttonLabel = "Upload & evaluate annotations", accept = ".xlsx"
              )
            ),
            shiny::column(
              2,
              shiny::conditionalPanel(
                condition = "false",
                shiny::textInput("alertsdb_show", value = "false", label = NULL)
              )
            )
          ),
          shiny::fluidRow(
            shiny::conditionalPanel(
              condition = "input.alertsdb_show == 'true'",
              shiny::column(
                12,
                ################################################
                ######### ALERT ANNOTATIONS EVALUATION #########
                ################################################
                shiny::h4(
                  "Performance evaluation of alert classification algorithm"
                ),
                DT::dataTableOutput("alertsdb_runs_table"),
                ################################################
                ######### ANNOTATED ALERTS #####################
                ################################################
                shiny::h4(
                  "Database used for training the alert classification algorithm"
                ),
                DT::dataTableOutput("alertsdb_table")
              )
            )
          )
        )
      # Defining configuration
      ################################################
      ######### CONFIGURATION PAGE####################
      ################################################
      config_page <-
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              4,
              ################################################
              ######### STATUS PANEL #########################
              ################################################
              shiny::h3("Status"),
              shiny::fluidRow(
                shiny::column(3, "episomer database"),
                shiny::column(5, shiny::htmlOutput("fs_running")),
                shiny::column(2, shiny::actionButton("activate_fs", "activate")),
                shiny::column(2, shiny::actionButton("stop_fs", "stop"))
              ),
              shiny::fluidRow(
                shiny::column(3, "Data collection & processing"),
                shiny::column(5, shiny::htmlOutput("search_running")),
                shiny::column(
                  2,
                  shiny::actionButton("activate_search", "activate")
                ),
                shiny::column(2, shiny::actionButton("stop_search", "stop"))
              ),
              shiny::fluidRow(
                shiny::column(3, "Requirements & alerts"),
                shiny::column(5, shiny::htmlOutput("detect_running")),
                shiny::column(
                  2,
                  shiny::actionButton("activate_detect", "activate")
                ),
                shiny::column(2, shiny::actionButton("stop_detect", "stop"))
              ),
              ################################################
              ######### GENERAL PROPERTIES ###################
              ################################################
              shiny::h3("Signal detection"),
              shiny::fluidRow(
                shiny::column(3, "Signal false positive rate"),
                shiny::column(
                  9,
                  shiny::sliderInput(
                    "conf_alpha",
                    label = NULL,
                    min = 0,
                    max = 0.3,
                    value = conf$alert_alpha,
                    step = 0.005
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Outlier false positive rate"),
                shiny::column(
                  9,
                  shiny::sliderInput(
                    "conf_alpha_outlier",
                    label = NULL,
                    min = 0,
                    max = 0.3,
                    value = conf$alert_alpha_outlier,
                    step = 0.005
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Outlier downweight strength"),
                shiny::column(
                  9,
                  shiny::sliderInput(
                    "conf_k_decay",
                    label = NULL,
                    min = 0,
                    max = 10,
                    value = conf$alert_k_decay,
                    step = 0.5
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Days in baseline"),
                shiny::column(
                  9,
                  shiny::numericInput(
                    "conf_history",
                    label = NULL,
                    value = conf$alert_history
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Same weekday baseline"),
                shiny::column(
                  9,
                  shiny::checkboxInput(
                    "conf_same_weekday_baseline",
                    label = NULL,
                    value = conf$alert_same_weekday_baseline
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Include quotes"),
                shiny::column(
                  9,
                  shiny::checkboxInput(
                    "conf_with_quotes",
                    label = NULL,
                    value = conf$alert_with_quotes
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Bonferroni correction"),
                shiny::column(
                  9,
                  shiny::checkboxInput(
                    "conf_with_bonferroni_correction",
                    label = NULL,
                    value = conf$alert_with_bonferroni_correction
                  )
                )
              ),
              shiny::h3("General"),
              shiny::fluidRow(
                shiny::column(3, "Data dir"),
                shiny::column(9, shiny::span(conf$data_dir))
              ),
              shiny::fluidRow(
                shiny::column(3, "Data collection & processing span (min)"),
                shiny::column(
                  9,
                  shiny::numericInput(
                    "conf_collect_span",
                    label = NULL,
                    value = conf$collect_span
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Requirements & alerts span (min)"),
                shiny::column(
                  9,
                  shiny::numericInput(
                    "conf_schedule_span",
                    label = NULL,
                    value = conf$schedule_span
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Launch slots"),
                shiny::column(9, shiny::htmlOutput("conf_schedule_slots"))
              ),
              shiny::fluidRow(
                shiny::column(3, "Password store"),
                shiny::column(
                  9,
                  shiny::selectInput(
                    "conf_keyring",
                    label = NULL,
                    choices = c(
                      "wincred",
                      "macos",
                      "file",
                      "secret_service",
                      "environment"
                    ),
                    selected = conf$keyring
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Spark cores"),
                shiny::column(
                  9,
                  shiny::numericInput(
                    "conf_spark_cores",
                    label = NULL,
                    value = conf$spark_cores
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Spark memory"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "conf_spark_memory",
                    label = NULL,
                    value = conf$spark_memory
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Geolocation threshold"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "geolocation_threshold",
                    label = NULL,
                    value = conf$geolocation_threshold
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "GeoNames URL"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "conf_geonames_url",
                    label = NULL,
                    value = conf$geonames_url
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Simplified GeoNames"),
                shiny::column(
                  9,
                  shiny::checkboxInput(
                    "conf_geonames_simplify",
                    label = NULL,
                    value = conf$geonames_simplify
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Stream database results"),
                shiny::column(
                  9,
                  shiny::checkboxInput(
                    "conf_onthefly_api",
                    label = NULL,
                    value = conf$onthefly_api
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Maven repository"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "conf_maven_repo",
                    label = NULL,
                    value = conf$maven_repo
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = "false",
                shiny::textInput("os_type", value = .Platform$OS.type, label = NULL)
              ),
              shiny::conditionalPanel(
                condition = "input.os_type == 'windows'",
                shiny::fluidRow(
                  shiny::column(3, "Winutils URL"),
                  shiny::column(
                    9,
                    shiny::textInput(
                      "conf_winutils_url",
                      label = NULL,
                      value = conf$winutils_url
                    )
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Region disclaimer"),
                shiny::column(
                  9,
                  shiny::textAreaInput(
                    "conf_regions_disclaimer",
                    label = NULL,
                    value = conf$regions_disclaimer
                  )
                )
              ),
              shiny::h2("Email authentication (SMTP)"),
              shiny::fluidRow(
                shiny::column(3, "Server"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "smtp_host",
                    label = NULL,
                    value = conf$smtp_host
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Port"),
                shiny::column(
                  9,
                  shiny::numericInput(
                    "smtp_port",
                    label = NULL,
                    value = conf$smtp_port
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "From"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "smtp_from",
                    label = NULL,
                    value = conf$smtp_from
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Login"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "smtp_login",
                    label = NULL,
                    value = conf$smtp_login
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Password"),
                shiny::column(
                  9,
                  shiny::passwordInput(
                    "smtp_password",
                    label = NULL,
                    value = conf$smtp_password
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Unsafe certificates"),
                shiny::column(
                  9,
                  shiny::checkboxInput(
                    "smtp_insecure",
                    label = NULL,
                    value = conf$smtp_insecure
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(3, "Admin email"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "admin_email",
                    label = NULL,
                    value = conf$admin_email
                  )
                )
              ),
              shiny::h2("Task registering"),
              shiny::fluidRow(
                shiny::column(3, "Custom date format"),
                shiny::column(
                  9,
                  shiny::textInput(
                    "force_date_format",
                    label = NULL,
                    value = conf$force_date_format
                  )
                )
              )
            ),
            shiny::column(
              8,
              ################################################
              ######### Requirements & alerts PANEL ######################
              ################################################
              shiny::h3("Requirements & alerts pipeline"),
              shiny::h5("Manual tasks"),
              shiny::fluidRow(
                shiny::column(
                  3,
                  shiny::actionButton("update_dependencies", "Run dependencies")
                ),
                shiny::column(
                  3,
                  shiny::actionButton("update_geonames", "Run GeoNames")
                ),
                shiny::column(
                  3,
                  shiny::actionButton("update_languages", "Run languages")
                ),
                shiny::column(
                  3,
                  shiny::actionButton("request_alerts", "Run alerts")                  
                )
              ),
              DT::dataTableOutput("tasks_df"),
              ################################################
              ######### SOCIAL MEDIA PANDEL###################
              ################################################
              shiny::h2("Social medias"),
              shiny::fluidRow(
                shiny::column(4, "Social Media"),
                shiny::column(8, "Configuration"),
                style = "font-weight:bold"
              ),
              shiny::fluidRow(
                shiny::column(
                  4,
                  shiny::checkboxInput(
                    "conf_sm_activated_bluesky",
                    label = "Bluesky",
                    value = conf$sm_activated_bluesky
                  )
                ),
                shiny::column(
                  8,
                  shiny::conditionalPanel(
                    condition = "input.conf_sm_activated_bluesky == true",
                    shiny::checkboxInput(
                      "conf_sm_alerts_bluesky",
                      label = "Calculate alerts",
                      value = conf$sm_alerts_bluesky
                    ),
                    shiny::fluidRow(
                      shiny::column(3, "User"),
                      shiny::column(
                        9,
                        shiny::textInput(
                          "bluesky_user",
                          label = NULL,
                          value = if (is_secret_set("bluesky_user"))
                            get_secret("bluesky_user") else NULL
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(3, "Password"),
                      shiny::column(
                        9,
                        shiny::passwordInput(
                          "bluesky_password",
                          label = NULL,
                          value = if (is_secret_set("bluesky_password"))
                            get_secret("bluesky_password") else NULL
                        )
                      )
                    )
                  )
                ),
                style = "background-color:#f9f9f9;border-top: 1px solid #b2b2b2;border-bottom: 1px solid #d9d9d9;"
              ),
              ################################################
              ######### TOPICS PANEL ######################
              ################################################
              shiny::h3("Topics"),
              shiny::fluidRow(
                shiny::column(3, shiny::h5("Available topics")),
                shiny::column(
                  2,
                  shiny::downloadButton("conf_topics_download", "Download")
                ),
                shiny::column(
                  2,
                  shiny::downloadButton(
                    "conf_orig_topics_download",
                    "Download default"
                  )
                ),
                shiny::column(
                  3,
                  shiny::fileInput(
                    "conf_topics_upload",
                    label = NULL,
                    buttonLabel = "Upload", accept = ".xlsx"
                  )
                ),
                shiny::column(
                  2,
                  shiny::actionButton(
                    "conf_dismiss_past_posts",
                    "Dismiss past posts"
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::h5("Limit topic history"))
              ),
              DT::dataTableOutput("config_topics"),
              ################################################
              ######### LANGUAGES PANEL ######################
              ################################################
              shiny::h3("Languages"),
              shiny::fluidRow(
                shiny::column(4, shiny::h5("Available languages")),
                shiny::column(
                  2,
                  shiny::downloadButton("conf_lang_download", "Download")
                ),
                shiny::column(
                  2,
                  shiny::downloadButton(
                    "conf_orig_lang_download",
                    "Download default"
                  )
                ),
                shiny::column(
                  4,
                  shiny::fileInput(
                    "conf_lang_upload",
                    label = NULL,
                    buttonLabel = "Upload", accept = ".xlsx"
                  )
                ),
              ),
              shiny::fluidRow(
                shiny::column(4, shiny::h5("Active languages")),
                shiny::column(6, shiny::uiOutput("lang_items_0")),
                shiny::column(1, shiny::actionButton("conf_lang_add", "+")),
                shiny::column(1, shiny::actionButton("conf_lang_remove", "-")),
              ),
              DT::dataTableOutput("config_langs"),
              ################################################
              ######### IMPORTANT USERS ######################
              ################################################
              shiny::h3("Important users"),
              shiny::fluidRow(
                shiny::column(4, shiny::h5("User file")),
                shiny::column(
                  2,
                  shiny::downloadButton("conf_users_download", "Download")
                ),
                shiny::column(
                  2,
                  shiny::downloadButton(
                    "conf_orig_users_download",
                    "Download default"
                  )
                ),
                shiny::column(
                  4,
                  shiny::fileInput(
                    "conf_users_upload",
                    label = NULL,
                    buttonLabel = "Upload", accept = ".xlsx"
                  )
                ),
              ),
              ################################################
              ######### SUSCRIBERS PANEL #####################
              ################################################
              shiny::h3("Subscribers"),
              shiny::fluidRow(
                shiny::column(4, shiny::h5("Subscribers")),
                shiny::column(
                  2,
                  shiny::downloadButton("conf_subscribers_download", "Download")
                ),
                shiny::column(
                  2,
                  shiny::downloadButton(
                    "conf_orig_subscribers_download",
                    "Download default"
                  )
                ),
                shiny::column(
                  4,
                  shiny::fileInput(
                    "conf_subscribers_upload",
                    label = NULL,
                    buttonLabel = "Upload", accept = ".xlsx"
                  )
                ),
              ),
              DT::dataTableOutput("config_subscribers"),
              ################################################
              ######### COUNTRIES / REGIONS PANEL ############
              ################################################
              shiny::h3("Countries & regions"),
              shiny::fluidRow(
                shiny::column(4, shiny::h5("Countries & regions")),
                shiny::column(
                  2,
                  shiny::downloadButton("conf_countries_download", "Download")
                ),
                shiny::column(
                  2,
                  shiny::downloadButton(
                    "conf_orig_countries_download",
                    "Download defaults"
                  )
                ),
                shiny::column(
                  4,
                  shiny::fileInput(
                    "conf_countries_upload",
                    label = NULL,
                    buttonLabel = "Upload", accept = ".xlsx"
                  )
                ),
              ),
              DT::dataTableOutput("config_regions")
            )
          )
        )
      # Defining geo tuning page UI
      ################################################
      ######### GEOTRAINING PAGE #####################
      ################################################
      geotraining_page <-
        shiny::fluidPage(
          shiny::h3("Geo tagging training"),
          shiny::fluidRow(
            ################################################
            ######### GEO TAG FILTERS ######################
            ################################################
            shiny::column(
              4,
              shiny::numericInput(
                "geotraining_posts2add",
                label = shiny::h4("Posts to add"),
                value = 100
              )
            ),
            shiny::column(
              2,
              shiny::actionButton("geotraining_update", "Geolocate annotations")
            ),
            shiny::column(
              2,
              shiny::downloadButton("geotraining_download", "Download annotations")
            ),
            shiny::column(
              4,
              shiny::fileInput(
                "geotraining_upload",
                label = NULL,
                buttonLabel = "Upload & evaluate annotations", accept = ".xlsx"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              ################################################
              ######### GEO TAG EVALUATION####################
              ################################################
              shiny::h4(
                "Performance evaluation of geo tagging algorithm (finding location position in text)"
              ),
              DT::dataTableOutput("geotraining_eval_df"),
              ################################################
              ######### GEO TAG TABLE #########################
              ################################################
              shiny::h4("Database used for training the geo tagging algorithm"),
              DT::dataTableOutput("geotraining_table")
            )
          )
        )
      ################################################
      ######### DATA PROTECTION PAGE #################
      ################################################
      dataprotection_page <-
        shiny::fluidPage(
          shiny::h3("Data protection"),
          shiny::fluidRow(
            ################################################
            ######### TWEET SEARCH FILTERS #################
            ################################################
            shiny::column(2, shiny::h4("Topic")),
            shiny::column(2, shiny::h4("Period")),
            shiny::column(2, shiny::h4("Countries & regions")),
            shiny::column(
              2,
              shiny::h4("Users"),
              shiny::radioButtons(
                "data_mode",
                label = NULL,
                choices = list(
                  "Mentioning" = "mentioning",
                  "From User" = "users",
                  "Both" = "all"
                ),
                selected = "all",
                inline = TRUE
              ),
            ),
            shiny::column(1, shiny::h4("Limit")),
            shiny::column(3, shiny::h4("Action"))
          ),
          shiny::fluidRow(
            shiny::column(
              2,
              shiny::selectInput(
                "data_topics",
                label = NULL,
                multiple = TRUE,
                choices = d$topics[d$topics != ""]
              )
            ),
            shiny::column(
              2,
              shiny::dateRangeInput(
                "data_period",
                label = "",
                start = d$date_end,
                end = d$date_end,
                min = d$date_min,
                max = d$date_max,
                format = "yyyy-mm-dd",
                startview = "month"
              )
            ),
            shiny::column(
              2,
              shiny::selectInput(
                "data_countries",
                label = NULL,
                multiple = TRUE,
                choices = d$countries
              )
            ),
            shiny::column(
              2,
              shiny::textInput("data_users", label = NULL, value = NULL)
            ),
            shiny::column(
              1,
              shiny::selectInput(
                "data_limit",
                label = NULL,
                multiple = FALSE,
                choices = list("50" = "50", "100" = "100", "500" = "500"),
                selected = "50"
              )
            ),
            shiny::column(
              3,
              shiny::actionButton("data_search", "Search"),
              shiny::actionButton("data_search_ano", "Anonym Search"),
              shiny::actionButton("data_anonymise", "Anonymize"),
              shiny::actionButton("data_delete", "Delete"),
            ),
          ),
          shiny::fluidRow(
            shiny::column(12, shiny::htmlOutput("data_message"))
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              ################################################
              ######### TWEET SEARCH RESULTS #################
              ################################################
              DT::dataTableOutput("data_search_df")
            )
          )
        )
      ################################################
      ######### TROUBLESHOOT PAGE ####################
      ################################################
      troubleshoot_page <-
        shiny::fluidPage(
          shiny::h3("Create snapshot file"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxGroupInput(
                "snapshot_types",
                "Include:",
                inline = TRUE,
                choices = c(
                  "Settings" = "settings",
                  "Dependencies" = "dependencies",
                  "Machine Learning" = "machine-learning",
                  "Aggregation" = "aggregations",
                  "Posts" = "posts",
                  "Logs" = "logs"
                ),
                selected = c("settings", "logs")
              )
            ),
            shiny::column(
              3,
              shiny::dateRangeInput(
                "snapshot_aggr_period",
                label = shiny::h4("Aggregated data period"),
                start = d$date_min,
                end = d$date_end,
                min = d$date_min,
                max = d$date_max,
                format = "yyyy-mm-dd",
                startview = "month"
              )
            ),
            shiny::column(
              3,
              shiny::dateRangeInput(
                "snapshot_post_period",
                label = shiny::h4("Post data period"),
                start = d$date_min,
                end = d$date_end,
                min = d$date_min,
                max = d$date_max,
                format = "yyyy-mm-dd",
                startview = "month"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::textInput(
                "snapshot_folder",
                value = ensure_snapshot_folder(),
                label = "Destination folder"
              ),
              shiny::checkboxInput(
                "snapshot_compress",
                "Compress snapshot",
                value = FALSE
              ),
              shiny::p(
                "It is recommended to stop the search and detect loop before running a snapshopt to ensure data consistency (embedded database must be running)"
              ),
              shiny::actionButton("build_snapshot", "Create snapshot")
            )
          ),
          shiny::h3("Diagnostics"),
          shiny::h5("Automated diagnostic tasks"),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::actionButton("run_diagnostic", "run diagnostics")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              ################################################
              ######### DIAGNOSTIC TABLE #####################
              ################################################
              DT::dataTableOutput("diagnostic_table")
            )
          )
        )
  }
  # Defining navigation UI
  ui_app <- function(profile) {
    args = list("episomer", id = "navbar_shinyapp")
    if("dashboard" %in% profile) {
        args = c(args, list(shiny::tabPanel("Dashboard", value = "dashboard_page", dashboard_page)))
    }
    if("admin" %in% profile) {
        args = c(
           args,  
           list(
	        shiny::tabPanel("Alerts", value = "alerts_page", alerts_page),
                shiny::tabPanel("Geotag", value = "geotraining_page", geotraining_page),
                shiny::tabPanel(
                  "Data protection",
                  value = "dataprotection_page",
                  dataprotection_page
                ),
                shiny::tabPanel("Configuration", value = "config_page", config_page),
                shiny::tabPanel(
                  "Troubleshoot",
                  value = "troubleshoot_page",
                  troubleshoot_page
                )
	   )
        ) 
    }
    do.call(shiny::navbarPage, args)
  }
  # Defining server logic
  server_app <- function(input, output, session, profile, ...) {
    `%>%` <- magrittr::`%>%`

    shiny::onStop(function() {
      options("shiny.port" = NULL)
      options("shiny.host" = NULL)
    })
    
    if("dashboard" %in% profile) {

        ################################################
        ######### FILTERS LOGIC ########################
        ################################################

        # updating alpha filter based on selected topic value
        shiny::observe({
          # reading input$topics will automatically trigger the update on change
          val <- {
            if (length(input$topics) == 0 || input$topics == "")
              conf$alert_alpha else
              unname(get_topics_alphas()[stringr::str_replace_all(
                input$topics,
                "%20",
                " "
              )])
          }
          shiny::updateSliderInput(
            session,
            "alpha_filter",
            value = val,
            min = 0,
            max = 0.3,
            step = 0.005
          )
        })

        # upadating outliers alpha filter based on selected topic value
        shiny::observe({
          # reading input$topics will automatically trigger the update on change
          val <- {
            if (length(input$topics) == 0 || input$topics == "")
              conf$alert_alpha_outlier else
              unname(get_topics_alpha_outliers()[stringr::str_replace_all(
                input$topics,
                "%20",
                " "
              )])
          }
          shiny::updateSliderInput(
            session,
            "alpha_outlier_filter",
            value = val,
            min = 0,
            max = 0.3,
            step = 0.005
          )
        })

        # update the date ranges based on type of range selected
        shiny::observe({
          # reading input$fixed_period will automatically trigger the update on change
          # refresh dashboard data is necessary to update active dates
          refresh_dashboard_data(d, input$fixed_period)
          shiny::updateDateRangeInput(
            session,
            "period",
            start = d$date_start,
            end = d$date_end,
            min = d$date_min,
            max = d$date_max
          )
        })

        ################################################
        ######### DASHBOARD LOGIC ######################
        ################################################
        # updating the topics if they change on the configuraton page

	shiny::observe({
	  d$topics_refresh_flag()
	  shiny::isolate(refresh_dashboard_data(d))
	  shiny::updateSelectInput(
             session,
             "topics",
	     choices = d$topics
	  )
	})

	# dashboard actions
	shiny::observeEvent(input$run_dashboard, {
          # rendering line chart
          rep = new.env()
          progress_start("Generating report", rep)
          output$line_chart <- plotly::renderPlotly({
            # Validate if minimal requirements for rendering are met
            progress_set(value = 0.15, message = "Generating line chart", rep)
            shiny::isolate({
              can_render(input, d)
              # getting the chart
              chart <- line_chart_from_filters(
                input$sms,
                input$topics,
                input$countries,
                input$period_type,
                input$period,
                input$with_quotes,
                input$alpha_filter,
                input$alpha_outlier_filter,
                input$k_decay_filter,
                input$history_filter,
                input$bonferroni_correction,
                input$same_weekday_baseline
              )$chart

              # Setting size based on container size
              height <- session$clientData$output_line_chart_height
              width <- session$clientData$output_line_chart_width

              # returning empty chart if no data is found on chart
              chart_not_empty(chart)

              # transforming chart on plotly
              gg <- plotly::ggplotly(
                chart,
                height = height,
                width = width,
                tooltip = c("label")
              ) %>%
                plotly::config(displayModeBar = FALSE)

              # Fixing bad entries on ggplotly chart
              for (i in 1:length(gg$x$data)) {
                if (
                  startsWith(gg$x$data[[i]]$name, "(") &&
                    endsWith(gg$x$data[[i]]$name, ")")
                )
                  gg$x$data[[i]]$name = gsub(
                    "\\(|\\)|,|[0-9]",
                    "",
                    gg$x$data[[i]]$name
                  ) else gg$x$data[[i]]$name = "                        "
                gg$x$data[[i]]$legendgroup = gsub(
                  "\\(|\\)|,|[0-9]",
                  "",
                  gg$x$data[[i]]$legendgroup
                )
              }
              gg
            })
          })

          # rendering map chart
          output$map_chart <- plotly::renderPlotly({
            # Validate if minimal requirements for rendering are met
            progress_set(value = 0.30, message = "Generating map chart", rep)

            shiny::isolate({
              can_render(input, d)
              # Setting size based on container size
              height <- session$clientData$output_map_chart_height
              width <- session$clientData$output_map_chart_width

              # getting the chart
              chart <- map_chart_from_filters(
                input$sms,
                input$topics,
                input$countries,
                input$period,
                input$with_quotes
              )$chart

              # returning empty chart if no data is found on chart
              chart_not_empty(chart)

              # transforming chart on plotly
              gg <- chart %>%
                plotly::ggplotly(
                  height = height,
                  width = width,
                  tooltip = c("label")
                ) %>%
                plotly::layout(
                  title = list(text = paste("<b>", chart$labels$title, "</b>")),
                  margin = list(l = 5, r = 5, b = 50, t = 80),
                  annotations = list(
                    text = chart$labels$caption,
                    font = list(size = 10),
                    showarrow = FALSE,
                    xref = 'paper',
                    x = 0,
                    yref = 'paper',
                    y = -0.15
                  ) #,
                  #legend = list(orientation = 'v')
                ) %>%
                plotly::config(displayModeBar = FALSE)

              # Fixing bad entries on ggplotly chart
              for (i in 1:length(gg$x$data)) {
                if (
                  substring(gg$x$data[[i]]$name, 1, 2) %in%
                    c("a.", "b.", "c.", "d.", "e.", "f.", "g.", "h.")
                )
                  gg$x$data[[i]]$name <- substring(
                    gg$x$data[[i]]$name,
                    4,
                    nchar(gg$x$data[[i]]$name)
                  )
                #gg$x$data[[i]]$legendgroup <- gsub("\\(|\\)|,|[0-9]", "", gg$x$data[[i]]$legendgroup)
              }
              gg
            })
          })

          output$top_chart1 <- plotly::renderPlotly({
            # Validate if minimal requirements for rendering are met
            progress_set(value = 0.45, message = "Generating top chart 1", rep)
            shiny::isolate({
              can_render(input, d)
              # Setting size based on container size
              height <- session$clientData$output_top_chart1_height
              width <- session$clientData$output_top_chart1_width

              # getting the chart
              chart <- top_chart_from_filters(
                input$sms,
                input$topics,
                "tags",
                input$countries,
                input$period,
                input$with_quotes,
                20
              )$chart

              # returning empty chart if no data is found on chart
              chart_not_empty(chart)

              # transforming chart on plotly
              chart %>%
                plotly::ggplotly(height = height, width = width) %>%
                plotly::layout(
                  title = list(
                    text = paste(
                      "<b>",
                      chart$labels$title,
                      "<br>",
                      chart$labels$subtitle
                    ),
                    "</b>"
                  ),
                  margin = list(l = 30, r = 30, b = 100, t = 80),
                  annotations = list(
                    text = chart$labels$caption,
                    font = list(size = 10),
                    showarrow = FALSE,
                    xref = 'paper',
                    x = 0,
                    yref = 'paper',
                    y = -0.4
                  )
                ) %>%
                plotly::config(displayModeBar = FALSE)
            })
          })

          output$top_chart2 <- plotly::renderPlotly({
            # Validate if minimal requirements for rendering are met
            progress_set(value = 0.7, message = "Generating top chart 2", rep)
            shiny::isolate({
              can_render(input, d)
              # Setting size based on container size
              height <- session$clientData$output_top_chart2_height
              width <- session$clientData$output_top_chart2_width

              # getting the chart
              chart <- top_chart_from_filters(
                input$sms,
                input$topics,
                "topwords",
                input$countries,
                input$period,
                input$with_quotes,
                20
              )$chart

              # returning empty chart if no data is found on chart
              chart_not_empty(chart)

              # transforming chart on plotly
              chart %>%
                plotly::ggplotly(height = height, width = width) %>%
                plotly::layout(
                  title = list(
                    text = paste(
                      "<b>",
                      chart$labels$title,
                      "<br>",
                      chart$labels$subtitle
                    ),
                    "</b>"
                  ),
                  margin = list(l = 30, r = 30, b = 100, t = 80),
                  annotations = list(
                    text = chart$labels$caption,
                    font = list(size = 10),
                    showarrow = FALSE,
                    xref = 'paper',
                    x = 0,
                    yref = 'paper',
                    y = -0.4
                  )
                ) %>%
                plotly::config(displayModeBar = FALSE)
            })
          })

          output$top_table_title <- shiny::isolate({
            shiny::renderText({
              topic <- unname(get_topics_labels()[stringr::str_replace_all(
                input$topics,
                "%20",
                " "
              )])
              paste(
                "<h5 style=\"font-weight:bold;font-family:Helvetica;font-size:16px\">Top URLS of messages mentioning",
                topic,
                "from",
                input$period[[1]],
                "to",
                input$period[[2]],
                "</h5>"
              )
            })
          })
          output$top_table <- DT::renderDataTable({
            # Validate if minimal requirements for rendering are met
            progress_set(value = 0.85, message = "Generating top links", rep)

            shiny::isolate({
              can_render(input, d)
              # Setting size based on container size
              height <- session$clientData$output_top_chart2_height
              width <- session$clientData$output_top_chart2_width

              # getting the chart to obtain the table
              chart <- top_chart_from_filters(
                input$sms,
                input$topics,
                "urls",
                input$countries,
                input$period,
                input$with_quotes,
                200
              )$chart

              # returning empty if no data is found on chart
              chart_not_empty(chart)

              data <- chart$data %>%
                dplyr::mutate(
                  top = paste(
                    "<a href=\"",
                    .data$top,
                    "\"  target=\"_blank\">",
                    .data$top,
                    "</a>"
                  )
                ) %>%
                dplyr::rename(Url = .data$top, Frequency = .data$frequency) %>%
                DT::datatable(escape = FALSE)
            })
          })

          output$top_table_disc <- shiny::isolate({
            shiny::renderText({
              progress_close(rep)
            })
          })
        })
        # Saving line chart as png and downloading it from shiny app
        output$export_line <- shiny::downloadHandler(
          filename = function() {
            paste(
              "line_dataset_",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".png",
              sep = ""
            )
          },
          content = function(file) {
            chart <-
              line_chart_from_filters(
                input$sms,
                input$topics,
                input$countries,
                input$period_type,
                input$period,
                input$with_quotes,
                input$alpha_filter,
                input$alpha_outlier_filter,
                input$k_decay_filter,
                input$history_filter,
                input$bonferroni_correction,
                input$same_weekday_baseline
              )$chart
            device <- function(..., width, height)
              grDevices::png(
                ...,
                width = width,
                height = height,
                res = 300,
                units = "in"
              )
            ggplot2::ggsave(file, plot = chart, device = device)
          }
        )
        # Saving line chart as CSV and downloading it from shiny app
        output$download_line_data <- shiny::downloadHandler(
          filename = function() {
            paste(
              "line_dataset_",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".csv",
              sep = ""
            )
          },
          content = function(file) {
            write.csv(
              line_chart_from_filters(
                input$sms,
                input$topics,
                input$countries,
                input$period_type,
                input$period,
                input$with_quotes,
                input$alpha_filter,
                input$alpha_outlier_filter,
                input$k_decay_filter,
                input$history_filter,
                input$bonferroni_correction,
                input$same_weekday_baseline
              )$data,
              file,
              row.names = FALSE
            )
          }
        )

        # Saving map chart as CSV and downloading it from shiny app
        output$download_map_data <- shiny::downloadHandler(
          filename = function() {
            paste(
              "map_dataset_",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".csv",
              sep = ""
            )
          },
          content = function(file) {
            write.csv(
              map_chart_from_filters(
                input$sms,
                input$topics,
                input$countries,
                input$period,
                input$with_quotes
              )$data,
              file,
              row.names = FALSE
            )
          }
        )

        # Saving map chart as png and downloading it from shiny app
        output$export_map <- shiny::downloadHandler(
          filename = function() {
            paste(
              "map_dataset_",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".png",
              sep = ""
            )
          },
          content = function(file) {
            chart <- map_chart_from_filters(
              input$sms,
              input$topics,
              input$countries,
              input$period,
              input$with_quotes
            )$chart
            device <- function(..., width, height)
              grDevices::png(
                ...,
                width = width,
                height = height,
                res = 300,
                units = "in"
              )
            ggplot2::ggsave(file, plot = chart, device = device)
          }
        )

        # Saving top chart 1 as CSV and downloading it from shiny app
        output$download_top1_data <- shiny::downloadHandler(
          filename = function() {
            paste(
              "top_dataset_",
              "tags",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".csv",
              sep = ""
            )
          },
          content = function(file) {
            write.csv(
              top_chart_from_filters(
                input$sms,
                input$topics,
                "tags",
                input$countries,
                input$period,
                input$with_quotes,
                200
              )$data,
              file,
              row.names = FALSE
            )
          }
        )
        # Saving top chart 2 as CSV and downloading it from shiny app
        output$download_top2_data <- shiny::downloadHandler(
          filename = function() {
            paste(
              "top_dataset_",
              "topwords",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".csv",
              sep = ""
            )
          },
          content = function(file) {
            write.csv(
              top_chart_from_filters(
                input$sms,
                input$topics,
                "topwords",
                input$countries,
                input$period,
                input$with_quotes,
                200
              )$data,
              file,
              row.names = FALSE
            )
          }
        )
        # Saving top table as CSV and downloading it from shiny app
        output$download_top3_data <- shiny::downloadHandler(
          filename = function() {
            paste(
              "top_dataset_",
              "urls",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".csv",
              sep = ""
            )
          },
          content = function(file) {
            write.csv(
              top_chart_from_filters(
                input$sms,
                input$topics,
                "urls",
                input$countries,
                input$period,
                input$with_quotes,
                200
              )$data,
              file,
              row.names = FALSE
            )
          }
        )

        # Saving top chart 1 as png and downloading it from shiny app
        output$export_top1 <- shiny::downloadHandler(
          filename = function() {
            paste(
              "top_",
              "tags",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".png",
              sep = ""
            )
          },
          content = function(file) {
            chart <-
              top_chart_from_filters(
                input$sms,
                input$topics,
                "tags",
                input$countries,
                input$period,
                input$with_quotes,
                50
              )$chart
            device <- function(..., width, height)
              grDevices::png(
                ...,
                width = width,
                height = height,
                res = 300,
                units = "in"
              )
            ggplot2::ggsave(file, plot = chart, device = device)
          }
        )

        # Saving top chart 2 as png and downloading it from shiny app
        output$export_top2 <- shiny::downloadHandler(
          filename = function() {
            paste(
              "top_",
              "topwords",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              ".png",
              sep = ""
            )
          },
          content = function(file) {
            chart <-
              top_chart_from_filters(
                input$sms,
                input$topics,
                "topwords",
                input$countries,
                input$period,
                input$with_quotes,
                50
              )$chart
            device <- function(..., width, height)
              grDevices::png(
                ...,
                width = width,
                height = height,
                res = 300,
                units = "in"
              )
            ggplot2::ggsave(file, plot = chart, device = device)
          }
        )

        # Saving dashboard as PDF downloading it from shiny app
        output$export_pdf <- shiny::downloadHandler(
          filename = function() {
            paste(
              "episomer_dashboard_",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              "_",
              "tags",
              "_",
              "topwords",
              ".pdf",
              sep = ""
            )
          },
          content = function(file) {
            export_dashboard(
              "pdf_document",
              file,
              input$sms,
              input$topics,
              input$countries,
              input$period_type,
              input$period,
              input$with_quotes,
              input$location_type,
              input$alpha_filter,
              input$alpha_outlier_filter,
              input$k_decay_filter,
              input$history_filter,
              input$bonferroni_correction,
              input$same_weekday_baseline,
              "tags",
              "topwords"
            )
          }
        )
        # Saving dashboard as markdown downloading it from shiny app
        output$export_md <- shiny::downloadHandler(
          filename = function() {
            paste(
              "episomer_dashboard_",
              "_",
              paste(input$topics, collapse = "-"),
              "_",
              paste(input$countries, collapse = "-"),
              "_",
              input$period[[1]],
              "_",
              input$period[[2]],
              "_",
              "tags",
              "_",
              "topwords",
              ".md",
              sep = ""
            )
          },
          content = function(file) {
            export_dashboard(
              "md_document",
              file,
              input$sms,
              input$topics,
              input$countries,
              input$period_type,
              input$period,
              input$with_quotes,
              input$location_type,
              input$alpha_filter,
              input$alpha_outlier_filter,
              input$k_decay_filter,
              input$history_filter,
              input$bonferroni_correction,
              input$same_weekday_baseline,
              "tags",
              "topwords"
            )
          }
        )
    }


    if("admin" %in% profile) {
        ################################################
        ######### CONFIGURATION LOGIC ##################
        ################################################
        
        ######### STATUS PANEL LOGIC ##################
        # Timer for updating task statuses
        # each ten seconds config data will be reloaded to capture changes from data collection & processing and requirements & alerts pipelines
        # each ten seconds a process refresh flag will be invalidated to trigger process status recalculation
        shiny::observe({
          # Setting the timer
          shiny::invalidateLater(10000)

          # updating config data
          refresh_config_data(cd, list("tasks", "topics", "langs"))

          # invalidating the process refresh flag
          cd$process_refresh_flag(Sys.time())
        })

        # rendering the Data collection & processing running status
        output$search_running <- shiny::renderText({
          # Adding a dependency to task refresh (each time a task has changed by the Requirements & alerts pipeline)
          cd$tasks_refresh_flag()
          # Adding a dependency to process update (each 10 seconds)
          cd$process_refresh_flag()

          color = if (
            cd$search_running &&
              !is.na(cd$search_diff) &&
              length(cd$missing_search_jobs) == 0
          ) {
            "#348017"
          } else if (!cd$search_running || is.na(cd$search_diff)) {
            "#F75D59"
          } else {
            "#EfAF0D"
          }

          message = ""
          if (!cd$search_running || is.na(cd$search_diff)) {
            message = "Stopped"
          } else {
            message = "Running"
          }
          if (!is.na(cd$search_diff)) {
            message = paste(
              message,
              "( wrote",
              round(cd$search_diff, 2),
              units(cd$search_diff),
              "ago )"
            )
          }
          if (length(cd$missing_search_jobs) > 0) {
            message = paste(
              message,
              "<br/>",
              paste(cd$missing_search_jobs, collapse = ", "),
              " missing"
            )
          }

          # updating the label
          paste(
            "<span",
            " style='color:",
            color,
            "'>",
            message,
            "</span>",
            sep = ""
          )
        })

        # rendering the fs running status
        output$fs_running <- shiny::renderText({
          # Adding a dependency to task refresh (each time a task has changed by the Requirements & alerts pipeline)
          cd$tasks_refresh_flag()
          # Adding a dependency to process update (each 10 seconds)
          cd$process_refresh_flag()

          # updating the label
          paste(
            "<span",
            " style='color:",
            if (cd$fs_running) "#348017'" else "#F75D59'",
            ">",
            if (cd$fs_running) "Running" else "Stopped",
            "</span>",
            sep = ""
          )
        })

        # rendering the Requirements & alerts running status
        output$detect_running <- shiny::renderText({
          # Adding a dependency to task refresh (each time a task has changed by the Requirements & alerts pipeline)
          cd$tasks_refresh_flag()
          # Adding a dependency to process update (each 10 seconds)
          cd$process_refresh_flag()

          # updating the label
          paste(
            "<span",
            " style='color:",
            if (cd$detect_running) "#348017'" else "#F75D59'",
            ">",
            if (cd$detect_running) "Running" else "Stopped",
            "</span>",
            sep = ""
          )
        })

        # rendering the slots each time properties are changed
        output$conf_schedule_slots <- shiny::renderText({
          # Adding a dependency to config refresh
          cd$properties_refresh_flag()

          #rendering the label
          paste(
            head(
              lapply(
                get_task_day_slots(get_tasks()$alerts),
                function(d) strftime(d, format = "%H:%M")
              ),
              -1
            ),
            collapse = ", "
          )
        })

        # registering the Data collection & processing runner after button is clicked
        shiny::observeEvent(input$activate_search, {
          # registering the scheduled task
          issues = loop_run_issues("search")
          if (issues == "") {
            register_search_runner_task()
            # refresh task data to check if tasks are to be updated
            refresh_config_data(cd, list("tasks"))
          } else {
            shiny::showModal(shiny::modalDialog(
              title = "Warning",
              issues,
              footer = shiny::tagList(shiny::modalButton("OK"))
            ))
          }
        })
        shiny::observeEvent(input$stop_search, {
          stop_search_runner_task()
          # refresh task data to check if tasks are to be updated
          refresh_config_data(cd, list("tasks"))
        })

        # registering the fs runner after button is clicked
        shiny::observeEvent(input$activate_fs, {
          issues = loop_run_issues("fs")
          if (issues == "") {
            # registering the scheduled task
            register_fs_runner_task()
            # refresh task data to check if tasks are to be updated
            refresh_config_data(cd, list("tasks"))
          } else {
            shiny::showModal(shiny::modalDialog(
              title = "Warning",
              issues,
              footer = shiny::tagList(shiny::modalButton("OK"))
            ))
          }
        })

        shiny::observeEvent(input$stop_fs, {
          # stop the scheduled task
          stop_fs_runner_task()
          # refresh task data to check if tasks are to be updated
          refresh_config_data(cd, list("tasks"))
        })

        # registering the Requirements & alerts runner after button is clicked
        shiny::observeEvent(input$activate_detect, {
          # Setting values configuration to trigger one shot tasks for the first time
          if (is.na(conf$dep_updated_on))
            conf$dep_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          if (is.na(conf$geonames_updated_on))
            conf$geonames_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          if (is.na(conf$lang_updated_on))
            conf$lang_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          # Forcing refresh of tasks
          cd$tasks_refresh_flag(Sys.time())
          # saving properties to ensure the Requirements & alerts pipeline can see the changes and start running the tasks
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )

          # registering the scheduled task
          register_detect_runner_task()

          # refresh task data to check if tasks are to be updated
          refresh_config_data(cd, list("tasks"))
        })

        shiny::observeEvent(input$stop_detect, {
          stop_detect_runner_task()
          # refresh task data to check if tasks are to be updated
          refresh_config_data(cd, list("tasks"))
        })

        # action to activate de dependency download tasks
        shiny::observeEvent(input$update_dependencies, {
          # Setting values on the configuration so the Requirements & alerts pipeline know it has to launch the task
          conf$dep_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          # forcing a task refresh :TODO test if this is still necessary
          cd$tasks_refresh_flag(Sys.time())
          # saving configuration so the Requirements & alerts pipeline will see the changes
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )
          # refreshing the tasks data
          refresh_config_data(cd, list("tasks"))
        })

        # action to activate the update geonames task
        shiny::observeEvent(input$update_geonames, {
          # Setting values on the configuration so the Requirements & alerts pipeline know it has to launch the task
          conf$geonames_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          # forcing a task refresh
          cd$tasks_refresh_flag(Sys.time())
          # saving configuration so the Requirements & alerts pipeline will see the changes
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )
          # refreshing the tasks data
          refresh_config_data(cd, list("tasks"))
        })

        # action to activate the update languages task
        shiny::observeEvent(input$update_languages, {
          # Setting values on the configuration so the Requirements & alerts pipeline know it has to launch the task
          conf$lang_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          # forcing a task refresh
          cd$tasks_refresh_flag(Sys.time())
          # saving configuration so the Requirements & alerts pipeline will see the changes
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )
          # refreshing the tasks data
          refresh_config_data(cd, list("tasks"))
        })

        # action to activate the alerts task
        shiny::observeEvent(input$request_alerts, {
          # Setting values on the configuration so the Requirements & alerts pipeline know it has to launch the task
          conf$alerts_requested_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          # forcing a task refresh
          cd$tasks_refresh_flag(Sys.time())
          # saving configuration so the Requirements & alerts pipeline will see the changes
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )
          # refreshing the tasks data
          refresh_config_data(cd, list("tasks"))
        })      

        ######### PROPERTIES LOGIC ##################
        reactive_elements_to_save_in_conf <- shiny::reactive({
          list(
          conf_collect_span = input$conf_collect_span,
          conf_schedule_span = input$conf_schedule_span,
          conf_keyring = input$conf_keyring,
          conf_spark_cores = input$conf_spark_cores,
          conf_spark_memory = input$conf_spark_memory,
          conf_onthefly_api = input$conf_onthefly_api,
          geolocation_threshold = input$geolocation_threshold,
          conf_geonames_url = input$conf_geonames_url,
          conf_maven_repo = input$conf_maven_repo,
          conf_winutils_url = input$conf_winutils_url,
          conf_geonames_simplify = input$conf_geonames_simplify,
          conf_regions_disclaimer = input$conf_regions_disclaimer,
          conf_alpha = input$conf_alpha,
          conf_alpha_outlier = input$conf_alpha_outlier,
          conf_k_decay = input$conf_k_decay,
          conf_history = input$conf_history,
          conf_same_weekday_baseline = input$conf_same_weekday_baseline,
          conf_with_bonferroni_correction = input$conf_with_bonferroni_correction,
          conf_with_quotes = input$conf_with_quotes,
          conf_smtp_host = input$smtp_host,
          conf_smtp_port = input$smtp_port,
          conf_smtp_insecure = input$smtp_insecure,
          conf_admin_email = input$admin_email,
          conf_smtp_from = input$smtp_from,
          conf_smtp_login = input$smtp_login,
          conf_smtp_password = input$smtp_password,
          conf_sm_alerts_bluesky = input$conf_sm_alerts_bluesky,
          conf_sm_activated_bluesky = input$conf_sm_activated_bluesky,
          conf_force_date_format = input$force_date_format
	  )
    })
    debounced_elements_to_save_in_conf <- shiny::debounce(reactive_elements_to_save_in_conf, 3000)

        shiny::observeEvent(debounced_elements_to_save_in_conf(), {          
          update_config_from_input(debounced_elements_to_save_in_conf())

          # Saving Bluesky propertes          
          sm_api_set_auth(
            network = "bluesky",
            shiny_input_list = input
          )                      
          
          # Saving properties.json
          save_config(
            data_dir = conf$data_dir,
          )

          # Forcing update on properties dependant refresh (e.g. time slots)
          cd$properties_refresh_flag(Sys.time())

        }, ignoreInit = TRUE)

        ######### IMPORTANT USERS LOGIC ###########
        # downloading current important users file
        output$conf_users_download <- shiny::downloadHandler(
          filename = function() "users.xlsx",
          content = function(file) {
            file.copy(get_known_users_path(), file)
          }
        )

        # downloading default important users file
        output$conf_orig_users_download <- shiny::downloadHandler(
          filename = function() "users.xlsx",
          content = function(file) {
            file.copy(get_default_known_users_path(), file)
          }
        )

        # uploading new important users
        shiny::observe({
          df <- input$conf_users_upload
          if (!is.null(df) && nrow(df) > 0) {
            uploaded <- df$datapath[[1]]
            file.copy(
              uploaded,
              paste(conf$data_dir, "users.xlsx", sep = "/"),
              overwrite = TRUE
            )
          }
        })

        ######### LANGUAGE LOGIC ##################
        # rendering languages each time a change is registered on languges
        output$config_langs <- DT::renderDataTable({
          # Adding dependency with lang refresh
          cd$langs_refresh_flag()
          DT::datatable(cd$langs)
        })

        # downloading current languages
        output$conf_lang_download <- shiny::downloadHandler(
          filename = function() "languages.xlsx",
          content = function(file) {
            file.copy(get_available_languages_path(), file)
          }
        )

        # downloading original languages file
        output$conf_orig_lang_download <- shiny::downloadHandler(
          filename = function() "languages.xlsx",
          content = function(file) {
            file.copy(get_default_available_languages_path(), file)
          }
        )

        # uploading new language file
        shiny::observe({
          df <- input$conf_lang_upload
          if (!is.null(df) && nrow(df) > 0) {
            uploaded <- df$datapath[[1]]
            file.copy(
              uploaded,
              paste(conf$data_dir, "languages.xlsx", sep = "/"),
              overwrite = TRUE
            )
            refresh_config_data(e = cd, limit = list("langs"))
          }
        })

        # rendering active languages when something has changed on languages
        output$lang_items_0 <- shiny::renderUI({
          # Adding a dependency to lang refresh
          cd$langs_refresh_flag()
          shiny::selectInput(
            "lang_items",
            label = NULL,
            multiple = FALSE,
            choices = cd$lang_items
          )
        })

        # adding the current language
        shiny::observeEvent(input$conf_lang_add, {
          add_config_language(input$lang_items, cd$lang_names[input$lang_items])
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )
          refresh_config_data(e = cd, limit = list("langs"))
        })

        #removing a language
        shiny::observeEvent(input$conf_lang_remove, {
          remove_config_language(input$lang_items)
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )
          refresh_config_data(e = cd, limit = list("langs"))
        })

        ######### TASKS LOGIC ##################
        # rendering the tasks each time something changes in the tasks
        output$tasks_df <- DT::renderDataTable({
          # Adding dependency with tasks refresh
          cd$tasks_refresh_flag()
          DT::datatable(cd$tasks_df)
        })

        ######### TOPICS LOGIC ##################
        # rendering the topics only on first load update is done on next statement
        output$config_topics <- DT::renderDataTable({
          `%>%` <- magrittr::`%>%`
          cd$topics_df %>%
            DT::datatable(
              colnames = c(
                "Query length" = "QueryLength",
                "Active plans" = "ActivePlans",
                "Progress" = "Progress",
                "Requests" = "Requests",
                "Signal alpha (FPR)" = "Alpha",
                "Outlier alpha (FPR)" = "OutliersAlpha"
              ),
              filter = "top",
              escape = TRUE
            ) %>%
            DT::formatPercentage(columns = c("Progress"))
        })
        # this is for updating the topics
        shiny::observe({
          # Adding a dependency to topics refresh or plans refresh
          cd$topics_refresh_flag()
          cd$plans_refresh_flag()
          DT::replaceData(DT::dataTableProxy('config_topics'), cd$topics_df)
        })

        # download the current topics file
        output$conf_topics_download <- shiny::downloadHandler(
          filename = function() "topics.xlsx",
          content = function(file) {
            file.copy(get_user_topics_path(), file)
          }
        )

        #download the original topics file
        output$conf_orig_topics_download <- shiny::downloadHandler(
          filename = function() "topics.xlsx",
          content = function(file) {
            file.copy(get_default_topics_path(), file)
          }
        )

        # uploading a new topics file
        shiny::observe({
          df <- input$conf_topics_upload
          if (!is.null(df) && nrow(df) > 0) {
            uploaded <- df$datapath[[1]]
            if(file.exists(uploaded)) {
              file.copy(
                uploaded,
                paste(conf$data_dir, "topics.xlsx", sep = "/"),
                overwrite = TRUE
              )
	      file.remove(uploaded)
              refresh_config_data(e = cd, limit = list("topics"))
	      d$topics_refresh_flag(cd$topics_refresh_flag())
	    }
          }
        })
        # action to dismiss past posts
        shiny::observeEvent(input$conf_dismiss_past_posts, {
          # Setting values on the configuration so the search loop knows history needs to be dismissed
          conf$dismiss_past_request <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          # saving configuration so the Requirements & alerts pipeline will see the changes
          save_config(
            data_dir = conf$data_dir,
            properties = TRUE,
            sm_topics = list()
          )
          # refreshing the tasks data
        })

        ######### SUBSCRIBERS LOGIC ##################
        # rendering subscribers (first run update on next statement)
        output$config_subscribers <- DT::renderDataTable({
          `%>%` <- magrittr::`%>%`
          get_subscribers() %>%
            DT::datatable(
              colnames = c(
                "User" = "User",
                "Email" = "Email",
                "Topics" = "Topics",
                "Excluded topics" = "Excluded Topics",
                "Immediate topics" = "Real time Topics",
                "Regions" = "Regions",
                "Immediate regions" = "Real time Regions",
                "Alert category" = "Alert category",
                "Alert slots" = "Alert Slots"
              ),
              filter = "top",
              escape = TRUE
            )
        })

        # updating the subscriber table on change of subscribers file
        shiny::observe({
          # Adding a dependency to subscribers refresh
          cd$subscribers_refresh_flag()
          DT::replaceData(
            DT::dataTableProxy('config_subscribers'),
            get_subscribers()
          )
        })

        # downloading subscribers default file
        output$conf_orig_subscribers_download <- shiny::downloadHandler(
          filename = function() "subscribers.xlsx",
          content = function(file) {
            file.copy(get_default_subscribers_path(), file)
          }
        )

        # downloading subscriber current file
        output$conf_subscribers_download <- shiny::downloadHandler(
          filename = function() "subscribers.xlsx",
          content = function(file) {
            file.copy(get_subscribers_path(), file)
          }
        )

        # uploading subscribers file
        shiny::observe({
          df <- input$conf_subscribers_upload
          if (!is.null(df) && nrow(df) > 0) {
            uploaded <- df$datapath[[1]]
            file.copy(
              uploaded,
              paste(conf$data_dir, "subscribers.xlsx", sep = "/"),
              overwrite = TRUE
            )
            cd$subscribers_refresh_flag(Sys.time())
          }
        })

        ######### REGIONS LOGIC ##################
        # rendering reguions for the first time (update on next statement)
        output$config_regions <- DT::renderDataTable({
          `%>%` <- magrittr::`%>%`
          get_regions_df() %>%
            DT::datatable(
              colnames = c(
                "Name" = "Name",
                "Codes" = "Codes",
                "Level" = "Level",
                "Min. lat." = "MinLatitude",
                "Max. lat." = "MaxLatitude",
                "Min. long." = "MinLongitude",
                "Max. long." = "MaxLingityde"
              ),
              filter = "top",
              escape = TRUE
            )
        })

        # Updating regions when the new file is uploaded
        shiny::observe({
          # Adding a dependency to country refresh
          cd$countries_refresh_flag()
          DT::replaceData(DT::dataTableProxy('config_regions'), get_regions_df())
        })

        # Download the current country file
        output$conf_countries_download <- shiny::downloadHandler(
          filename = function() "countries.xlsx",
          content = function(file) {
            file.copy(get_countries_path(), file)
          }
        )

        # Dowload the original country file
        output$conf_orig_countries_download <- shiny::downloadHandler(
          filename = function() "countries.xlsx",
          content = function(file) {
            file.copy(get_default_countries_path(), file)
          }
        )

        #Update the new country file
        shiny::observe({
          df <- input$conf_countries_upload
          if (!is.null(df) && nrow(df) > 0) {
            uploaded <- df$datapath[[1]]
            file.copy(
              uploaded,
              paste(conf$data_dir, "countries.xlsx", sep = "/"),
              overwrite = TRUE
            )
            cd$countries_refresh_flag(Sys.time())
          }
        })

        ######### ALERTS LOGIC ##################
        # rendering the alerts
        # updates are launched automatically when any input value changes

        # setting default value for number of alerts to return depending on wether we are displaying posts or not
        shiny::observe({
          # Can also set the label and select items
          shiny::updateSelectInput(
            session,
            "alerts_limit",
            selected = if (input$alerts_display == "posts") "10" else "0"
          )
        })

        shiny::observeEvent(input$alerts_search, {
          shiny::updateTextInput(
            session,
            "alerts_show_search",
            label = NULL,
            value = "true"
          )
          output$alerts_table <- DT::renderDataTable({
            `%>%` <- magrittr::`%>%`
            shiny::isolate({
              topposts <- if (input$alerts_display == "posts") 10 else 0
              progress_start("Getting alerts")
              alerts <- get_alerts(
                topic = input$alerts_topics,
                countries = as.numeric(input$alerts_countries),
                from = input$alerts_period[[1]],
                until = input$alerts_period[[2]],
                topposts = topposts,
                limit = as.integer(input$alerts_limit),
                progress = function(value, message) {
                  progress_set(value = value, message = message)
                }
              )
            })
            shiny::validate(
              shiny::need(
                !is.null(alerts),
                'No alerts generated for the selected period'
              )
            )
            if (topposts == 0) {
            dt <-   alerts %>%
                dplyr::select(
                  "date",
                  "hour",
                  "topic",
                  "country",
                  "episomer_category",
                  "tops",
                  "number_of_posts",
                  "known_ratio",
                  "limit",
                  "no_historic",
                  "bonferroni_correction",
                  "same_weekday_baseline",
                  "rank",
                  "with_quotes",
                  "alpha",
                  "alpha_outlier",
                  "k_decay"
                ) %>%
                DT::datatable(
                  colnames = c(
                    "Date" = "date",
                    "Hour" = "hour",
                    "Topic" = "topic",
                    "Region" = "country",
                    "Category" = "episomer_category",
                    "Tops" = "tops",
                    "Posts" = "number_of_posts",
                    "% from important user" = "known_ratio",
                    "Threshold" = "limit",
                    "Baseline" = "no_historic",
                    "Bonf. corr." = "bonferroni_correction",
                    "Same weekday baseline" = "same_weekday_baseline",
                    "Day rank" = "rank",
                    "With quotes" = "with_quotes",
                    "Alert FPR (alpha)" = "alpha",
                    "Outlier FPR (alpha)" = "alpha_outlier",
                    "Downweight strenght" = "k_decay"
                  ),
                  filter = "top",
                  escape = FALSE
                )
            } else {
              alerts$topposts <- sapply(alerts$topposts, function(postsbylang) {
                if (length(postsbylang) == 0) "" else {
                  paste(
                    "<UL>",
                    lapply(1:length(postsbylang), function(i) {
                      paste(
                        "<LI>",
                        names(postsbylang)[[i]],
                        "<OL>",
                        paste(
                          lapply(postsbylang[[i]], function(t) {
                            paste0(
                              "<LI>",
                              htmltools::htmlEscape(t),
                              "</LI>"
                            )
                          }),
                          collapse = ""
                        ),
                        "</OL>",
                        "</LI>"
                      )
                    }),
                    "</UL>",
                    collapse = ""
                  )
                }
              })

              dt <- alerts %>%
                dplyr::select(
                  "date",
                  "hour",
                  "topic",
                  "country",
                  "episomer_category",
                  "tops",
                  "number_of_posts",
                  "topposts"
                ) %>%
                DT::datatable(
                  colnames = c(
                    "Date" = "date",
                    "Hour" = "hour",
                    "Topic" = "topic",
                    "Region" = "country",
                    "Category" = "episomer_category",
                    "Tops" = "tops",
                    "Posts" = "number_of_posts",
                    "Top posts" = "topposts"
                  ),
                  filter = "top",
                  escape = FALSE
                )
            }
            progress_close()
            dt
          })
        })

        shiny::observeEvent(input$alerts_close, {
          shiny::updateTextInput(
            session,
            "alerts_show_search",
            label = NULL,
            value = "false"
          )
        })

        shiny::observeEvent(input$alertsdb_search, {
          shiny::updateTextInput(
            session,
            "alertsdb_show",
            label = NULL,
            value = "true"
          )
          output$alertsdb_table <- DT::renderDataTable(
            get_alertsdb_html() %>%
              DT::datatable(
                colnames = c(
                  "Date" = "date",
                  "Topic" = "topic",
                  "Region" = "country",
                  "Top words" = "topwords",
                  "Posts" = "number_of_posts",
                  "Top posts" = "topposts",
                  "Given Category" = "given_category",
                  "Episomer Category" = "episomer_category"
                ),
                filter = "top",
                escape = FALSE
              )
          )

          output$alertsdb_runs_table <- DT::renderDataTable(
            get_alertsdb_runs_html() %>%
              DT::datatable(
                colnames = c(
                  "Ranking" = "ranking",
                  "Models" = "models",
                  "Alerts" = "alerts",
                  "Runs" = "runs",
                  "F1Score" = "f1score",
                  "Accuracy" = "accuracy",
                  "Precision By Class" = "precision_by_class",
                  "Sensitivity By Class" = "sensitivity_by_class",
                  "FScore By Class" = "fscore_by_class",
                  "Last run" = "last_run",
                  "Balance classes" = "balance_classes",
                  "Force to use" = "force_to_use",
                  "Active" = "active",
                  "Documentation" = "documentation",
                  "Custom Parameters" = "custom_parameters"
                ),
                filter = "top",
                escape = FALSE
              )
          )
        })
        shiny::observeEvent(input$alertsdb_close, {
          shiny::updateTextInput(
            session,
            "alertsdb_show",
            label = NULL,
            value = "false"
          )
        })
        output$alertsdb_download <- shiny::downloadHandler(
          filename = function() "alert-training.xlsx",
          content = function(file) {
            progress_start("updating geo training dataset for download")
            #updating geotraining file before downloading
            tryCatch(
              {
                if (
                  get_alert_training_path() == get_default_alert_training_path()
                ) {
                  alerts <- get_alerts(
                    topic = input$alerts_topics,
                    countries = as.numeric(input$alerts_countries),
                    from = input$alerts_period[[1]],
                    until = input$alerts_period[[2]],
                    topposts = 10,
                    limit = if (
                      !is.na(input$alerts_limit) &&
                        as.integer(input$alerts_limit) > 0
                    )
                      as.integer(input$alerts_limit) else 20,
                    progress = function(value, message) {
                      progress_set(value = value, message = message)
                    }
                  )
                  if (!is.null(alerts)) {
                    alerts$given_category <- NA
                    write_alert_training_db(alerts)
                    cd$alert_training_refresh_flag(Sys.time())
                    progress_close()
                    file.copy(get_alert_training_path(), file)
                  } else
                    app_error(
                      "Cannot download alerts since no alerts where found with the selected filters",
                      env = cd
                    )
                } else {
                  progress_close()
                  file.copy(get_alert_training_path(), file)
                }
              },
              error = function(w) {
                app_error(w, env = cd)
              }
            )
          }
        )

        shiny::observeEvent(input$alertsdb_add, {
          progress_start("Adding new alerts to the alert database")
          #updating geotraining file before downloading
          tryCatch(
            {
              new_alerts <- get_alerts(
                topic = input$alerts_topics,
                countries = as.numeric(input$alerts_countries),
                from = input$alerts_period[[1]],
                until = input$alerts_period[[2]],
                topposts = 10,
                limit = if (
                  !is.na(input$alerts_limit) && as.integer(input$alerts_limit) > 0
                )
                  as.integer(input$alerts_limit) else 20,
                progress = function(value, message) {
                  progress_set(value = value, message = message)
                }
              )

              existing_alerts <- get_alert_training_df()
              if (!is.null(new_alerts) && nrow(new_alerts) > 0) {
                new_alerts$given_category <- NA
                alerts <- Reduce(
                  x = list(existing_alerts, new_alerts),
                  f = function(df1, df2) {
                    dplyr::bind_rows(df1, df2)
                  }
                )
                write_alert_training_db(alerts)
                cd$alert_training_refresh_flag(Sys.time())
              } else {
                app_error(
                  "Cannot add alerts to classify. No alerts where found with the selected filters",
                  env = cd
                )
              }
            },
            error = function(w) {
              app_error(w, env = cd)
            }
          )
        })

        # uploading alerts training file
        shiny::observe({
          df <- input$alertsdb_upload
          if (!is.null(df) && nrow(df) > 0) {
            progress_start("processing new training set")
            uploaded <- df$datapath[[1]]
            #copying file and making a backup of existing one
            if (file.exists(get_user_alert_training_path()))
              file.copy(
                get_user_alert_training_path(),
                paste(get_user_alert_training_path(), ".bak", sep = ""),
                overwrite = TRUE
              )
            file.copy(uploaded, get_user_alert_training_path(), overwrite = TRUE)
            #updating geotraining df taking in condideration new uploaded file

            tryCatch(
              {
                retrain_alert_classifier(progress_set)
                cd$alert_training_refresh_flag(Sys.time())
              },
              warning = function(w) {
                app_error(w, env = cd)
              },
              error = function(w) {
                app_error(w, env = cd)
              }
            )
          }
        })

        # updating alerts table on change of geotraining file
        shiny::observe({
          # Adding a dependency to alert refresh
          cd$alert_training_refresh_flag()
          DT::replaceData(DT::dataTableProxy('alertsdb_table'), get_alertsdb_html())
          DT::replaceData(
            DT::dataTableProxy('alertsdb_runs_table'),
            get_alertsdb_runs_html()
          )
          progress_close()
        })

        ######### GEOTRAINING EVALUATION DATAFRAME ####
        # rendering a dataframe showing evaluation metrics
        output$geotraining_eval_df <- DT::renderDataTable({
          df <- get_geotraining_eval_df()
          DT::datatable(df, escape = TRUE)
        })

        get_geotraining_eval_df <- function() {
          `%>%` <- dplyr::`%>%`
          ret <- data.frame(
            Category = character(),
            `True positives` = integer(),
            `False positives` = integer(),
            `True negatives` = integer(),
            `False negatives` = integer(),
            `Precision` = double(),
            `Sensitivity` = double(),
            `F1Score` = double(),
            check.names = FALSE
          )
          if (file.exists(get_geotraining_evaluation_path())) {
            df <- jsonlite::fromJSON(
              get_geotraining_evaluation_path(),
              flatten = TRUE
            )
            df <- as.data.frame(df)
            if (nrow(df) > 0) {
              ret <- df %>%
                dplyr::select(
                  .data$test,
                  .data$tp,
                  .data$tn,
                  .data$fp,
                  .data$fn
                ) %>%
                dplyr::group_by(.data$test) %>%
                dplyr::summarise(
                  tp = sum(.data$tp),
                  fp = sum(.data$fp),
                  tn = sum(.data$tn),
                  fn = sum(.data$fn)
                ) %>%
                janitor::adorn_totals("row") %>%
                dplyr::mutate(
                  Precision = round(.data$tp / (.data$tp + .data$fp), 3)
                ) %>%
                dplyr::mutate(
                  Sensitivity = round(.data$tp / (.data$tp + .data$fn), 3)
                ) %>%
                dplyr::mutate(
                  F1Score = round(
                    (2 * .data$tp) / (2 * .data$tp + .data$fn + .data$fp),
                    3
                  )
                ) %>%
                dplyr::rename(Category = .data$test) %>%
                dplyr::rename("True positives" = .data$tp) %>%
                dplyr::rename("False positives" = .data$fp) %>%
                dplyr::rename("True negatives" = .data$tn) %>%
                dplyr::rename("False negatives" = .data$fn)
            }
          }
          ret
        }

        ######### GEOTEST LOGIC ##################
        # rendering geotest data, updates are done automatically whenever an input changes
        output$geotraining_table <- DT::renderDataTable({
          `%>%` <- magrittr::`%>%`
          df <- get_geotraining_df()
          df %>%
            DT::datatable(
              filter = "top",
              escape = TRUE
            )
        })
        # uploading geotagging training file
        shiny::observe({
          df <- input$geotraining_upload
          if (!is.null(df) && nrow(df) > 0) {
            progress_start("processing new training set")
            uploaded <- df$datapath[[1]]
            #copying file and making a backup of existing one
            if (file.exists(get_user_geotraining_path()))
              file.copy(
                get_user_geotraining_path(),
                paste(get_user_geotraining_path(), ".bak", sep = ""),
                overwrite = TRUE
              )
            file.copy(uploaded, get_user_geotraining_path(), overwrite = TRUE)
            #updating geotraining df taking in condideration new uploaded file

            tryCatch(
              {
                #update_geotraining_df(input$geotraining_posts2add, progress = function(value, message) {progress_set(value = value /3, message = message)})
                progress_set(
                  value = 0.5,
                  message = "Retraining models and evaluating"
                )
                retrain_languages()
                update_geotraining_df(
                  input$geotraining_posts2add,
                  progress = function(value, message) {
                    progress_set(value = 0.5 + value / 2, message = message)
                  }
                )
                cd$geotraining_refresh_flag(Sys.time())
                #TO DO : afficher les résultats + observe qui met à jour si de nouveelles données sont uploadées
              },
              error = function(w) {
                app_error(w, env = cd)
              }
            )
          }
        })
        # Updating geolocation data
        shiny::observeEvent(input$geotraining_update, {
          progress_start("updating geo training dataset")
          #updating geotraining df taking in condideration new uploaded file
          tryCatch(
            {
              update_geotraining_df(
                input$geotraining_posts2add,
                progress = function(value, message) {
                  progress_set(value = value, message = message)
                }
              )
              cd$geotraining_refresh_flag(Sys.time())
            },
            error = function(w) {
              app_error(w, env = cd)
            }
          )
        })

        # download geotraining data
        output$geotraining_download <- shiny::downloadHandler(
          filename = function() "geo-training.xlsx",
          content = function(file) {
            progress_start("updating geo training dataset for download")
            #updating geotraining file before downloading
            tryCatch(
              {
                if (get_geotraining_path() == get_default_geotraining_path()) {
                  update_geotraining_df(
                    input$geotraining_posts2add,
                    progress = function(value, message) {
                      progress_set(value = value, message = message)
                    }
                  )
                }
                file.copy(get_geotraining_path(), file)
                cd$geotraining_refresh_flag(Sys.time())
              },
              error = function(w) {
                app_error(w, env = cd)
              }
            )
          }
        )

        # updating geotraining table on change of geotraining file
        shiny::observe({
          # Adding a dependency to subscribers refresh
          cd$geotraining_refresh_flag()
          DT::replaceData(
            DT::dataTableProxy('geotraining_table'),
            get_geotraining_df()
          )
          DT::replaceData(
            DT::dataTableProxy('geotraining_eval_df'),
            get_geotraining_eval_df()
          )
          progress_close()
        })

        ######### DATA PROTECTION LOGIC ##################
        # rendering the data protection search table
        shiny::observeEvent(input$data_anonymise, {
          shiny::showModal(shiny::modalDialog(
            title = "Warning",
            "Please confirm you want to anonymise the posts matching this search criteria, this action cannot be undone",
            footer = shiny::tagList(
              shiny::actionButton("data_perform_anonymise", "Yes, anonymise posts"),
              shiny::modalButton("Cancel")
            )
          ))
        })

        shiny::observeEvent(input$data_delete, {
          shiny::showModal(shiny::modalDialog(
            title = "Warning",
            "Please confirm you want to delete the posts matching this search criteria, this action cannot be undone",
            footer = shiny::tagList(
              shiny::actionButton("data_perform_delete", "Yes, delete posts"),
              shiny::modalButton("Cancel")
            )
          ))
        })

        shiny::observeEvent(input$data_perform_delete, {
          search_posts_exp(hide_users = FALSE, action = "delete")
        })
        shiny::observeEvent(input$data_perform_anonymise, {
          search_posts_exp(hide_users = FALSE, action = "anonymise")
        })

        shiny::observeEvent(input$data_search_ano, {
          search_posts_exp(hide_users = TRUE)
        })

        shiny::observeEvent(input$data_search, {
          search_posts_exp(hide_users = FALSE)
        })
        search_posts_exp <- function(hide_users, action = NULL) {
          output$data_message <- shiny::renderText("")
          output$data_search_df <- DT::renderDataTable({
            `%>%` <- magrittr::`%>%`
            shiny::removeModal()
            query <- (if (!is.null(action) && action == "anonymise")
              "created_at:[0 TO Z] NOT user_name:user" else NULL)
            progress_start("Searching")
            shiny::isolate({
              posts <- search_posts(
                query = query,
                topic = input$data_topics,
                from = input$data_period[[1]],
                to = input$data_period[[2]],
                countries = as.numeric(input$data_countries),
                mentioning = if (
                  input$data_mode %in%
                    c("all", "mentioning") &&
                    input$data_users != ""
                )
                  strsplit(input$data_users, "\\s+")[[1]] else NULL,
                users = if (
                  input$data_mode %in% c("all", "users") && input$data_users != ""
                )
                  strsplit(input$data_users, "\\s+")[[1]] else NULL,
                hide_users = hide_users,
                action = action,
                max = as.integer(input$data_limit)
              )
            })

            if (!is.null(action) && nrow(posts) > 0) {
              processed <- 0
              while (nrow(posts) > 0) {
                processed <- processed + nrow(posts)
                progress_set(
                  value = 0.5,
                  message = paste(
                    "Performing",
                    action,
                    processed,
                    "posts processed"
                  )
                )

                shiny::isolate({
                  posts <- search_posts(
                    query = query,
                    topic = input$data_topics,
                    from = input$data_period[[1]],
                    to = input$data_period[[2]],
                    countries = as.numeric(input$data_countries),
                    mentioning = if (
                      input$data_mode %in%
                        c("all", "mentioning") &&
                        input$data_users != ""
                    )
                      strsplit(input$data_users, "\\s+")[[1]] else NULL,
                    users = if (
                      input$data_mode %in%
                        c("all", "users") &&
                        input$data_users != ""
                    )
                      strsplit(input$data_users, "\\s+")[[1]] else NULL,
                    hide_users = hide_users,
                    action = action,
                    max = as.integer(input$data_limit)
                  )
                })
              }
              progress_close()
            }

            shiny::validate(
              shiny::need(
                !is.null(posts) && nrow(posts) > 0,
                'No posts found for the provided filters'
              )
            )
            output$data_message <- if (posts$totalCount[[1]] > 1000) {
              shiny::renderText(paste(
                "more than ",
                posts$totalCount[[1]],
                " posts found"
              ))
            } else {
              shiny::renderText(paste(posts$totalCount[[1]], " posts found"))
            }

            dt <- {
              posts$country_code <- if ("text_loc" %in% colnames(posts))
                posts$text_loc$geo_country_code else ""
              posts$geo_name <- if ("text_loc" %in% colnames(posts))
                posts$text_loc$geo_name else ""
              cols <- c("id","topic","country_code","geo_name","created_at","text","user_name","quoted_text")
	      for(c in cols) {
	        if(!(c %in% names(posts)))
			posts[c] <- ""
	      }
	      posts %>%
                dplyr::select(cols) %>%
                DT::datatable(
                  colnames = c(
                    "Post Id" = "id",
                    "Topic" = "topic",
                    "Created" = "created_at",
                    "Country in text" = "country_code",
                    "Location in text" = "geo_name",
                    "User" = "user_name",
                    "Text" = "text",
                    "Repost/Quoted text" = "quoted_text"
                  ),
                  filter = "top",
                  escape = TRUE
                )
            }
            progress_close()
            dt
          })
        }

        ######## SNAPSHOT LOGIC ############
        # creating an snapshot of episomer data for backup or debuging purposes
        shiny::observeEvent(input$build_snapshot, {
          `%>%` <- magrittr::`%>%`
          progress_start("Creating snapshot")
          shiny::isolate({
            create_snapshot(
              destination_dir = input$snapshot_folder,
              types = input$snapshot_types,
              posts_period = input$snapshot_post_period,
              aggregated_period = input$snapshot_aggr_period,
              compress = input$snapshot_compress,
              progress = function(value, message) {
                progress_set(value = value, message = message)
              }
            )
          })
          progress_close()
        })

        ######## DIAGNOSTIC LOGIC ############
        # running the diagnostics when the button is clicked
        shiny::observeEvent(input$run_diagnostic, {
          `%>%` <- magrittr::`%>%`

          output$diagnostic_table <- DT::renderDataTable(
            check_all() %>%
              DT::datatable(
                colnames = c(
                  "Check Code" = "check",
                  "Passed" = "passed",
                  "Message" = "message"
                ),
                filter = "top",
                escape = TRUE,
                rownames = FALSE,
                options = list(pageLength = 50)
              ) %>%
              DT::formatStyle(
                columns = c("Passed"),
                valueColumns = c("Passed"),
                color = DT::styleEqual(c(TRUE, FALSE), c("green", "red"))
              )
          )
        })
      }
  }

  # Functions for managing calculation progress
  progress_start <- function(start_message = "processing", env = cd) {
    progress_close(env = env)
    env$progress <- shiny::Progress$new()
    env$progress$set(message = start_message, value = 0)
  }

  progress_close <- function(env = cd) {
    if (exists("progress", where = env) && !is.null(env$progress)) {
      env$progress$close()
      rm("progress", envir = env)
    }
  }

  progress_inc <- function(value, message = "processing", env = cd) {
    if (!is.null(env$progress$set))
      env$progress$set(
        value = env$progress$getValue() + value,
        detail = message
      )
  }

  progress_set <- function(value, message = "processing", env = cd) {
    if (!is.null(env$progress$set))
      env$progress$set(value = value, detail = message)
  }

  app_error <- function(e, env = cd) {
    message("000000000000000000000000000")
    message(class(e))
    message(paste(e))
    message("--------------------------------")

    message(e)
    message("11111111111111111111111111")
    progress_close(env = env)
    message("2222222222222222222222222222")
    shiny::showNotification(paste(e), type = "error")
    message("33333333333333333333333333333")
  }

  # Printing PID
  message(Sys.getpid())
  # Launching the app
  old <- options()
  on.exit(options(old))
  shiny::shinyApp(
    ui = ui_app(profile),
    server = function(input, output, session)
      server_app(input, output, session, profile),
    options = options(
      shiny.fullstacktrace = TRUE
    )
  )
}
#' @rdname episomer_app
#' @export
dashboard_app <- function(
  data_dir = NA,
  host = NULL,
  port = NULL
) {
  episomer_app(
    data_dir = data_dir,
    profile = "dashboard",
    host = host,
    port = port
  )
}

#' @rdname episomer_app
#' @export
admin_app <- function(data_dir = NA, host = NULL, port = NULL) {
  episomer_app(
    data_dir = data_dir,
    profile = "admin",
    host = host,
    port = port
  )
}

# Get base path depending on local or remote access
epipath <- function(...) {
    if(!fs_remote()) 
        file.path(conf$data_dir, ...)
    else 
        get_scala_file_url(...)
}
# to know if a path is remote or not
is_remote <- function(path) {
   p <- urltools::url_parse(path)
   if(is.na(p$scheme) || !p$scheme %in% c("http", "https"))
      return(FALSE)
   if(is.null(conf$fs_remote))
      stop("Cannot validate url agains fs_remote")
   else
      b <- urltools::url_parse(conf$fs_remote)
   if(((is.na(p$port) && is.na(b$port)) || b$port == p$port) &&
     ((is.na(p$scheme) && is.na(b$scheme)) || b$scheme == p$scheme) && 
     ((is.na(p$domain) && is.na(b$domain)) || b$domain == p$domain) &&
     !is.na(p$path) && startsWith(p$path, "file/"))
      return(TRUE)
   else
      return(FALSE)
}

to_remote_relative <- function(path) {
   if(!is_remote(path))
        stop("This function only work with remote episomer paths")
   p <- urltools::url_parse(path)
   gsub("^file", "", p$path)
}

# Destination path as the fs API expects it in the body of a move or a copy: relative to the
# epiHome of the fs machine, with no leading slash. The API rejects an absolute destination (and
# any ".."), so "http://host/file/runners/a.json" has to become "runners/a.json".
to_remote_dest <- function(path) {
   sub("^/", "", to_remote_relative(path))
}

to_remote_if_needed <- function(path) {
   if(fs_remote() && !is_remote(path)) {
     gsub(conf$data_dir, sprintf("%s/file/", conf$fs_remote), path)
   } else {
     path
   }
}

# Get path of properties files (written by Shiny app)
get_properties_path <- function() epipath("properties.json")

# Get path of search files (written by search loop)
get_search_path <- function() epipath("posts", "search")

# Get path of search files (written by search loop)
get_search_touch_path <- function() epipath("search.touched")

# Get path of geolocated files (written by detect loop)
get_geo_path <- function() epipath("posts", "geolocated")

# Get path of search files (written by search loop)
get_search_archive_path <- function()
  epipath("posts", "search_archive")

# Get path of plan base file (written by search loop)
get_plans_path <- function() epipath("topics.json")

# Get paths of plans by sm (written by search loop)
get_plans_paths <- function() {
  sms <- active_social_media()
  if (length(sms) > 0) {
    ret <- sms
    names(ret) = ret
    sapply(ret, function(sm) {
      gsub(".json", paste0(".", sm, ".json"), get_plans_path())
    })
  } else {
    character(0)
  }
}

# Get task path (written by detect pipeline)
get_tasks_path <- function() epipath("tasks.json")


# Get available alert training file path
get_alert_training_path <- function() {
  path <- get_user_alert_training_path()
  if (!epifile.exists(path)) path <- get_default_alert_training_path()
  path
}

# Get default alert training file path
get_default_alert_training_path <- function()
  system.file("extdata", "alert-training.xlsx", package = get_package_name())

# Get user alert training file
get_user_alert_training_path <- function() {
  epipath("alert-training.xlsx")
}

# Get available geotraining file path
get_geotraining_path <- function() {
  path <- get_user_geotraining_path()
  if (!epifile.exists(path)) path <- get_default_geotraining_path()
  path
}

# Get default languages file path
get_default_geotraining_path <- function()
  system.file("extdata", "geo-training.xlsx", package = get_package_name())

# Get user geo training file
get_user_geotraining_path <- function() {
  epipath("geo-training.xlsx")
}

# Get geo training evaluation file
get_geotraining_evaluation_path <- function() {
  epipath("geo-training-evaluation.json")
}
# Get default languages file path
get_default_available_languages_path <- function()
  system.file("extdata", "languages.xlsx", package = get_package_name())

# Get available languages file path (written by Shiny app)
get_available_languages_path <- function() {
  path <- epipath("languages.xlsx")
  if (!epifile.exists(path)) path <- get_default_available_languages_path()
  path
}

# Get default topics file path
get_default_known_users_path <- function()
  system.file("extdata", "users.xlsx", package = get_package_name())

# Get topics file path either from user or package location (written by Shiny app)
get_known_users_path <- function() {
  users_path <- epipath("users.xlsx")
  if (!epifile.exists(users_path)) users_path <- get_default_known_users_path()
  return(users_path)
}

# Get default topics files path
get_default_topics_path <- function()
  system.file("extdata", "topics.xlsx", package = get_package_name())

# Get topics file path either from user or package location (written by Shiny app)
get_user_topics_path <- function() {
  
  topics_path <- epipath("topics.xlsx")
  if (!epifile.exists(topics_path)) 
    topics_path <- get_default_topics_path()
  return(topics_path)
}

# Get default country files path
get_default_countries_path <- function()
  system.file("extdata", "countries.xlsx", package = get_package_name())

# Get countries file path either from user or package location (written by Shiny app)
get_countries_path <- function() {
  countries_path <- epipath("countries.xlsx")
  if (!epifile.exists(countries_path))
    countries_path <- get_default_countries_path()
  return(countries_path)
}

# Get default subscribers file path
get_default_subscribers_path <- function()
  system.file("extdata", "subscribers.xlsx", package = get_package_name())

# Get the path for default or user defined subscribed user file (written by Shiny app)
get_subscribers_path <- function() {
  path <- epipath("subscribers.xlsx")
  if (!epifile.exists(path)) path <- get_default_subscribers_path()
  path
}

# Get email template path
get_email_alert_template_path <- function()
  system.file("extdata", "mail.html", package = get_package_name())

# Get Scala Building Tools (SBT) dependencies file
get_sbt_file_dep_path <- function()
  system.file("extdata", "sbt-deps.txt", package = get_package_name())

# Get JAR directory
get_jars_dest_path <- function() epipath("jars")

# Directory where the embedded java bundle lives, split out from get_app_jar_path() so tests can
# mock it instead of base::system.file()
get_java_dir_path <- function() system.file("java", package = get_package_name())

# Get application JAR (embedded on package). The jar filename carries its own version
# (ecdc-episomer-bundle_2.13-<version>.jar, set from ../episomer/DESCRIPTION at sbt package
# time), so it is located by pattern rather than by a hardcoded name.
get_app_jar_path <- function() {
  java_dir <- get_java_dir_path()
  jars <- list.files(
    java_dir,
    pattern = "^ecdc-episomer-bundle_2\\.13-.*\\.jar$",
    full.names = TRUE
  )
  if (length(jars) == 0) {
    return("")
  }
  jars[[1]]
}

# Get hadoop home path for winutils
get_winutils_hadoop_home_path <- function() epipath("hadoop")

# Get winutils path
get_winutils_path <- function()
  file.path(get_winutils_hadoop_home_path(), "bin", "winutils.exe")

# Get JSON file name for alert on given date
get_alert_file <- function(date) {
  alert_folder <- epipath("alerts")
  if (!epifile.exists(alert_folder)) epidir.create(alert_folder)
  alert_folder <- file.path(alert_folder, strftime(date, format = "%Y"))
  if (!epifile.exists(alert_folder)) epidir.create(alert_folder)
  alert_file <- file.path(
    alert_folder,
    paste(strftime(date, format = "%Y.%m.%d"), "-alerts.json", sep = "")
  )
}

# Get geonames txt file
get_geonames_txt_path <- function() {
  epipath("geo", "allCountries.txt")
}

# Get geonames parquet path
get_geonames_parquet_path <- function(relative = FALSE) {
  if (!relative) epipath("geo", "all-geos.parquet") else
    file.path("geo", "all-geos.parquet")
}

# Get geonames cities parquet path
get_cities_parquet_path <- function(relative = FALSE) {
  if (!relative) epipath("geo", "all-cities.parquet") else
    file.path("geo", "all-cities.parquet")
}

# Get geonames index path
get_geonames_index_path <- function(relative = FALSE) {
  if (!relative) epipath("geo", "all-geos.parquet.index") else
    file.path("geo", "all-geos.parquet.index")
}

# Get path of the topics-keyword.json file used for ignoring topic in topwords
get_topic_keywords_path <- function() {
  geo_folder <- epipath("geo")
  if (!epifile.exists(geo_folder)) epidir.create(geo_folder)
  file.path(geo_folder, "topic-keywords.json")
}

# 
get_forced_geo_path <- function() {
  epipath("geo", "forced-geo.json")
}

get_forced_geo_codes_path <- function() {
  epipath("geo", "forced-geo-codes.json")
}

# Get languages index path
get_lang_index_path <- function(relative = FALSE) {
  if (!relative) epipath("geo", "lang_vectors.index") else
    file.path("geo", "lang_vectors.index")
}

# Get language vectors path
get_lang_vectors_path <- function(code) {
  epipath("languages", paste(code, ".txt.gz", sep = ""))
}

# Get language model path
get_lang_model_path <- function(code) {
  epipath("languages", paste(code, ".txt.gz.model", sep = ""))
}


# Get post collection path
get_post_togeo_path <- function() {
  epipath("geo", "togeolocate.json")
}

# Get path of the geolocating json file
get_post_geoing_path <- function() {
  epipath("geo", "geolocating.json")
}

# Get path of posts to aggregate
get_post_toaggr_path <- function() {
  epipath("geo", "toaggregate.json")
}
# Get path of posts being aggregated
get_post_aggring_path <- function() {
  epipath("geo", "aggregating.json")
}
# Get path of the session info log file
get_session_info_path <- function() {
  epipath("session-info.log")
}

# Runners-related paths

# Folder holding the live leases, one file per runner. Created on demand by the writers only:
# reading a lease happens far more often than writing one, and a reader must not create anything
# on the fs machine.
runner_lease_dir <- function(create = FALSE) {
  path <- epipath("runners")
  if (create && !epifile.exists(path)) {
    epidir.create(path, recursive = TRUE)
  }
  path
}

# Folder keeping the released leases (see release_runner_lease()). Nothing ever lists it: it is a
# trail an operator can read after the fact.
runner_lease_old_dir <- function(create = FALSE) {
  path <- epipath("runners", "old")
  if (create && !epifile.exists(path)) {
    epidir.create(path, recursive = TRUE)
  }
  path
}

# Path of the lease file of a runner, e.g. <data_dir>/runners/search.bluesky.json
runner_lease_path <- function(name, create_dir = FALSE) {
  check_runner_name(name)
  runner_lease_dir(create = create_dir)
  epipath("runners", paste0(name, ".json"))
}

# Path a lease is moved to when it is released. The instance_id is part of the name so that
# successive releases of the same runner do not overwrite each other.
#
# `instance_id` is a parameter rather than always conf$instance_id because a lease is not always
# released by its own holder: release_dead_local_runner_lease() (R/runners-lease.R) gives back the
# lease of a local process that has stopped, and the trail under runners/old/ has to keep naming
# the instance the file describes, not the one that swept it away.
#
# The identifier ends up in a file name and, in that one case, comes from a file rather than from
# new_instance_id(): anything outside the character set it generates is replaced instead of being
# pasted into a path.
runner_lease_released_path <- function(name, instance_id = conf$instance_id) {
  check_runner_name(name)
  instance_id <- gsub("[^A-Za-z0-9._-]", "_", paste(instance_id, collapse = ""))
  epipath("runners", "old", paste0(name, ".", instance_id, ".json"))
}

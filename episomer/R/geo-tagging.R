# Get the SQL-like expression to extract post geolocation variables and apply prioritisation
# This function is used on aggregate posts for translating variables names into SQL-valid columns of geotagged posts
get_location_var <- function(varname) {
  paste(
    "coalesce(",
    "text_loc.",
    varname,
    ", quoted_text_loc.",
    varname,
    ")",
    sep = ""
  )
}

# It gets columns used for post geolocation. This is used for limiting columns to extract from JSON files
get_location_columns <- function(table) {
  list(
    "text_loc",
    "quoted_text_loc"
  )
}

# Get raw countries data frame from Excel file
get_raw_countries <- function() {
  # obtained from https://gist.github.com/AdrianBinDC/621321d6083b176b3a3215c0b75f8146#file-country-bounding-boxes-md but it had been manually updated
  df <- epiread_excel(get_countries_path())
  names(df) <- make.names(names(df), unique = TRUE)
  df$minLat <- as.numeric(df$minLat)
  df$maxLat <- as.numeric(df$maxLat)
  df$minLong <- as.numeric(df$minLong)
  df$maxLong <- as.numeric(df$maxLong)
  df
}


# Getting a list of regions, sub regions and countries for use as a selection
get_country_items <- function(order = "level") {
  `%>%` <- magrittr::`%>%`
  #If countries are already on cache we return them otherwise we calculate them
  # One key, computed once: the lookup used sep = "" while the write at the end of this function
  # uses sep = "_", so the two never named the same entry and the cache never hit once. Every
  # caller - refresh_dashboard_data() on every dashboard filter change, get_regions_df(),
  # get_country_code_map() - paid a countries.xlsx read (an HTTP download when the fs is remote)
  # plus the quadratic region/country loops below. The `return()` was broken too: `[[` on an
  # environment takes a single name, so the extra arguments would have errored had it ever run.
  cache_key <- paste("country_items", order, sep = "_")
  if (exists(cache_key, where = cached)) {
    return(cached[[cache_key]])
  } else {
    # Getting country data from package embedded excel, ignoring Antarctica
    countries <- get_raw_countries() %>% dplyr::filter(.data$region != "")

    # using intermediate region when available
    countries$sub.region <- mapply(
      function(int, sub) {
        if (is.na(int)) sub else int
      },
      countries$intermediate.region,
      countries$sub.region
    )
    #getting regions and subregions
    regions <- (countries %>%
      dplyr::group_by(.data$region) %>%
      dplyr::summarize(
        minLat = min(.data$minLat),
        minLong = min(.data$minLong),
        maxLat = max(.data$maxLat),
        maxLong = max(.data$maxLong)
      ) %>%
      dplyr::ungroup())
    subregions <- (countries %>%
      dplyr::group_by(.data$region, .data$sub.region) %>%
      dplyr::summarize(
        minLat = min(.data$minLat),
        minLong = min(.data$minLong),
        maxLat = max(.data$maxLat),
        maxLong = max(.data$maxLong)
      ) %>%
      dplyr::ungroup())

    # creating list containing results
    items <- list()
    # adding hardcoded world items
    row <- 1
    items[[row]] <- list(
      name = "World (all)",
      codes = list(),
      level = 0,
      minLat = -90,
      maxLat = 90,
      minLong = -180,
      maxLong = 180
    )
    row <- 2
    items[[row]] <- list(
      name = "World (geolocated)",
      codes = as.list(countries$alpha.2),
      level = 0,
      minLat = -90,
      maxLat = 90,
      minLong = -180,
      maxLong = 180
    )
    for (r in 1:nrow(regions)) {
      row <- row + 1
      region <- regions$region[[r]]
      items[[row]] = list(
        name = region,
        codes = list(),
        level = 1,
        minLat = 90,
        maxLat = -90,
        minLong = 180,
        maxLong = -180
      )
      # filling region item
      for (c in 1:nrow(countries)) {
        if (countries$region[[c]] == region) {
          # adding country codes for region
          items[[row]]$codes[[
            length(items[[row]]$codes) + 1
          ]] <- countries$alpha.2[[c]]
          # calculating region bounding box
          if (countries$minLat[[c]] < items[[row]]$minLat)
            items[[row]]$minLat <- countries$minLat[[c]]
          if (countries$maxLat[[c]] > items[[row]]$maxLat)
            items[[row]]$maxLat <- countries$maxLat[[c]]
          if (countries$minLong[[c]] < items[[row]]$minLong)
            items[[row]]$minLong <- countries$minLong[[c]]
          if (countries$maxLong[[c]] > items[[row]]$maxLong)
            items[[row]]$maxLong <- countries$maxLong[[c]]
        }
      }
      # Adding elements for subregions
      for (s in 1:nrow(subregions)) {
        if (subregions$region[[s]] == region) {
          row <- row + 1
          subregion <- subregions$sub.region[[s]]
          items[[row]] = list(
            name = subregion,
            codes = list(),
            level = 2,
            minLat = 90,
            maxLat = -90,
            minLong = 180,
            maxLong = -180
          )
          # filling sub region item
          for (c in 1:nrow(countries)) {
            if (
              countries$region[[c]] == region &&
                countries$sub.region[[c]] == subregion
            ) {
              # adding country codes for region
              items[[row]]$codes[[
                length(items[[row]]$codes) + 1
              ]] <- countries$alpha.2[[c]]
              # calculating region bounding box
              if (countries$minLat[[c]] < items[[row]]$minLat)
                items[[row]]$minLat <- countries$minLat[[c]]
              if (countries$maxLat[[c]] > items[[row]]$maxLat)
                items[[row]]$maxLat <- countries$maxLat[[c]]
              if (countries$minLong[[c]] < items[[row]]$minLong)
                items[[row]]$minLong <- countries$minLong[[c]]
              if (countries$maxLong[[c]] > items[[row]]$maxLong)
                items[[row]]$maxLong <- countries$maxLong[[c]]
            }
          }
          # adding country items
          for (c in 1:nrow(countries)) {
            if (
              countries$region[[c]] == region &&
                countries$sub.region[[c]] == subregion
            ) {
              row <- row + 1
              country <- countries$name[[c]]
              items[[row]] <- list(
                name = country,
                codes = list(countries$alpha.2[[c]]),
                level = 3,
                minLat = countries$minLat[[c]],
                maxLat = countries$maxLat[[c]],
                minLong = countries$minLong[[c]],
                maxLong = countries$maxLong[[c]]
              )
            }
          }
        }
      }
    }

    # Getting EU classification
    eu_zones <- countries$EU.EEA[!is.na(countries$EU.EEA)] %>% unique
    eu_codes <- lapply(eu_zones, function(r) {
      list(
        name = r,
        codes = countries$alpha.2[
          !is.na(countries$EU.EEA) & countries$EU.EEA == r
        ],
        level = 0.5
      )
    })
    eu_codes <- (if (length(eu_codes) == 0) eu_codes else
      c(
        eu_codes,
        list(list(
          name = paste(eu_zones, collapse = "+"),
          codes = countries$alpha.2[!is.na(countries$EU.EEA)],
          level = 0.5
        ))
      ))

    # Getting WHO classification
    who_zones <- countries$WHO.Region[!is.na(countries$WHO.Region)] %>% unique
    who_codes <- lapply(who_zones, function(r) {
      list(
        name = r,
        codes = countries$alpha.2[
          !is.na(countries$WHO.Region) & countries$WHO.Region == r
        ],
        level = 0.6
      )
    })
    specials <- c(eu_codes, who_codes)
    for (s in 1:length(specials)) {
      row <- row + 1
      items[[row]] = list(
        name = specials[[s]]$name,
        codes = list(),
        level = specials[[s]]$level,
        minLat = 90,
        maxLat = -90,
        minLong = 180,
        maxLong = -180
      )
      for (c in 1:nrow(countries)) {
        if (countries$alpha.2[[c]] %in% specials[[s]]$codes) {
          # adding country codes for region
          items[[row]]$codes[[
            length(items[[row]]$codes) + 1
          ]] <- countries$alpha.2[[c]]
          # calculating region bounding box
          if (countries$minLat[[c]] < items[[row]]$minLat)
            items[[row]]$minLat <- countries$minLat[[c]]
          if (countries$maxLat[[c]] > items[[row]]$maxLat)
            items[[row]]$maxLat <- countries$maxLat[[c]]
          if (countries$minLong[[c]] < items[[row]]$minLong)
            items[[row]]$minLong <- countries$minLong[[c]]
          if (countries$maxLong[[c]] > items[[row]]$maxLong)
            items[[row]]$maxLong <- countries$maxLong[[c]]
        }
      }
    }
    # Items are natively ordered as a hierarchy, but default of this function is ordered by level and name
    if (order == "level")
      items <- items[order(sapply(items, function(i) {
        paste(i$level, i$name)
      }))]

    # Setting the cache
    cached[[cache_key]] <- items
    return(items)
  }
}

# Forgets the memoised country items, so that the next get_country_items() reparses the file.
# Called when countries.xlsx is replaced from the configuration page.
drop_country_items_cache <- function() {
  keys <- ls(cached, all.names = TRUE)
  keys <- keys[startsWith(keys, "country_items_")]
  if (length(keys) > 0) rm(list = keys, envir = cached)
  invisible(NULL)
}

# Get country codes
# usage
# map <- get_country_code_map()
# map[["FR"]]
get_country_code_map <- function() {
  regions <- get_country_items()
  countries <- regions[sapply(regions, function(i) i$level == 3)]
  return(setNames(
    sapply(countries, function(r) r$name),
    sapply(countries, function(r) r$code)
  ))
}

# Get country codes by name
# usage
# map <- get_country_codes_by_name()
# map[["Asia"]]
get_country_codes_by_name <- function() {
  regions <- get_country_items()
  return(setNames(
    sapply(regions, function(r) r$code),
    sapply(regions, function(r) r$name)
  ))
}

# Get regions data as data frame
get_regions_df <- function() {
  regions <- get_country_items()
  data.frame(
    Name = sapply(regions, function(r) r$name),
    Codes = sapply(regions, function(r) paste(r$codes, collapse = ", ")),
    Level = sapply(regions, function(r) r$level),
    MinLatitude = as.numeric(sapply(regions, function(r) r$minLat)),
    MaxLatitude = as.numeric(sapply(regions, function(r) r$maxLat)),
    MinLongitude = as.numeric(sapply(regions, function(r) r$minLong)),
    MaxLingityde = as.numeric(sapply(regions, function(r) r$maxLong))
  )
}

# Get country indexes
# usage
# map <- get_country_index_map()
# map[["FR"]]
get_country_index_map <- function() {
  regions <- get_country_items()
  all_index <- 1:length(regions)
  indexes <- all_index[sapply(all_index, function(i) regions[[i]]$level == 3)]
  return(setNames(indexes, sapply(indexes, function(i) regions[[i]]$code)))
}

#' @title Updates the local copy of the GeoNames database
#' @description Downloading and indexing a fresh version of the GeoNames database from the provided URL
#' @param download Logical, whether to download geonames database default: TRUE
#' @param assemble Logical, whether to calculate the geographical hierarchy to index: TRUE
#' @param index Logical, whether to write geographical location in full text index default: TRUE
#' @return The list of tasks updated with produced messages
#' @details Run a one shot task to download and index a local copy of the  \href{http://www.geonames.org/}{GeoNames database}.
#' The GeoNames geographical database covers all countries and contains over eleven million place names that are available; Creative Commons Attribution 4.0 License.
#'
#' The URL to download the database from is set on the configuration tab of the Shiny app, in case it changes.
#'
#' The indexing is developed in Spark and Lucene
#'
#' A prerequisite to this function is that the \code{\link{search_loop}} must already have stored collected posts in the search folder and that the task \code{\link{download_dependencies}}
#' has been successfully run.
#'
#' Normally this function is not called directly by the user but from the \code{\link{detect_loop}} function.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    # setting up the data folder
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'
#'    # geolocating last posts
#'    tasks <- update_geonames()
#' }
#' @rdname update_geonames
#' @seealso
#'  \code{\link{download_dependencies}}
#'
#'  \code{\link{detect_loop}}
#'
#'  \code{\link{get_tasks}}
#'
#' @export
#' @importFrom utils download.file unzip
update_geonames <- function(download=TRUE, assemble = TRUE, index = TRUE) {
  stop_if_no_config()
  old <- options()
  on.exit(options(old))
  options(timeout = 900)
  if(download) {
      message("Downloading geonames")
      if (!file.exists(file.path(conf$data_dir, "geo"))) {
        dir.create(file.path(conf$data_dir, "geo"))
      }
      # Downloading GeoNames
      temp <- tempfile()
      temp_dir <- tempdir()
      download.file(conf$geonames_url, temp, mode = "wb")
      
      message("Decompressing geonames")
      # Decompressing file
      unzip(zipfile = temp, files = "allCountries.txt", exdir = temp_dir)
      file.remove(temp)
      file.rename(
        from = file.path(temp_dir, "allCountries.txt"),
        to = get_geonames_txt_path()
      )
  }
  if(assemble) {
      # Assembling GeoNames datasets
      # using Spark job with entry point updateGeonames part 'ASSEMBLE'
      message("Assembling geonames")
      spark_job(
        paste(
          "updateGeonames",
          conf_geonames_as_arg(),
          "assemble",
          paste("\"TRUE\"", sep = ""),
          "index",
          paste("\"FALSE\"", sep = ""),
          "parallelism",
          conf$spark_cores
        )
      )
  }
  if(index) {
      # Indexing GeoNames datasets
      message("Indexing geonames")
      spark_job(
        paste(
          "updateGeonames",
          conf_geonames_as_arg(),
          "assemble",
          paste("\"FALSE\"", sep = ""),
          "index",
          paste("\"TRUE\"", sep = ""),
          "parallelism",
          conf$spark_cores
        )
      )

  }
}


#' @title Updates language index and retrain models
#' @description Indexing downloaded languages models and proceed to retrain based on manual or automatic annotations
#' @return Nothing
#' @details Index locally downloaded fasttext \href{https://fasttext.cc/docs/en/crawl-vectors.html}{pretrained models} and train entity detection models. 
#' A fasttext model is a collection of vectors for a language automatically produced scrolling a big corpus of text that can be used to capture the semantic of a word.
#'
#' The URL to download the vectors from are set on the configuration tab of the Shiny app.
#'
#' This task will also update SVM models to predict whether a word is a location that will be used in the geolocation process.
#'
#' The indexing is developed in SPARK and Lucene.
#'
#' Normally this function is not called directly by the user but from the \code{\link{setup}} function.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    # setting up the data folder
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'
#'    # updating language tasks
#'    tasks <- update_languages()
#' }
#' @rdname update_languages
#' @seealso
#'  \code{\link{download_dependencies}}
#'
#'  \code{\link{update_geonames}}
#'
#'  \code{\link{detect_loop}}
#'
#'  \code{\link{get_tasks}}
#'
#' @export
update_languages <- function() {
  stop_if_no_config()
  old <- options()
  on.exit(options(old))
  options(timeout = 900)
  
  message("Getting annotations")
  update_geotraining_df()
  
  message("Indexing and retraining languages") 
  retrain_languages()

  message("updating annotations")
  update_geotraining_df()
  
  # touching the geotraining_evaluation_path after the update of the geotraining dataset to allow proper change detection of training set in setup
  if(file.exists(get_geotraining_evaluation_path()))
      Sys.setFileTime(get_geotraining_evaluation_path(), Sys.time())
  
}

download_big_file <- function(url, dest) {
    old <- options()
    on.exit(options(old))
    options(timeout = 900)
    temp <- sprintf("%s.tmp", dest)
    # downloading the file
    tryCatch2 <- function(exp, handler) {tryCatch(exp, error=handler, warning=handler)}
    tryCatch2(download.file(url, temp, mode = "wb"), handler = function(e) stop(e))
    tryCatch2({
            file.rename(from = file.path(temp), to = file.path(dest))
        },
        handler = function(e) {
            message(sprintf("Failed while trying to rename %s to %s with error : %s", file.path(temp), file.path(dest), e))
            message("going try to copy the file")
                    if(file.copy(from = file.path(temp), to = file.path(dest), overwrite=T)) { 
        	        message("Copy of file succeeded, going to delete origin file")
        	        file.remove(temp)
        	    } else {
                        stop("copy failed silently")
        	    }
        }
    )
}

#' @importFrom utils download.file
remove_language_files <- function(code) {
    base <- get_lang_vectors_path(code)
    to_remove <- list(
        base,
        sprintf("%s.txt", base),
        sprintf("%s.model.I.post-texts", base),
        sprintf("%s.model.I.txt", base),
        sprintf("%s.model.I", base),
        sprintf("%s.model.I.post-texts.txt", base)
    )
    for(f in to_remove)
        if(file.exists(f))
            unlink(f, recursive = TRUE)
}


#' @title geolocate text in a data frame given a text column and optionally a language column
#' @description extracts geolocaion information on text on a column of the provided data frame and returns a new data frame with geolocation information
#' @param df A data frame containing at least character column with text, a column with the language name can be provided to improve geolocation quality
#' @param text_col character, name of the column on the data frame containing the text to geolocalize, default:text
#' @param lang_col character, name of the column on the data frame containing the language of texts, default: NULL
#' @param min_score, numeric, the minimum score obtained on the Lucene scoring function to accept matches on GeoNames. It has to be empirically set default: NULL
#' @return A new data frame containing the following geolocation columns: geo_code, geo_country_code, geo_country, geo_name, tags
#' @details This function perform a call to the episomer database which includes functionality for geolocating for languages activated and successfully processed on the shiny app.
#'
#' The geolocation process tries to find the best match in GeoNames database \url{https://www.geonames.org/} including all local aliases for words.
#'
#' If no language is associated to the text, all tokens will be sent as a query to the indexed GeoNames database.
#'
#' If a language code is associated to the text and this language is trained on episomer, entity recognition techniques will be used to identify the best candidate in text to contain a location
#' and only these tokens will be sent to the GeoNames query.
#'
#' A custom scoring function is implemented to grant more weight to cities increasing with population to try to perform disambiguation.
#'
#' Rules for forcing the geolocation choices of the algorithms and for tuning performance with manual annotations can be performed on the geotag tab of the Shiny app.
#'
#' A prerequisite to this function is that the tasks \code{\link{download_dependencies}} \code{\link{update_geonames}} and \code{\link{update_languages}} has been run successfully.
#'
#' This function is called from the Shiny app on geolocation evaluation tab but can also be used for manually evaluating the episomer geolocation algorithm.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    # setting up the data folder
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'
#'    # creating a test dataframe
#'    df <- data.frame(text = c("Me gusta Santiago de Chile es una linda ciudad"), lang = c("es"))
#'    geo <- geolocate_text(df = df, text_col = "text", lang_col="lang")
#'
#' }
#' @rdname geolocate_text
#' @seealso
#'  \code{\link{download_dependencies}}
#'
#'  \code{\link{update_geonames}}
#'
#'  \code{\link{detect_loop}}
#'
#' @export
#' @importFrom utils download.file
geolocate_text <- function(
  df,
  text_col = "text",
  lang_col = NULL,
  min_score = NULL
) {
  stop_if_no_config()
  stop_if_no_fs()

  `%>%` <- magrittr::`%>%`
  if (is.null(lang_col)) df[[lang_col]] <- NA
  to_geolocate = df %>%
    dplyr::transmute(
      id = as.character(dplyr::row_number()),
      text = .data[[text_col]],
      lang = .data[[lang_col]]
    ) %>%
    jsonlite::toJSON()
  geo_uri = paste(get_scala_geolocate_text_url(), "?jsonnl=true", sep = "")
  if (!is.null(min_score)) {
    geo_uri <- paste(geo_uri, "&minScore=", min_score, sep = "")
  }
  ret = stream_post(uri = geo_uri, body = to_geolocate) %>%
    dplyr::arrange(as.integer(.data$id))
  for (col in c(
    "geo_code",
    "geo_country_code",
    "geo_country",
    "geo_name",
    "tags"
  ))
    if (!exists(col, where = ret)) ret[[col]] <- NA
  dplyr::select(ret, -which(names(ret) %in% c("text", "id", "lang")))
}

# returns the current geotraining annotations as a data frame
get_geotraining_df <- function() {
  `%>%` <- magrittr::`%>%`
  data_types <- c(
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text",
    "text"
  )
  current <- epiread_excel(get_geotraining_path(), col_types = data_types)
  current$Text <- stringr::str_trim(current$Text)
  current <- current %>%
    dplyr::distinct(
      .data$`Text`,
      .data$Lang,
      .data$`Location in text`,
      .keep_all = T
    )
  current
}

# returns the current geotraining annotation augmented with posts_to_add posts to annotate
updated_geotraining_df <- function(
  posts_to_add = 100,
  progress = function(a, b) {
  }
) {
  `%>%` <- magrittr::`%>%`
  progress(0.1, "getting current training file")
  current <- get_geotraining_df()
  locations <- current %>%
    dplyr::filter(
      .data$Type == "Location" & .data$`Location yes/no` %in% c("yes", "OK")
    )
  add_locations <- if (nrow(locations) == 0) "&locationSamples=true" else
    "&locationSamples=false"
  non_locations <- current %>%
    dplyr::filter(
      .data$Type == "Location" & .data$`Location yes/no` %in% c("KO", "no")
    )
  non_location_langs <- unique(non_locations$Lang)
  excl_langs <- if (length(non_location_langs) > 0)
    paste(
      "&excludedLangs=",
      paste(non_location_langs, collapse = "&excludedLangs="),
      sep = ""
    ) else ""

  progress(0.3, "obtaining locations from downloaded models and geonames")
  geo_training_uri <- paste(
    get_scala_geotraining_url(),
    "?jsonnl=true",
    add_locations,
    excl_langs,
    sep = ""
  )
  geo_training_url <- url(geo_training_uri)
  geo_training <- jsonlite::stream_in(geo_training_url)
  if (nrow(geo_training) > 0) {
    geo_training <- geo_training %>%
      dplyr::transmute(
        Type = "Location",
        `Text` = .data$word,
        `Location in text` = as.character(NA),
        `Location yes/no` = ifelse(.data$isLocation, "yes", "no"),
        `Associate country code` =  as.character(NA),
        `Associate with` =  as.character(NA),
        `Source` = "Episomer model",
        `Post Id` =  as.character(NA),
        `Lang` = .data$lang,
        `Post part` =  as.character(NA),
        `Episomer match` =  as.character(NA),
        `Episomer country match` =  as.character(NA),
        `Episomer country code match` =  as.character(NA)
      )
    geo_training$Text <- stringr::str_trim(geo_training$Text)
    geo_training <- geo_training %>%
      dplyr::distinct(.data$`Text`, .data$Lang, .keep_all = T)
  }
  # Adding new posts
  untagged <- unique(length(
    (current %>%
      dplyr::filter(
        .data$`Post part` == 'text' & .data$`Location yes/no` == '?'
      ))$`Post Id`
  ))
  to_add <- if (posts_to_add > untagged) posts_to_add else 0

  progress(0.6, "adding new posts")
  to_tag <- search_posts(
    query = paste(
      "lang:",
      lapply(conf$languages, function(l) l$code),
      collapse = " ",
      sep = ""
    ),
    max = to_add
  )
  if (nrow(to_tag) > 0) {
    to_tag$text <- stringr::str_trim(to_tag$text)
    texts <- to_tag %>%
      dplyr::transmute(
        Type = "Text",
        `Text` = .data$text,
        `Location in text` = as.character(NA),
        `Location yes/no` = "?",
        `Associate country code` =  as.character(NA),
        `Associate with` =  as.character(NA),
        `Source` = "Post",
        `Post Id` = .data$id,
        `Lang` = .data$lang,
        `Post part` = "text",
        `Episomer match` =  as.character(NA),
        `Episomer country match` =  as.character(NA),
        `Episomer country code match` =  as.character(NA)
      ) %>%
      dplyr::filter(!.data$`Text` %in% current$`Text`) %>%
      dplyr::distinct(.data$`Text`, .data$Lang, .keep_all = T)

    ret <- jsonlite::rbind_pages(list(
      current,
      geo_training,
      texts
    ))
  } else {
    ret <- jsonlite::rbind_pages(list(
      current,
      geo_training
    ))
  }

  ret <- ret %>%
    dplyr::filter(!is.na(.data$Text) & !.data$Text == "") %>%
    dplyr::distinct(.data$`Text`, .data$Lang, .keep_all = TRUE)

  text_togeo <- ret %>%
    dplyr::transmute(
      Text = ifelse(
        !is.na(.data$`Associate with`),
        .data$`Associate with`,
        .data$Text
      ),
      Lang = ifelse(!is.na(.data$`Associate with`), "all", .data$Lang)
    )
  #getting current geolocation evaluation
  progress(0.7, "geolocating all items")
  tryCatch(
    {
      geoloc = geolocate_text(
        df = text_togeo,
        text_col = "Text",
        lang_col = "Lang"
      )
      ret$`Episomer match` <- geoloc$geo_name
      ret$`Episomer country match` <- geoloc$geo_country
      ret$`Episomer country code match` <- geoloc$geo_country_code
      ret$`Location in text` <- ifelse(
        (is.na(ret$`Location yes/no`) | ret$`Location yes/no` == "?") &
          ret$Type == "Text",
        geoloc$tags,
        ret$`Location in text`
      )
    },
    error = function(e) {
      message(
        "Models are not trained, getting geotraining dataset without evaluation"
      )
      ret$`Episomer match` <- NA
      ret$`Episomer country match` <- NA
      ret$`Episomer country code match` <- NA
    }
  )
  ret %>% arrange(dplyr::desc(.data$Type), .data$`Post part`)
}

# returns the current geotraining annotation augmented with posts_to_add posts to annotate and write the results to the geotraining spreadsheet
update_geotraining_df <- function(
  posts_to_add = 100,
  progress = function(a, b) {
  }
) {
  `%>%` <- magrittr::`%>%`
  training_df <- updated_geotraining_df(
    posts_to_add = 100,
    progress = progress
  )
  update_topic_keywords()
  update_forced_geo()
  update_forced_geo_codes()
  if (is.function(progress)) progress(0.9, "writing result file")
  wbk <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wbk, "geolocation")
  # writing data to the worksheet
  openxlsx::writeDataTable(
    wbk,
    sheet = "geolocation",
    training_df,
    colNames = TRUE,
    startRow = 1,
    startCol = "A"
  )
  # setting some minimal formatting
  openxlsx::setColWidths(wbk, "geolocation", cols = c(2), widths = c(70))
  openxlsx::setColWidths(
    wbk,
    "geolocation",
    cols = c(3, 11, 12, 13),
    widths = c(25)
  )
  openxlsx::setColWidths(
    wbk,
    "geolocation",
    cols = c(4, 5, 6, 10),
    widths = c(17)
  )
  openxlsx::setRowHeights(wbk, "geolocation", rows = 1, heights = 20)
  openxlsx::addStyle(
    wbk,
    sheet = "geolocation",
    style = openxlsx::createStyle(
      fontSize = 10,
      halign = "center",
      fgFill = "#ff860d",
      border = c("top", "bottom", "left", "right"),
      textDecoration = "bold",
      wrapText = T,
      fontColour = "#222222"
    ),
    rows = 1,
    cols = 1,
    gridExpand = FALSE
  )
  openxlsx::addStyle(
    wbk,
    sheet = "geolocation",
    style = openxlsx::createStyle(
      fontSize = 10,
      halign = "center",
      fgFill = "#ffde59",
      border = c("top", "bottom", "left", "right"),
      textDecoration = "bold",
      wrapText = T,
      fontColour = "#222222"
    ),
    rows = 1,
    cols = 2:6,
    gridExpand = FALSE
  )
  openxlsx::addStyle(
    wbk,
    sheet = "geolocation",
    style = openxlsx::createStyle(
      fontSize = 10,
      halign = "center",
      fgFill = "#f7d1d5",
      border = c("top", "bottom", "left", "right"),
      textDecoration = "bold",
      wrapText = T,
      fontColour = "#222222"
    ),
    rows = 1,
    cols = 7:10,
    gridExpand = FALSE
  )
  openxlsx::addStyle(
    wbk,
    sheet = "geolocation",
    style = openxlsx::createStyle(
      fontSize = 10,
      halign = "center",
      fgFill = "#dedce6",
      border = c("top", "bottom", "left", "right"),
      textDecoration = "bold",
      wrapText = T,
      fontColour = "#222222"
    ),
    rows = 1,
    cols = 11:13,
    gridExpand = FALSE
  )
  openxlsx::addStyle(
    wbk,
    sheet = "geolocation",
    style = openxlsx::createStyle(
      fontSize = 10,
      border = c("top", "bottom", "left", "right")
    ),
    rows = 1:nrow(training_df) + 1,
    cols = 1:13,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wbk,
    sheet = "geolocation",
    style = openxlsx::createStyle(
      fontSize = 10,
      border = c("top", "bottom", "left", "right"),
      wrapText = T
    ),
    rows = 2:nrow(training_df) + 1,
    cols = 2,
    gridExpand = TRUE
  )
  openxlsx::freezePane(wbk, "geolocation", firstActiveRow = 2)
  tfile = tempfile("episomerxlsx")
  openxlsx::saveWorkbook(wbk, file = tfile, overwrite = TRUE)
  epifile.copy(tfile, get_user_geotraining_path())
  unlink(tfile)
}

# retrain the language classifiers for entity recognition with the current set of annotations
retrain_languages <- function() {
  `%>%` <- magrittr::`%>%`
  body <- get_geotraining_df() %>% jsonlite::toJSON()
  # This POST holds the connection open for as long as the JVM takes to index and retrain every
  # language, and the JVM sends nothing at all in the meantime. Two libcurl bounds have to be set
  # for that to be allowed, and neither of them is options(timeout = ):
  #
  #  * options(timeout = ) is a base R setting, read by download.file() and url() and by nothing
  #    else. httr goes through libcurl, which never looks at it. It used to be set to 1800 here,
  #    which did nothing whatsoever - verified by measurement: with options(timeout = 8) an httr
  #    request to a server that accepts and stays silent runs for well over two minutes;
  #  * libcurl's own default is a low-speed abort - less than 1 byte a second for 600 seconds - so
  #    a retraining that takes more than ten minutes died on
  #        Operation too slow. Less than 1 bytes/sec transferred the last 600 seconds
  #    with the training already running in the JVM, and, in a container, an Rscript that exited
  #    non-zero and a service that restarted on top of its own log. low_speed_limit = 0 turns that
  #    check off, which is what makes a silent connection legitimate.
  #
  # conf$fs_batch_timeout is then the single bound, and it is the right one: it is what the Scala
  # side gives itself for a batch of exactly this kind (Settings.fsBatchTimeout), so the client
  # waiting less than the server allows could only ever be wrong.
  post_result <- httr::POST(
    url = get_scala_geotraining_url(),
    httr::content_type_json(),
    httr::config(timeout = conf$fs_batch_timeout, low_speed_limit = 0),
    body = body,
    encode = "raw",
    encoding = "UTF-8"
  )
  if (httr::status_code(post_result) != 200) {
    stop(paste(
      "retrain web service failed with the following output: ",
      substring(httr::content(post_result, "text", encoding = "UTF-8"), 1, 100),
      sep = "\n"
    ))
  } else {
    fileConn <- file(get_geotraining_evaluation_path(), encoding = "UTF-8")
    writeLines(httr::content(post_result, "text", encoding = "UTF-8"), fileConn)
    close(fileConn)
    log_geotagging_models()
  }
}

# Print, for each language, the date of the entity detection model currently on disk. Called right
# after a retraining, it says which model files that retraining actually produced; the database logs
# then say which of them a geolocation picked up ("Loading geolocation model ..." in the JVM log,
# scala/src/main/scala/geo/GeoTraining.scala).
#
# The date printed here is what the geolocation of collected posts compares itself against: it
# builds its pipeline once and holds the models it loaded then, so it rebuilds that pipeline as soon
# as these files are newer (LuceneActor.geolocatePosts). The database log then says which of the two
# happened, on its next batch of posts, without anything having to be restarted.
log_geotagging_models <- function() {
  for (lang in conf$languages) {
    model_path <- paste(get_lang_model_path(lang$code), "I", sep = ".")
    if (!file.exists(model_path)) {
      message(sprintf("No entity detection model for %s in %s", lang$code, model_path))
      next
    }
    # The model is a Spark ML directory: its own timestamp is not always the one of the files that
    # were rewritten inside it, so the most recent file it contains is what dates the training.
    trained_on <- suppressWarnings(max(file.mtime(c(
      model_path,
      list.files(model_path, recursive = TRUE, full.names = TRUE)
    ))))
    message(sprintf(
      "Entity detection model for %s trained on %s (%s)",
      lang$code,
      strftime(trained_on, "%Y-%m-%d %H:%M:%S"),
      model_path
    ))
  }
}

# update the topic keywords JSON file. When several location entities are found, the one closest to a topic is chosen
update_topic_keywords <- function() {
  `%>%` <- magrittr::`%>%`
  keywords <- lapply(
    strsplit(
      gsub(
        "\\-\\w*\\b|\"",
        "",
        unlist(lapply(conf$topics, function(t) t$query))
      ),
      "OR|AND|NOT|\\(|\\)"
    ),
    function(t) {
      x <- gsub("^ *| *$", "", t)
      x[x != ""]
    }
  )
  topicKeywords <- setNames(keywords, sapply(conf$topics, function(t) t$topic))
  ret <- list()
  for (i in 1:length(conf$topics))
    if (exists(conf$topics[[i]]$topic, where = ret))
      ret[[conf$topics[[i]]$topic]] <- unique(c(
        ret[[conf$topics[[i]]$topic]],
        topicKeywords[[i]]
      )) else ret[[conf$topics[[i]]$topic]] <- unique(topicKeywords[[i]])
  write_json_atomic(
    ret,
    get_topic_keywords_path(),
    pretty = TRUE,
    force = TRUE,
    auto_unbox = TRUE
  )
}

# update the forced geo JSON file, listing words that will be associated with particular location names, ignoring the geolocation algorithm
update_forced_geo <- function() {
  `%>%` <- magrittr::`%>%`
  df <- get_geotraining_df() %>%
    dplyr::transmute(
      from = ifelse(
        is.na(.data$`Location in text`),
        .data$`Text`,
        .data$`Location in text`
      ),
      to = .data$`Associate with`
    ) %>%
    dplyr::filter(!is.na(.data$to))
  ret <- list()
  if (nrow(df) > 0) {
    for (i in 1:nrow(df)) {
      toReplace <- sapply(strsplit(df$from[[i]], ",")[[1]], function(t) {
        x <- gsub("^ *| *$", "", t)
        x[x != ""]
      })
      for (j in 1:length(toReplace)) ret[[toReplace[[j]]]] <- df$to[[i]]
    }
    write_json_atomic(
      ret,
      get_forced_geo_path(),
      pretty = TRUE,
      force = TRUE,
      auto_unbox = TRUE
    )
  } else {
    if (epifile.exists(get_forced_geo_path())) epifile.remove(get_forced_geo_path())
  }
}

# update the forced geo codes JSON file lisiting words that will be associated to particular location codes ignoring the geolocation algorithm
update_forced_geo_codes <- function() {
  `%>%` <- magrittr::`%>%`
  df <- get_geotraining_df() %>%
    dplyr::transmute(
      from = ifelse(
        is.na(.data$`Location in text`),
        .data$`Text`,
        .data$`Location in text`
      ),
      to = .data$`Associate country code`
    ) %>%
    dplyr::filter(!is.na(.data$to))
  ret <- list()
  if (nrow(df) > 0) {
    for (i in 1:nrow(df)) {
      toReplace <- sapply(strsplit(df$from[[i]], ",")[[1]], function(t) {
        x <- gsub("^ *| *$", "", t)
        x[x != ""]
      })
      for (j in 1:length(toReplace)) ret[[toReplace[[j]]]] <- df$to[[i]]
    }
    write_json_atomic(
      ret,
      get_forced_geo_codes_path(),
      pretty = TRUE,
      force = TRUE,
      auto_unbox = TRUE
    )
  } else {
    if (epifile.exists(get_forced_geo_codes_path()))
      epifile.remove(get_forced_geo_codes_path())
  }
}

# check whether languages need retrain
get_lang_traing_need <- function() {
    conf_langs <- unlist(lapply(conf$languages, `[[`, "code"))
    existing_vectors <- conf_langs[file.exists(unlist(lapply(conf$languages, `[[`, "vectors")))]
    # If there are some vectors not downloaded then retraining is needed
    if(length(existing_vectors) < length(conf_langs) )
        return(TRUE)
    # If the training set is not yet produced then training is needed
    if(!file.exists(get_geotraining_path()))
        return(TRUE)
    # If the evaluation of models has not been performed then training is needed
    if(!file.exists(get_geotraining_evaluation_path()))
        return(TRUE)
    # If date of update of vectors or training set is later than the evaluation of models then training is needed
    last_vectors_update <- max(file.mtime(sapply(existing_vectors, function(code) get_lang_vectors_path(code))))
    last_evaluation <- file.mtime(get_geotraining_evaluation_path())
    last_training_update <-file.mtime(get_geotraining_path())
    if(max(last_vectors_update, last_training_update) > max(last_evaluation))
        return(TRUE)
    return(FALSE)
}

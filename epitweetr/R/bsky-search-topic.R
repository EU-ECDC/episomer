bsky_search_topic <- function(
  plan,
  query,
  topic,
  output_in_scala = FALSE,
  conf
) {
  #, conf
  token <- bsky_get_token()
  # Set date boundaries for the search
  boundaries <- bsky_set_date_boundaries(plan)
  plan <- boundaries$plan
  max_text <- boundaries$max_text
  min_text <- boundaries$min_text

  msg(paste(
    "searching for topic",
    topic,
    "from",
    min_text,
    "until",
    max_text
  ))

  # Tweets are stored on the following folder structure data_folder/tweets/search/topic/year
  # Ensuring that folders for storing tweets are created
  year <- format(Sys.time(), "%Y")
  create_dirs(topic, year, conf) #, conf

  # Tweets are stored as gzipped files with the following naming: "YYYY.MM.DD.counter.json.gz"
  # Counter starts with 00001 and it is increased after the last file grows over 100M
  # getting prefix and regular expression for tweet archive name
  file_prefix <- paste(format(Sys.time(), "%Y.%m.%d"))
  file_pattern <- paste(format(Sys.time(), "%Y\\.%m\\.%d"))
  # TODO: update folder name
  dir <- paste(conf$data_dir, "tweets", "search", topic, year, sep = "/")

  # files will contain all files matching the naming pattern the last alphabetically is going to be measured to evaluate if a new file has to be started
  files <- sort(list.files(path = dir, pattern = file_prefix))

  # file_name will contain the name of the gz file to add
  file_name <- (if (length(files) == 0) {
    # default case for first file
    paste(
      file_prefix,
      formatC(1, width = 5, format = "d", flag = "0"),
      "json.gz",
      sep = "."
    )
  } else {
    #If last file matching pattern is smaller than 100MB we keep adding to the same file else a new incremented file is created
    last <- files[[length(files)]]
    if (file.info(paste(dir, last, sep = "/"))$size / (1024 * 1024) < 100) {
      last
    } else {
      #Try to get current index after date as integer and increasing it by one, if not possible a 00001 index will be added
      parts <- strsplit(gsub(".json.gz", "", last), split = "\\.")[[
        1
      ]]
      if (
        length(parts) <= 3 ||
          is.na(as.integer(parts[[length(parts)]]))
      ) {
        paste(
          c(
            parts,
            formatC(1, width = 5, format = "d", flag = "0"),
            "json.gz"
          ),
          collapse = "."
        )
      } else {
        paste(
          c(
            parts[1:length(parts) - 1],
            formatC(
              as.integer(parts[[length(parts)]]) + 1,
              width = 5,
              format = "d",
              flag = "0"
            ),
            "json.gz"
          ),
          collapse = "."
        )
      }
    }
  })

  # putting all parts together to get current file name
  # TODO: update folder name
  dest <- paste(
    conf$data_dir,
    "tweets",
    "search",
    topic,
    year,
    file_name,
    sep = "/"
  )

  # Ensuring that query is smaller than 400 character (Twitter API limit)
  #if (nchar(query) < 400) {
  # doing the tweet search and storing the response object to obtain details on resp
  content <- bsky_search(
    keyword = query,
    access_jwt = token,
    since = plan$research_min_date,
    until = plan$research_max_date
  )
  json <- content$results

  if (output_in_scala) {
    # Interpreting the content as JSON and storing the results on json (nested list with dataframes)
    # interpreting is necessary to know the number of obtained tweets and the id of the oldest tweet found and to keep tweet collecting stats
    # Saving uninterpreted content as a gzip archive
    tries <- 3
    done <- FALSE
    while (!done) {
      tries <- tries - 1
      tryCatch(
        {
          post_result <- httr::POST(
            url = paste0(
              get_scala_tweets_url(),
              "?topic=",
              curl::curl_escape(topic),
              "&geolocate=true"
            ),
            httr::content_type_json(),
            body = content$results,
            encode = "raw",
            encoding = "UTF-8",
            httr::timeout((4 - tries) * 5)
          )
          if (httr::status_code(post_result) != 200) {
            print(substring(
              httr::content(
                post_result,
                "text",
                encoding = "UTF-8"
              ),
              1,
              100
            ))
            stop()
          }
          done = TRUE
        },
        error = function(e) {
          msg(paste("Error found while sending tweets", e))
          if (tries < 0) {
            stop("too many retries")
          }
        }
      )
    }
  }

  # evaluating if rows are obtained
  got_rows <- (exists("posts", json) & length(json$posts) > 0)
  if (got_rows) {
    year <- format(Sys.time(), "%Y")
    first_date <- bsky_extract_many_posts_created_at(json$posts) %>%
      unlist() %>%
      min()
    last_date <- bsky_extract_many_posts_created_at(json$posts) %>%
      unlist() %>%
      max()
    # If rows were obtained we update the stat file that will stored the posted date period of each gz archive.
    # This is used to improve aggregating performance, by targeting only the files containing tweets for a particular date
    update_file_stats(
      filename = gsub(".gz", "", file_name),
      topic = topic,
      year = year,
      first_date = strptime(
        first_date,
        format = "%Y-%m-%dT%H:%M:%OS",
        tz = "UTC"
      ),
      last_date = strptime(
        last_date,
        format = "%Y-%m-%dT%H:%M:%OS",
        tz = "UTC"
      ),
      conf
    )
    plan <- bsky_update_plan(plan, content)
  } else {
    plan$has_more <- FALSE
    plan$end_on <- Sys.time()
    # ? plan$request <- 0
  }
  # else {
  # Managing the case when the query is too long
  #     warning(
  #         paste("Query too long for API for topic", topic),
  #         immediate. = TRUE
  #     )
  #     plan$requests = plan$requests
  #     return(plan)
  # }
  #}
  # Update plan boundaries based on search results

  plan
}

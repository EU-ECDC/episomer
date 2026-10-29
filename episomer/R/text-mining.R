# Load stop words from language files which will be a fixed set of twitter stop words plus first 500 more popular words on language
get_stop_words <- function(language_code) {
  #If language stopwords are already cached we rerun them
  stop_id <- paste("stop_words", language_code, sep = "_")
  if (exists(stop_id, where = cached)) {
    return(cached[[stop_id]])
  } else {
    fixed <- c("rt", "RT", "http", "https", "t.co", "amp", "via")
    stop_words <- unlist(jsonlite::read_json(get_scala_stopwords_url(
      language_code
    )))
    if (length(stop_words) > 0) {
      stop_words <- c(fixed, stop_words)
      stop_words <- stop_words[grep("'|\\\\", stop_words, invert = TRUE)]
      cached[[stop_id]] <- stop_words
      return(stop_words)
    } else {
      return(vector(mode = "character", length = 0))
    }
  }
}

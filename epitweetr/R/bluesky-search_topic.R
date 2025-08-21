bluesky_translate_query <- function(parts, excluded) {
    queries = list()
    comb = do.call(expand.grid, parts)
    if(nrow(comb) > 0) {
        ret <- lapply(1:nrow(comb), function(i) do.call(paste, as.list(comb[i,])))
        if(length(excluded) > 0)
          ret <- paste(ret, do.call(paste, as.list(paste0("-", excluded))))
	ret
    } else {
      list()
    }
}
#bluesky_set_date_boundaries <- function(plan) {
#  # We set the upper bound of the research: if missing we set it to the current time
#  max_text <- plan$research_max_date
#  if (is.null(plan$research_max_date)) {
#    plan$research_max_date <- Sys.time()
#    max_text <- "last message"
#  }
#
#  # We set the lower bound of the research
#  # If messages were retrived before, we set it to the newest message retrieved by the previous query
#  if (is.null(plan$research_min_date)) {
#    min_text <- "first message"
#  }
#  if (!is.null(plan$research_min_date)) {
#    min_text <- plan$research_min_date
#  }
#  if (!is.null(plan$boundaries_date_max)) {
#    plan$research_min_date <- plan$boundaries_date_max
#    min_text <- plan$research_min_date
#
#    if (plan$research_max_date <= plan$research_min_date) {
#      stop("We already have all the data we need")
#    }
#  }
#
#  list(plan = plan, max_text = max_text, min_text = min_text)
#}



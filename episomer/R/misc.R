#' @noRd
removeHTMLtags <- function(htmlString) {
  clean_string <- gsub(pattern = "<.*?>", replacement = "", x = htmlString)
  clean_string <- gsub(pattern = "<a", replacement = "", x = clean_string)
  clean_string <- gsub(pattern = "href", replacement = "", x = clean_string)
  clean_string
}

#' @noRd
removeURLS <- function(htmlString) {
  gsub(pattern = "http.*", replacement = "",x = htmlString)
}

#' @noRd
remove_newlines <- function(htmlString) {
  gsub(pattern = "[\r\n]", replacement = "",x = htmlString)
}

#' @noRd
remove_nbsp <- function(htmlString) {
  clean_string <- gsub(pattern = "&nbsp;", replacement = " ", x = htmlString)
  clean_string <- gsub(pattern = "nbsp", replacement = " ", x = clean_string)
  clean_string
}

#' @noRd
clean_messages <- function(text) {
  if(is.null(text))
     NULL
  else
    text %>% 
      removeHTMLtags()%>% 
      removeURLS()%>% 
      remove_newlines()%>%
      remove_nbsp()
}

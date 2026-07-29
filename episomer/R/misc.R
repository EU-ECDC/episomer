#' @noRd
removeHTMLtags <- function(htmlString) {
  gsub(pattern = "<.*?>", replacement = "", x = htmlString)
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
  gsub(pattern = "&nbsp;",replacement = " ",x = htmlString)
}

#' @noRd
clean_messages <- function(text) {
  text %>% 
    removeHTMLtags()%>% 
    removeURLS()%>% 
    remove_newlines()%>%
    remove_nbsp()
}
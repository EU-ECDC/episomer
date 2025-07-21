#' Convert date or datetime to ISO 8601 format for Bluesky API
#'
#' @param date_input A date or datetime object. Can be:
#'   - Date object (e.g., `as.Date("2024-01-01")`)
#'   - POSIXct/POSIXlt object (e.g., `as.POSIXct("2024-01-01 12:00:00")`)
#'   - Character string that can be parsed as date/datetime
#'   - Numeric value representing seconds since Unix epoch
#' @param timezone Timezone for the output. Defaults to "UTC" for Bluesky API compatibility.
#' @param include_time Whether to include time in the output. If FALSE, time will be set to 00:00:00.
#' @return Character string in ISO 8601 format (e.g., "2024-01-01T12:00:00Z")
#' @export
#' @examples
#' # Date only
#' bsky_format_date("2024-01-01")
#' bsky_format_date(as.Date("2024-01-01"))
#'
#' # Date and time
#' bsky_format_date("2024-01-01 12:30:45")
#' bsky_format_date(as.POSIXct("2024-01-01 12:30:45"))
#'
#' # Unix timestamp
#' bsky_format_date(1704067200)
#'
#' # Current time
#' bsky_format_date(Sys.time())
bsky_format_date <- function(
  date_input,
  timezone = "UTC",
  include_time = TRUE
) {
  # Handle different input types
  if (is.numeric(date_input)) {
    # Assume Unix timestamp (seconds since epoch)
    datetime <- as.POSIXct(date_input, origin = "1970-01-01", tz = timezone)
  } else if (is.character(date_input)) {
    # Try to parse as datetime first, then as date
    datetime <- tryCatch(
      {
        as.POSIXct(date_input, tz = timezone)
      },
      error = function(e) {
        # If datetime parsing fails, try date parsing
        as.POSIXct(paste(date_input, "00:00:00"), tz = timezone)
      }
    )
  } else if (inherits(date_input, "Date")) {
    # Convert Date to POSIXct
    datetime <- as.POSIXct(date_input, tz = timezone)
  } else if (inherits(date_input, c("POSIXct", "POSIXlt"))) {
    # Already a datetime object
    datetime <- as.POSIXct(date_input, tz = timezone)
  } else {
    stop(
      "Unsupported date input type. Please provide a Date, POSIXct, character string, or numeric timestamp."
    )
  }

  # Format based on include_time parameter
  if (include_time) {
    # Full ISO 8601 format with time
    format(datetime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    # Date only with time set to 00:00:00
    format(datetime, "%Y-%m-%dT00:00:00Z", tz = "UTC")
  }
}

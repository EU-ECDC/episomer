# API fixtures live in tests/testthat/fixtures/ as anonymised copies of real response bodies
# (see dev/dump_fixtures.R for the capture and dev/anonymize_fixtures.R for the anonymisation).
#
# simplifyVector = FALSE is not optional: with the default TRUE, jsonlite turns the arrays into
# data.frames and the parsers no longer see the nested lists they are written against.
read_fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("fixtures", name), simplifyVector = FALSE)
}

# Overrides the named fields of a list, NULL included: `override(x, list(a = NULL))` keeps `a` with
# a NULL value, which is what jsonlite produces for a json null.
#
# utils::modifyList() is deliberately NOT used by the fixture builders: it walks names(val), so an
# unnamed list (media_attachments, images, facets, tags...) replacing a field that is already a
# list is silently ignored, and the test then asserts on a status that never got the attachment.
override <- function(base, over) {
  for (nm in names(over)) base[nm] <- list(over[[nm]])
  base
}

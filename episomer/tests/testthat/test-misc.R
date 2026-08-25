test_that("removeHTMLtags removes HTML tags", {
  expect_equal(removeHTMLtags("<p>Hello</p>"), "Hello")
  expect_equal(removeHTMLtags("<a href='#'>link</a>"), "link")
  expect_equal(
    removeHTMLtags("text with <b>bold</b> content"),
    "text with bold content"
  )
  expect_equal(removeHTMLtags("<div class='test'>content</div>"), "content")
})

test_that("removeHTMLtags handles edge cases", {
  expect_equal(removeHTMLtags(""), "")
  expect_equal(removeHTMLtags("no tags here"), "no tags here")
  expect_equal(removeHTMLtags("<>"), "")
})

test_that("removeURLS removes URLs", {
  expect_equal(
    removeURLS("Check this http://example.com out"),
    "Check this  out"
  )
  expect_equal(removeURLS("Visit https://google.com today"), "Visit  today")
  expect_equal(
    removeURLS("text with http://example.com/path/to/page and more"),
    "text with  and more"
  )
})

test_that("removeURLS handles edge cases", {
  expect_equal(removeURLS(""), "")
  expect_equal(removeURLS("no urls here"), "no urls here")
  expect_equal(removeURLS("http://example.com"), "")
})

test_that("remove_newlines removes line breaks", {
  expect_equal(remove_newlines("line1\nline2"), "line1line2")
  expect_equal(remove_newlines("line1\r\nline2"), "line1line2")
  expect_equal(
    remove_newlines("text\nwith\nmultiple\nbreaks"),
    "textwithmultiplebreaks"
  )
})

test_that("remove_newlines handles edge cases", {
  expect_equal(remove_newlines(""), "")
  expect_equal(remove_newlines("no breaks"), "no breaks")
  expect_equal(remove_newlines("\n\n\n"), "")
})

test_that("remove_nbsp replaces non-breaking spaces", {
  expect_equal(remove_nbsp("word1&nbsp;word2"), "word1 word2")
  expect_equal(remove_nbsp("text nbsp more"), "text   more")
  expect_equal(remove_nbsp("&nbsp;start"), " start")
})

test_that("remove_nbsp handles edge cases", {
  expect_equal(remove_nbsp(""), "")
  expect_equal(remove_nbsp("normal spaces"), "normal spaces")
  expect_equal(remove_nbsp("&nbsp;&nbsp;"), "  ")
})

test_that("clean_messages applies all transformations", {
  expect_equal(
    clean_messages("<p>Hello&nbsp;World</p>"),
    "Hello World"
  )
  expect_equal(
    clean_messages("Check <a href='http://example.com'>this link</a>"),
    "Check this link"
  )
  expect_equal(
    clean_messages("Line1\n<b>Line2</b>&nbsp;with&nbsp;spaces"),
    "Line1Line2 with spaces"
  )
})

test_that("clean_messages handles NULL input", {
  expect_null(clean_messages(NULL))
})

test_that("clean_messages handles edge cases", {
  expect_equal(clean_messages(""), "")
  expect_equal(clean_messages("plain text"), "plain text")
})

test_that("clean_messages handles complex HTML", {
  html <- "<div class='container'><p>Read more at&nbsp;<a href='https://example.com'>example.com</a></p>\n<p>Second paragraph</p></div>"
  result <- clean_messages(html)
  expect_true(grepl("Read more at", result))
  expect_true(grepl("example.com", result))
  expect_false(grepl("<", result))
  expect_false(grepl("https://", result))
  expect_false(grepl("\n", result))
})

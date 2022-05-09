test_that("italics is replaced", {
  expect_match(latex2html("\\textit{something}"), "\\*something\\*")
  expect_match(latex2html("\\em{something}"), "\\*something\\*")
  expect_match(latex2html("\\textit{something} and \\em{another}"), "\\*something\\* and \\*another\\*")
})

test_that("bold is replaced", {
  expect_match(latex2html("\\textbf{something} in bold"), "\\*\\*something\\*\\* in bold")
})

test_that("small caps are replaced", {
  expect_match(latex2html("Text with \\textsc{small caps}"), "Text with SMALL CAPS")
  expect_match(latex2html("Word.\\textsc{tag}"), "Word.TAG")
})

test_that("words are split", {
  to_test <- gloss_linesplit("Many words")
  expect_length(to_test, 2)
})

test_that("words between curly braces are not split", {
  to_test <- gloss_linesplit("Many words is {one expression}.")
  expect_length(to_test, 4)
})

test_that("styles are modified", {
  expect_s3_class(format_html(), "shiny.tag")
  expect_match(as.character(format_html()), "<style>")
  # TODO maybe test the outputs, but they depend on options()...
})

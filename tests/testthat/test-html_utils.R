test_that("italics is replaced", {
  expect_match(ignore_latex("\\textit{something}"), "\\*something\\*")
  expect_match(ignore_latex("\\em{something}"), "\\*something\\*")
  expect_match(ignore_latex("\\textit{something} and \\em{another}"), "\\*something\\* and \\*another\\*")
})

test_that("bold is replaced", {
  expect_match(ignore_latex("\\textbf{something} in bold"), "\\*\\*something\\*\\* in bold")
})

test_that("small caps are replaced", {
  expect_match(ignore_latex("Text with \\textsc{small caps}"), "Text with SMALL CAPS")
  expect_match(ignore_latex("Word.\\textsc{tag}"), "Word.TAG")
})

test_that("words are split", {
  to_test <- gloss_linesplit("Many words")
  expect_length(to_test, 2)
})

test_that("words between curly braces are not split", {
  to_test <- gloss_linesplit("Many words is {one expression}.")
  expect_length(to_test, 4)
})

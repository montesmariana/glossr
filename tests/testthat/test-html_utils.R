test_that("italics is replaced", {
  expect_match(ignore_latex("\\textit{something}"), "\\*something\\*")
})

test_that("bold is replaced", {
  expect_match(ignore_latex("\\textbf{something}"), "\\*something\\*")
})

test_that("small caps are replaced", {
  expect_match(ignore_latex("\\textsc{something}"), "\\*something\\*")
})

test_that("words are split", {
  to_test <- gloss_linesplit("Many words")
  expect_length(to_test, 2)
})

test_that("words between curly braces are not split", {
  to_test <- gloss_linesplit("Many words is {one expression}.")
  expect_length(to_test, 4)
})

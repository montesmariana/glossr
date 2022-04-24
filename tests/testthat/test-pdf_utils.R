test_that("long string formating works", {
  expect_match(
    gloss_format_words("some text", "textit"),
    "\\\\textit\\{some\\} \\\\textit\\{text\\}")
})

# TODO add tests for gloss_list

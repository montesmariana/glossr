test_that("long string formating works", {
  expect_match(
    gloss_format_words("some text", "textit"),
    "\\\\textit\\{some\\} \\\\textit\\{text\\}")
})

# test gloss_list ----
glosses <- dplyr::select(glosses, -language)
test_that("gloss_list has the right class", {
  expect_error(gloss_list("not a gloss"))
  gl <- gloss_list(gloss_df(glosses))
  expect_s3_class(gl, "gloss")
  expect_identical(attr(gl, "data"), glosses)
})

test_that("gloss_list renders properly with leipzig", {
  options("glossr.output" = "leipzig")
  from_glosses <- gloss_df(glosses)
  gl <- gloss_list(from_glosses)
  expect_identical(gl[[1]], from_glosses[[1]])
})

test_that("gloss_list renders properly with latex", {
  options("glossr.output" = "latex")
  from_glosses <- gloss_df(glosses)
  gl <- gloss_list(from_glosses)
  for (gloss_item in from_glosses) {
    as_re <- gsub("\\\\ex", "\\\\a", gloss_item) %>%
      gsub(pattern = "\\\\xe \\n", replacement = "") %>%
      gsub(pattern = "([\\{\\(])", replacement = "\\\\\\1")
    expect_match(gl[[1]], as_re)
  }
})

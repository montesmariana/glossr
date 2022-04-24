test_that("tooltip is generated", {
  observed <- as.character(gloss_tooltip("one", "DET.SG"))
  expected <- "<span data-toggle=\"tooltip\" title=\"DET.SG\"> one </span>"
  expect_match(observed, expected)
})

ex_sp <- "Un ejemplo en español"
ex_gloss <- "DET.M.SG example in Spanish"
line <- gloss_linetooltip(ex_sp, ex_gloss)

test_that("tooltip list has right length", {
  expect_length(line, 4)
})

test_that("first item of tooltip line is correct", {
  expected <- "<span data-toggle=\"tooltip\" title=\"DET.M.SG\"> Un </span>"
  expect_match(as.character(line[[1]]), expected)
})

test_that("fourth item of tooltip line is correct", {
  expected <- "<span data-toggle=\"tooltip\" title=\"Spanish\"> español </span>"
  expect_match(as.character(line[[4]]), expected)
})

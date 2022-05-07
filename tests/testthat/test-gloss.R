ex_sp <- "Un ejemplo en español"
ex_gloss <- "DET.M.SG example in Spanish"
ex_third <- "One example in Spanish"
ex_trans <- "An example in Spanish."

bare <- new_gloss_data(list(ex_sp, ex_gloss, ex_third))

test_that("class is correct", {
  expect_s3_class(bare, "gloss_data")
  expect_s3_class(as_gloss(ex_sp, ex_gloss, translation = ex_trans), "gloss")
})

test_that("Default values are empty", {
  expect_false(attr(bare, "has_source"))
  expect_false(attr(bare, "has_translation"))
  expect_equal(length(bare), length(attr(bare, "lengths")))
  expect_equal(nchar(attr(bare, "translation")), 0)
  expect_equal(nchar(attr(bare, "label")), 0)
})

test_that("Translation is quoted", {
  with_trans <- new_gloss_data(list(ex_sp, ex_gloss), translation = ex_trans)
  expect_match(attr(with_trans, "translation"), "^\\\"An example in Spanish.\\\"$")
  with_single <- new_gloss_data(list(ex_sp, ex_gloss), translation = ex_trans, trans_quotes = "'")
  expect_match(attr(with_single, "translation"), "^'An example in Spanish.'$")
})

test_that("Wrong output throws error", {
  expect_error(new_gloss_data(ex_sp))
  expect_error(gloss_df(data.frame()))
  expect_error(gloss_df(exp_sp))
})

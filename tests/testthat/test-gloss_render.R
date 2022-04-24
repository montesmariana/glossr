ex_sp <- "Un ejemplo en español"
ex_gloss <- "DET.M.SG example in Spanish"
ex_trans <- "'An example in Spanish.'"
pdf_full <- gloss_pdf(ex_sp, ex_gloss, ex_trans, "ex1")
pdf_notrans <- gloss_pdf(ex_sp, ex_gloss, label = "ex2")
pdf_bare <- gloss_pdf(ex_sp, ex_gloss)

test_that("gloss label renders in pdf", {
  expect_match(pdf_full[[1]], " +\\\\ex\\\\label")
})

test_that("first gloss line renders in pdf", {
  expect_match(pdf_full[[2]], " +\\\\gll ")
})

test_that("second gloss line renders in pdf", {
  expect_match(pdf_full[[3]], "DET.M.SG example in Spanish")
})

test_that("translation is rendered in pdf", {
  expect_match(pdf_full[[4]], " +\\\\trans ")
})

test_that("translation is not rendered if not required", {
  expect_equal(length(pdf_notrans[[4]]), 1)
})

test_that("label is correct with missing translation", {
  expect_match(pdf_notrans[[1]], "ex2")
})

test_that("labels is not rendered if not required", {
  expect_match(pdf_bare[[1]], " +\\\\ex\\n")
})

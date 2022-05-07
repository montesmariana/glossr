ex_sp <- "Un ejemplo en español"
ex_gloss <- "DET.M.SG example in Spanish"
ex_trans <- "An example in Spanish."
gloss_lines <- list(ex_sp, ex_gloss)
my_gloss <- new_gloss_data(gloss_lines, translation = ex_trans, label = "ex1")
no_trans <- new_gloss_data(gloss_lines, label = "ex2")
bare_gloss <- new_gloss_data(gloss_lines)
source_gloss <- new_gloss_data(gloss_lines, source = "(Author:year)")

# Test classes
test_that("Classes are correct", {
  expect_s3_class(my_gloss, "gloss_data")
  expect_s3_class(gloss_pdf(my_gloss), "gloss")
  expect_s3_class(gloss_html(my_gloss), "gloss")
  expect_s3_class(gloss_word(my_gloss), "gloss")
  expect_error(gloss_pdf(ex_sp))
  expect_error(gloss_html(ex_sp))
  expect_error(gloss_word(ex_sp))
})

# Test pdf ----
test_that("almost full gloss label renders in pdf", {
  pdf_full <- gloss_pdf(my_gloss)
  expect_match(pdf_full[[1]], "^\\\\ex$")
  expect_match(pdf_full[[2]], "^\\\\label\\{ex1\\}\\\n$")
  expect_match(pdf_full[[3]], "^\\\\begingl \\\n$")
  expect_match(pdf_full[[5]], "^\\\\gla Un ejemplo en español// \\\n$")
  expect_match(pdf_full[[6]], "^\\\\glb DET.M.SG example in Spanish// \\\n$")
  expect_match(pdf_full[[7]], "^\\\\glft \\\"An example in Spanish.\\\"// \n$")
  expect_match(pdf_full[[8]], "^\\\\endgl \n$")
  expect_match(pdf_full[[9]], "^\\\\xe \n$")
})

test_that("gloss label without translation renders in pdf", {
  pdf <- gloss_pdf(no_trans)
  expect_equal(nchar(pdf[[7]]), 0)
})

test_that("gloss label with source renders in pdf", {
  pdf <- gloss_pdf(source_gloss)
  expect_equal(nchar(pdf[[4]]), 29)
})


# Test html ----
test_that("gloss label renders in html", {
  html <- gloss_html(my_gloss)
  expect_match(html[[1]], "\\(@ex1\\)")
  expect_match(html[[2]], "^<div data-gloss=\\\"\\\">\\n")
  expect_match(html[[2]], ".*\\n</div>")
  expect_match(
    html[[2]],
    "<p class=\\\"gloss__line--free\\\">\\\"An example in Spanish.\\\"</p>"
    )
  expect_match(html[[3]], "^\\n$")
})

test_that("translation is not rendered if not required", {
  html <- gloss_html(no_trans)
  expect_no_match(
    html[[2]],
    "<p class=\\\"gloss__line--free\\\">\\\"An example in Spanish.\\\"</p>"
    )
})

test_that("labels is not rendered if not required", {
  expect_match(gloss_html(bare_gloss)[[1]], "^\\(@\\) $")
})

test_that("source is rendered", {
  expect_match(
    gloss_html(source_gloss)[[2]],
    "<p class=\\\"gloss__line--original\\\">\\(Author:year\\)</p>"
    )
})

# Test word ----
test_that("gloss label renders in word", {
  word <- gloss_word(my_gloss)
  expect_length(word, 3)
  expect_match(word[[1]], "^\\(@ex1\\) _\\n$")
  expect_match(
    word[[2]],
    "^```\\{=html\\}\\n<div class=\\\"tabwid tabwid_left\\\"><style>"
    )
  expect_match(gloss_word(source_gloss)[[1]], "^\\(@\\) \\(Author:year\\)\\n$")
})

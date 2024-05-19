ex_sp <- "Un ejemplo en español"
ex_gloss <- "DET.M.SG example in Spanish"
ex_trans <- "An example in Spanish."
gloss_lines <- list(ex_sp, ex_gloss)
my_gloss <- new_gloss_data(gloss_lines, translation = ex_trans, label = "ex1")
no_trans <- new_gloss_data(gloss_lines, label = "ex2")
bare_gloss <- new_gloss_data(gloss_lines)
source_gloss <- new_gloss_data(gloss_lines, source = "(Author:year)")
single_gloss <- new_gloss_data(list(ex_sp))

# Test classes
test_that("Classes are correct", {
  expect_s3_class(my_gloss, "gloss_data")
  expect_s3_class(single_gloss, "gloss_data")
  expect_s3_class(gloss_pdf(my_gloss), "gloss")
  expect_s3_class(gloss_html(my_gloss), "gloss")
  expect_s3_class(gloss_word(my_gloss), "gloss")
  expect_s3_class(gloss_single(single_gloss), "gloss")
  expect_error(gloss_pdf(ex_sp))
  expect_error(gloss_html(ex_sp))
  expect_error(gloss_word(ex_sp))
  expect_error(gloss_single(source_gloss))
})

# Test pdf ----
test_that("almost full gloss label renders in pdf", {
  pdf_full <- gloss_pdf(my_gloss)
  expect_match(pdf_full[[1]], r"(^\\ex\\label\{ex1} \\begingl \\gla Un ejemplo en español// \\glb DET.M.SG example in Spanish// \\glft \"An example in Spanish.\"// \n \\endgl \\xe \n$)")
})

test_that("gloss label without translation renders in pdf", {
  pdf <- gloss_pdf(no_trans)
  expect_match(pdf[[1]], r"(\\ex\\label\{ex2} \\begingl \\gla Un ejemplo en español// \\glb DET.M.SG example in Spanish//  \\endgl \\xe \n)")
})

test_that("gloss label with source renders in pdf", {
  pdf <- gloss_pdf(source_gloss)
  expect_match(pdf[[1]], r"{\\ex \\begingl \\glpreamble \(Author:year)// \\gla Un ejemplo en español// \\glb DET.M.SG example in Spanish//  \\endgl \\xe \n}")
})


# Test html ----
test_that("gloss label renders in html", {
  html <- gloss_html(my_gloss)
  expect_match(html[[1]], "\\(@ex1\\)")
  expect_match(html[[2]], "^<style>.*<div data-gloss=\\\"\\\">\\n")
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
spaces <- "(&nbsp;| )+"
test_that("gloss label renders in word", {
  word <- gloss_word(my_gloss)
  expect_length(word, 1)
  word_parts <- strsplit(word, "\n\n    ")[[1]]
  expect_length(word_parts, 3)
  expect_match(
    word_parts[[1]],
    sprintf("^\\(@ex1\\) Un%sejemplo%sen%sespañol%s$", spaces, spaces, spaces, spaces)
  )
  expect_match(
    strsplit(gloss_word(source_gloss)[[1]], "\n\n    ")[[1]][[1]],
    "^\\(@\\) \\(Author:year\\)$"
    )
})

# Test single gloss ----
test_that("Single gloss has the right text", {
  single <- gloss_single(single_gloss)
  expect_length(single, 1)
  expect_match(single[[1]], r"[\(@\) Un ejemplo en español]")

  single2 <- gloss_single(new_gloss_data(list(ex_sp), translation = ex_trans, label = "ex1"))
  expect_length(single2, 1)
  expect_match(single2[[1]], r"{\(@ex1\) Un ejemplo en español \n+\s+\"An example in Spanish\.\"\n+}")

  expect_equal(as_gloss(ex_sp, output_format = 'leipzig'), gloss_single(single_gloss))
  expect_equal(as_gloss(ex_sp, output_format = 'word'), gloss_word(single_gloss))
  expect_equal(as_gloss(ex_sp, output_format = 'latex'), gloss_pdf(single_gloss))
})

test_that("gloss lines are identified",  {
  ex_sp <- "Un ejemplo en español"
  ex_gloss <- "DET.M.SG example in Spanish"
  ex_trans <- "An example in Spanish."
  my_gloss <- create_gloss(ex_sp, ex_gloss, translation = ex_trans, label = "ex1")
  lines <- gloss_word_lines(unclass(my_gloss))
  expect_length(lines, 1)
  expect_equal(
    length(lines[[1]]),
    max(attr(my_gloss, "lengths"))
  )
  expect_equal(
    length(my_gloss),
    nrow(lines[[1]])
  )
}
)

test_that("long examples are properly folded", {
  long_ex <- glosses[5,]
  long_gloss <- create_gloss(
    long_ex$original,
    long_ex$parsed,
    translation = long_ex$translation,
    source = long_ex$source,
    label = long_ex$label
  )
  lines <- gloss_word_lines(unclass(long_gloss))
  expect_length(lines, 2)
  expect_equal(length(lines[[1]]), 6)
  expect_equal(length(long_gloss), nrow(lines[[1]]))
  expect_equal(length(lines[[2]]), 3)
  expect_equal(length(long_gloss), nrow(lines[[2]]))
  expect_equal(
    length(lines[[1]]) + length(lines[[2]]),
    max(attr(long_gloss, "lengths"))
    )
})

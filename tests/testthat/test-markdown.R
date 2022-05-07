a <- "First line"
b <- "Second line"
t <- "This is the translation"
s <- "Some source"
l <- "label"

test_that("Single gloss lines are printed", {
  expect_snapshot_output(knit_print(
    as_gloss(a, b, translation = t, source = s, label = l,
             output_format = "leipzig")
  ))
  expect_snapshot_output(knit_print(
    as_gloss(a, b, translation = t, source = s, label = l,
             output_format = "latex")
  ))
  expect_snapshot_output(knit_print(
    as_gloss(a, b, translation = t, source = s, label = l,
             output_format = "word")
  ))
})

test_that("Single gloss lines without source are printed", {
  expect_snapshot_output(knit_print(
    as_gloss(a, b, translation = t, label = l,
             output_format = "leipzig")
  ))
  expect_snapshot_output(knit_print(
    as_gloss(a, b, translation = t, label = l,
             output_format = "latex")
  ))
  expect_snapshot_output(knit_print(
    as_gloss(a, b, translation = t, label = l,
             output_format = "word")
  ))
})


test_that("Series of gloss lines are printed", {
  expect_snapshot_output(knit_print(
    gloss_df(glosses, output_format = "leipzig"))
  )
  expect_snapshot_output(knit_print(
    gloss_df(glosses, output_format = "latex"))
  )
  expect_snapshot_output(knit_print(
    gloss_list(gloss_df(glosses, output_format = "latex")))
  )
  expect_snapshot_output(knit_print(
    gloss_df(glosses, output_format = "word"))
  )
})

a <- "First line"
b <- "Second line"
t <- "This is the translation"
s <- "Some source"
l <- "label"

# One line with source ----
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

# One line without source ----
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


# Multiple lines ----
expect_snapshot_output(knit_print(
  gloss_df(glosses, output_format = "leipzig"))
  )
expect_snapshot_output(knit_print(
  gloss_df(glosses, output_format = "latex"))
)
expect_snapshot_output(knit_print(
  gloss_df(glosses, output_format = "word"))
)

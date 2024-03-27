width_with_spaces <- function(x) {
  systemfonts::string_width(gsub("&nbsp;", " ", x))
}
test_that("gloss lines are identified",  {
  ex_sp <- "Un ejemplo en español"
  ex_gloss <- "DET.M.SG example in Spanish"
  ex_trans <- "An example in Spanish."
  gloss_lines <- list(ex_sp, ex_gloss)
  my_gloss <- new_gloss_data(gloss_lines, translation = ex_trans, label = "ex1")
  lines <- align_word(my_gloss)
  expect_length(lines, 2)
  expect_lt(abs(width_with_spaces(lines[[1]]) - width_with_spaces(lines[[2]])),
            systemfonts::string_width(" "))
  for (line in lines) {
    expect_lte(width_with_spaces(line), 1332)
  }
  final_gloss <- gloss_word(my_gloss)
  expect_length(final_gloss, 1)
}
)

test_that("long examples are properly folded", {
  long_ex <- glosses[5,]
  long_gloss <- new_gloss_data(
    list(
      long_ex$original,
      long_ex$parsed
      ),
    translation = long_ex$translation,
    source = long_ex$source,
    label = long_ex$label
  )
  lines <- align_word(long_gloss)
  expect_length(lines, 4)
  expect_lt(abs(width_with_spaces(lines[[1]]) - width_with_spaces(lines[[2]])),
            systemfonts::string_width(" ")*2)
  expect_lt(abs(width_with_spaces(lines[[3]]) - width_with_spaces(lines[[4]])),
            systemfonts::string_width(" "))
  for (line in lines) {
    expect_lte(width_with_spaces(line), 1332)
  }

})

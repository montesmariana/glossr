test_that("styling options are checked", {
  expect_warning(set_style_options(list(test = "testing")))
  expect_warning(set_style_options(list(a = "testing")))
})

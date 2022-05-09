test_that("defaults are set", {
  expect_null(set_default(NULL, NULL))
  expect_null(set_default(3, NULL))
  expect_equal(nchar(set_default(NA)), 0)
  expect_equal(nchar(set_default('')), 0)
  expect_equal(nchar(set_default(NULL)), 0)
  expect_equal(nchar(set_default(3)), 0)
  expect_equal(set_default("test"), "test")
  expect_equal(set_default("test", NULL), "test")
})

test_that("output is set", {
  expect_warning(validate_output("test"))
  expect_invisible(validate_output("latex"))
  expect_invisible(validate_output("leipzig"))
  expect_invisible(validate_output("html"))
  expect_invisible(validate_output("word"))
})

test_that("style options are correct", {
  expect_equal(style_options("i"), c("i", "it", "italics", "textit"), ignore_attr = TRUE)
  expect_equal(style_options("b"), c("b", "bf", "bold", "textbf"), ignore_attr = TRUE)
  expect_null(style_options("m"))
  expect_null(style_options("bold"))
})

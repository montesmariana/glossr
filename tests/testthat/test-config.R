test_that("The configuration file overrides properly", {
  reset_config()
  expect_equal(config$pdf$exskip, 0)
  expect_match(config$format$a, "")
  expect_equal(config$word$font_size, 12)
  expect_true(config$numbering)

  conffile <- config_from_file(system.file("extdata/glossr-config.yml", package="glossr"))
  expect_contains(names(conffile), c("format", "pdf", "word", "other"))
  expect_equal(config$pdf$exskip, 2)
  expect_length(config$word$font_family, 3)
  expect_match(config$word$font_family$b, conffile$word$font_family$default)
  expect_true(config$numbering)
  reset_config()
})

test_that("use_glossr() overrides configuration properly", {
  reset_config()
  expect_equal(config$pdf$exskip, 0)
  expect_match(config$format$a, "")
  expect_equal(config$word$font_size, 12)
  expect_true(config$numbering)

  use_glossr(styling = list(
    exskip = 2,
    font_family = list(a = "Cambria"),
    numbering = FALSE
  ))
  expect_equal(config$pdf$exskip, 2)
  expect_length(config$word$font_family, 3)
  expect_match(config$word$font_family$b, config$word$font_family$c)
  expect_false(config$numbering)
  reset_config()
})

test_that("use_glossr() throws some errors", {
  expect_error(use_glossr(styling = list(font_family = 3)))
  expect_warning(use_glossr(styling = list(font_family = list(x = "Cambria"))))
})

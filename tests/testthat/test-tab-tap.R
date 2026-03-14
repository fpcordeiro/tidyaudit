test_that("tab_tap stores tabulation in trail and returns data unchanged", {
  trail <- audit_trail("test")
  result <- mtcars |>
    tab_tap(cyl, .trail = trail, .label = "by_cyl")

  expect_identical(result, mtcars)
  expect_equal(length(trail$snapshots), 1L)

  snap <- trail$snapshots[[1L]]
  expect_equal(snap$label, "by_cyl")
  expect_true("tab(cyl)" %in% names(snap$custom))

  tab_result <- snap$custom[["tab(cyl)"]]
  expect_s3_class(tab_result, "tidyaudit_tab")
  expect_equal(tab_result$var_name, "cyl")
})

test_that("tab_tap works with multiple taps in a pipeline", {
  trail <- audit_trail("pipeline")
  result <- mtcars |>
    tab_tap(cyl, .trail = trail, .label = "raw") |>
    dplyr::filter(mpg > 20) |>
    tab_tap(cyl, .trail = trail, .label = "filtered")

  expect_equal(length(trail$snapshots), 2L)
  expect_equal(nrow(result), sum(mtcars$mpg > 20))
})

test_that("tab_tap passes .sort and .cutoff through", {
  trail <- audit_trail("test")
  mtcars |>
    tab_tap(cyl, .trail = trail, .label = "s1",
            .sort = "freq_desc", .cutoff = 2)

  tab_result <- trail$snapshots[[1L]]$custom[["tab(cyl)"]]
  expect_equal(tab_result$sort, "freq_desc")
  expect_equal(tab_result$n_other, 1L)
})

test_that("tab_tap passes .wt through", {
  trail <- audit_trail("test")
  mtcars |>
    tab_tap(cyl, .trail = trail, .label = "s1", .wt = mpg)

  tab_result <- trail$snapshots[[1L]]$custom[["tab(cyl)"]]
  expect_true(tab_result$weighted)
  expect_equal(tab_result$wt_name, "mpg")
})

test_that("tab_tap errors without audit_trail", {
  expect_error(
    tab_tap(mtcars, cyl, .trail = "not a trail", .label = "x"),
    "audit_trail"
  )
})

test_that("tab_tap with two variables names the diagnostic correctly", {
  trail <- audit_trail("test")
  mtcars |>
    tab_tap(cyl, gear, .trail = trail, .label = "cross")

  snap <- trail$snapshots[[1L]]
  expect_true("tab(cyl, gear)" %in% names(snap$custom))

  tab_result <- snap$custom[["tab(cyl, gear)"]]
  expect_equal(tab_result$type, "twoway")
})

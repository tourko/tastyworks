context("Confirmation - Generic block")

test_that("Probe with no lines", {
  result <- block$new()$probe(lines = c())
  expected <- dplyr::tibble()

  expect_identical(result, expected)
})

test_that("Probe with an empty lines", {
  result <- block$new()$probe(lines = c(""))
  expected <- dplyr::tibble()

  expect_identical(result, expected)
})

test_that("Probe with a line", {
  result <- block$new()$probe(lines = c("Test line"))
  expected <- dplyr::tibble()

  expect_identical(result, expected)
})

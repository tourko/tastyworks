context("Confirmation - Total block")

test_that("All zeros", {
  lines <- c(
    "TOTAL SHARES BOUGHT: 0.00 TOTAL DOLLARS BOUGHT: 0.00",
    "TOTAL SHARES SOLD: 0.00 TOTAL DOLLARS SOLD: 0.00"
  )

  result   <- total_block$new()$probe(lines)
  expected <- tibble::tibble(shares  = c(0L, 0L),
                             dollars = c(0.00, 0.00))

  expect_identical(result, expected)
})

test_that("Negative numbers", {
  lines <- c(
    "TOTAL SHARES BOUGHT: 5.00 TOTAL DOLLARS BOUGHT: -100.00",
    "TOTAL SHARES SOLD: -5.00 TOTAL DOLLARS SOLD: 100.00"
  )

  result   <- total_block$new()$probe(lines)
  expected <- tibble::tibble(shares  = c(5L, -5L),
                             dollars = c(-100.00, 100.00))

  expect_identical(result, expected)
})

test_that("Dollar amount with ','", {
  lines <- c(
    "TOTAL SHARES BOUGHT: 100.00 TOTAL DOLLARS BOUGHT: -6,505.00",
    "TOTAL SHARES SOLD: 0.00 TOTAL DOLLARS SOLD: 0.00"
  )

  result   <- total_block$new()$probe(lines)
  expected <- tibble::tibble(shares  = c(100L, 0L),
                             dollars = c(-6505.00, 0.00))

  expect_identical(result, expected)
})

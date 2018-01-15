context("Confirmation - Total block")

test_that("Parse", {
  lines <- c(
    "TOTAL SHARES BOUGHT: 100.00 TOTAL DOLLARS BOUGHT: -6,505.00",
    "TOTAL SHARES SOLD: 0.00 TOTAL DOLLARS SOLD: 0.00"
  )

  result   <- total_block_factory$new()$probe(lines)
  expected <- tibble::tibble(shares  = c(100L, 0L),
                             dollars = c(-6505.00, 0.00))

  expect_identical(result, expected)
})

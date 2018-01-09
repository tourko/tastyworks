context("Confirmation - Total block")

test_that("Parse", {
  lines <- c(
    "TOTAL SHARES BOUGHT: 100.00 TOTAL DOLLARS BOUGHT: -6,505.00",
    "TOTAL SHARES SOLD: 0.00 TOTAL DOLLARS SOLD: 0.00"
  )

  result   <- lines %>% block$parse(total_block)
  expected <- tibble(shares = c(100L, 0L), dollars = c(-6505.00, 0.00))

  expect_identical(result, expected)
})

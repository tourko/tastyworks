library(dplyr)

context("Assemble orders")

# Location of the confirmations
confirmations_folder <- file.path("confirmations")

test_that("Simple orders", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file)
  orders <- assemble_orders(transactions)

  xop <- orders %>% filter(symbol == "XOP")
  fb  <- orders %>% filter(symbol == "FB")

  # There should be 2 orders.
  expect_equal(nrow(orders), 2)
  # One for XOP
  expect_equal(nrow(xop), 1)
  # And one for FB
  expect_equal(nrow(fb), 1)
})

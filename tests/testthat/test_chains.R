library(dplyr)

context("Order chains")

# Location of the confirmations
confirmations_folder <- file.path("confirmations")

test_that("Simple order chain", {
  confirmation_files <- c(
    file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf")
  )

  xop <- confirmation_files %>%
    read_confirmations() %>%
    filter(symbol == "XOP")

  chains <- orders$create(xop) %>%
    orders$classify() %>%
    orders$summarise() %>%
    orders$chain()

  # The confirmation has only opening transactions, so no orphats are expected.
  expect_equal(length(chains), 1)
})

library(dplyr)

context("Find orphanted transactions")

# Location of the confirmations
confirmations_folder <- file.path("confirmations")

test_that("Confirmation with no orphants", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file)
  orphants <- find_orphants(transactions)

  # The confirmation has only opening transactions, so no orphats are expected.
  expect_equal(nrow(orphants), 0)
})

test_that("Confirmation with an orphant", {
  confirmation_file <- file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file)
  orphants <- find_orphants(transactions)

  # The confirmation has 1 closing transactions, and no corresponding opening transaction.
  expect_equal(nrow(orphants), 1)
  # The orphant is an XOP
  expect_true(orphants[1, "symbol"] == "XOP")
})

test_that("Matching open and close transactions", {
  confirmation_files <- c(
    file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf")
  )

  transactions <- read_confirmations(confirmation_files)
  orphants <- find_orphants(transactions)

  # The confirmations have both open and close transactions for XOP,
  # so no orphats are expected.
  expect_equal(nrow(orphants), 0)
})

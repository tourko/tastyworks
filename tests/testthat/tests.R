library(dplyr)

context("Read confirmations")

# Location of the confirmations
confirmations_folder <- file.path("confirmations")

test_that("Read a single confirmation", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file)

  # There are 5 transactions in the confirmation
  expect_equal(nrow(transactions), 5)
})

test_that("Mixed option and stock transactions", {
  confirmation_file <- file.path(confirmations_folder, "2017-12-15-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file)

  # UNG transactions
  ung <- transactions %>%
    filter(symbol == "UNG")

  ung_option <- ung %>%
    filter(instrument == "OPTION")

  ung_stock <- ung %>%
    filter(instrument == "STOCK")

  # There are 2 UNG transactions
  expect_equal(nrow(ung), 2)
  # One is option
  expect_equal(nrow(ung_option), 1)
  # And one is stock
  expect_equal(nrow(ung_stock), 1)
})

test_that("Read several confirmations", {
  confirmation_file <- c(
    file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-08-31-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-05-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-06-1NE23456-confirmation.pdf")
  )

  transactions <- read_confirmations(confirmation_file)

  # There are 36 transactions in the confirmations
  expect_equal(nrow(transactions), 36)
})

test_that("Assigned stock", {
  confirmation_file <- c(
    file.path(confirmations_folder, "2017-10-18-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-10-19-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-11-09-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-12-08-1NE23456-confirmation.pdf")
  )

  transactions <- read_confirmations(confirmation_file)

  # Filter UAL transactions
  ual <- transactions %>%
    filter(symbol == "UAL")

  # Filter "dummy" assigned option
  assigned_option <- ual %>%
    filter(instrument == "OPTION", reason == "ASSIGNED")

  # There are total 21 transactions in the confirmations + 1 dummy assigned option
  expect_equal(nrow(transactions), 22)
  # There are 8 UAL transactions
  expect_equal(nrow(ual), 10)
  # There is 1 assigned option
  expect_equal(nrow(assigned_option), 1)
})

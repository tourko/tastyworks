library(dplyr)

context("Check transactions integrity")

# Location of the confirmations
confirmations_folder <- file.path(test_path(), "confirmations")

test_that("OPEN transactions only", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  # A message about integrity checking is expected and no warnings
  expect_message(read_confirmations(confirmation_file, check.integrity = TRUE),
                 regexp = "Checking transactions integrity...")
})

test_that("CLOSE transaction without an OPEN transaction", {
  confirmation_file <- file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf")

  # Expect to see warnings
  expect_warning(read_confirmations(confirmation_file, check.integrity = TRUE),
                 regexp = "There are CLOSE transactions that have greater quantity than OPEN transactions:")
  expect_warning(read_confirmations(confirmation_file, check.integrity = TRUE),
                 regexp = "2017-09-01 UNSOLICITED BUY CLOSE XOP OPTION 1 PUT 28 2017-10-20 0.39 39 0 0.04 0.1 39.14 9H82162 Y8611")
})

test_that("OPEN and matching CLOSE transaction", {
  confirmation_files <- c(
    file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf")
  )

  # A message about integrity checking is expected and no warnings
  expect_message(read_confirmations(confirmation_files, check.integrity = TRUE),
                 regexp = "Checking transactions integrity...")
})

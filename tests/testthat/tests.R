context("Read Confirmations")

# Location of the confirmations
confirmations_folder <- file.path("confirmations")

test_that("Read single confirmation", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file)

  expect_equal(nrow(transactions), 5)
})

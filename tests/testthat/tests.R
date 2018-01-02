context("Read Confirmations")

# Location of the confirmations
path <- file.path("confirmations")

test_that("Read single confirmation", {
  confirmation_file <- list.files(path       = path.expand(path),
                                  pattern    = "2017-08-30-1NE23456-confirmation.pdf",
                                  full.names = TRUE)

  transactions <- read_confirmations(confirmation_file)

  expect_equal(nrow(transactions), 5)
})

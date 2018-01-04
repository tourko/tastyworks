library(dplyr)

context("Read confirmations")

# Location of the confirmations
confirmations_folder <- file.path("confirmations")

test_that("Read a single confirmation file", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file)

  # If there is a confirmation, there must be at least 1 transactions in it.
  expect_gt(nrow(transactions), 0)
})

test_that("Read several confirmation files", {
  confirmation_file <- c(
    file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-08-31-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-05-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-06-1NE23456-confirmation.pdf")
  )

  transactions <- read_confirmations(confirmation_file)

  # There must be at least 1 transactions in it
  expect_gt(nrow(transactions), 0)
})

test_that("Confirmation with unsolicited options only", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file, add.expired = FALSE)

  # Get unsolicited transactions
  unsolicited <- transactions %>%
    filter(reason == "UNSOLICITED")

  # Get unsolicited transactions
  expired <- transactions %>%
    filter(reason == "EXPIRED")

  # There are 5 unsolicited transactions
  expect_equal(nrow(unsolicited), 5)

  # ... and no expired options were added
  expect_equal(nrow(expired), 0)
})

test_that("Confirmation with unsolicited and added expired options", {
  confirmation_file <- file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf")

  transactions <- read_confirmations(confirmation_file, add.expired = TRUE)

  # Get unsolicited transactions
  unsolicited <- transactions %>%
    filter(reason == "UNSOLICITED")

  # Get unsolicited transactions
  expired <- transactions %>%
    filter(reason == "EXPIRED")

  # There are:
  # • 5 unsolicited transactions
  # • 5 added expired transaction
  expect_equal(nrow(transactions), 10)

  # Number of unsolicited transactions should be equal to that of expired transaction
  expect_true(nrow(unsolicited) == nrow(expired))
})

test_that("Confirmation with both option and stock transactions", {
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

test_that("Confirmation with closed options", {
  confirmation_file <- c(
    file.path(confirmations_folder, "2017-08-30-1NE23456-confirmation.pdf"),
    file.path(confirmations_folder, "2017-09-01-1NE23456-confirmation.pdf")
  )

  transactions <- read_confirmations(confirmation_file)

  xop <- transactions %>%
    filter(symbol == "XOP")

  xop_open    <- xop %>% filter(reason == "UNSOLICITED", position == "OPEN")
  xop_close   <- xop %>% filter(reason == "UNSOLICITED", position == "CLOSE")
  xop_expired <- xop %>% filter(reason == "EXPIRED")

  # There are 2 XOP transactions
  expect_equal(nrow(xop), 2)

  # There is 1 opening transaction
  expect_equal(nrow(xop_open), 1)

  # There is 1 closing transaction
  expect_equal(nrow(xop_close), 1)

  # There should be none dummy expired options
  expect_equal(nrow(xop_expired), 0)
})

test_that("Confirmation with assigned stock", {
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

  # Filter assigned stock
  assigned_stock <- ual %>%
    filter(instrument == "STOCK", reason == "ASSIGNED")

  # Filter "dummy" assigned option
  assigned_option <- ual %>%
    filter(instrument == "OPTION", reason == "ASSIGNED")

  # There are:
  # • 21 unsolicited transactions
  # • 1 dummy assigned option
  expect_equal(nrow(transactions), 21 + 1)

  # There are 10 unsolicited UAL transactions
  expect_equal(nrow(ual), 10)

  # There is 1 assigned option
  expect_equal(nrow(assigned_option), 1)

  # Check that values of the assigned options have correct values
  expect_true(assigned_option$trade_date == assigned_stock$trade_date)
  expect_true(assigned_option$action == "REMOVE")
  expect_true(assigned_option$position == "CLOSE")
  expect_true(assigned_option$quantity == assigned_stock$quantity/100L)
  expect_true(assigned_option$price == 0)
  expect_true(assigned_option$principal == 0)
  expect_true(assigned_option$commission == 0)
  expect_true(assigned_option$transaction_fee == 0)
  expect_true(assigned_option$additional_fee == 0)
  expect_true(assigned_option$net_amount == 0)
  expect_true(assigned_option$tag_number == assigned_stock$tag_number)
})

context("Confirmation - Option for a split stock block")

test_that("Buy To Close split option", {
  lines <- c(
    "2 B 01/05/18 01/08/18 1 0.0600000 6.00 0.00 0.04 0.10 F2625 6.14 TUA0108 5 1",
    "Desc: CALL UNG1 01/19/18 6 UNITED STATES NATRAL GAS FD LP ADJ 1:4 REV SPLIT DEL:25 UNG Interest/STTax: 0.00 CUSIP: 9LVZSV2",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: UNSOLICITED, CLOSING CONTRACT"
  )

  result   <- split_option_block$new()$probe(lines)
  expected <- tibble::tibble(transaction_id  = 1L+2L+3L+4L,
                             trade_date      = lubridate::mdy("01/05/18"),
                             reason          = as.reason("UNSOLICITED"),
                             action          = as.action("BUY"),
                             position        = as.position("CLOSE"),
                             symbol          = "UNG1",
                             instrument      = as.instrument("OPTION"),
                             quantity        = 1L,
                             option_type     = as.option_type("CALL"),
                             strike          = 6,
                             expiration_date = lubridate::mdy("01/19/18"),
                             price           = 0.06,
                             principal       = 6.00,
                             commission      = 0.00,
                             transaction_fee = 0.04,
                             additional_fee  = 0.10,
                             net_amount      = 6.14,
                             cusip           = "9LVZSV2",
                             tag_number      = "F2625")

  expect_identical(result, expected)
})

test_that("Read confirmation with a CLOSE split option", {
  confirmations_folder <- file.path(test_path(), "confirmations")

  confirmation_files <- c(
    file.path(confirmations_folder, "2018-01-05-1NE23456-confirmation.pdf")
  )

  transactions <- read_confirmations(confirmation_files)

  # Filter UNG options
  ung_options <- transactions %>%
    filter_by_symbol("UNG") %>%
    filter(instrument == "OPTION")

  expect_equal(nrow(ung_options), 3)
})


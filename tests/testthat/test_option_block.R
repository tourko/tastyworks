context("Confirmation - Option block")

test_that("Sell To Open option", {
  lines <- c(
    "2 S 08/30/17 08/31/17 1 0.7500000 75.00 1.00 0.06 0.10 E8470 73.84 TUA0831 1 1",
    "Desc: PUT XOP 10/20/17 28 SPDR S&P OIL&GAS EXPL & PRDN Interest/STTax: 0.00 CUSIP: 9H82162",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: UNSOLICITED, OPEN CONTRACT"
  )

  result   <- lines %>% option_block$probe()
  expected <- tibble::tibble(trade_date      = lubridate::mdy("08/30/17"),
                             reason          = as.reason("UNSOLICITED"),
                             action          = as.action("SELL"),
                             position        = as.position("OPEN"),
                             symbol          = "XOP",
                             instrument      = as.instrument("OPTION"),
                             quantity        = 1L,
                             option_type     = as.option_type("PUT"),
                             strike          = 28,
                             expiration_date = lubridate::mdy("10/20/17"),
                             price           = 0.75,
                             principal       = 75.00,
                             commission      = 1.00,
                             transaction_fee = 0.06,
                             additional_fee  = 0.10,
                             net_amount      = 73.84,
                             cusip           = "9H82162",
                             tag_number      = "E8470")

  expect_identical(result, expected)
})

test_that("Buy To Open option", {
  lines <- c(
    "2 B 08/30/17 08/31/17 1 1.1900000 119.00 1.00 0.04 0.10 Q9060 120.14 TUA0831 1 1",
    "Desc: PUT FB 10/20/17 155 FACEBOOK INC CL A Interest/STTax: 0.00 CUSIP: 8BWGYG3",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: UNSOLICITED, OPEN CONTRACT"
  )

  result   <- lines %>% option_block$probe()
  expected <- tibble::tibble(trade_date      = lubridate::mdy("08/30/17"),
                             reason          = as.reason("UNSOLICITED"),
                             action          = as.action("BUY"),
                             position        = as.position("OPEN"),
                             symbol          = "FB",
                             instrument      = as.instrument("OPTION"),
                             quantity        = 1L,
                             option_type     = as.option_type("PUT"),
                             strike          = 155,
                             expiration_date = lubridate::mdy("10/20/17"),
                             price           = 1.19,
                             principal       = 119.00,
                             commission      = 1.00,
                             transaction_fee = 0.04,
                             additional_fee  = 0.10,
                             net_amount      = 120.14,
                             cusip           = "8BWGYG3",
                             tag_number      = "Q9060")

  expect_identical(result, expected)
})

test_that("Buy To Close option", {
  lines <- c(
    "2 B 09/01/17 09/05/17 1 0.3900000 39.00 0.00 0.04 0.10 Y8611 39.14 TUA0905 1 1",
    "Desc: PUT XOP 10/20/17 28 SPDR S&P OIL&GAS EXPL & PRDN Interest/STTax: 0.00 CUSIP: 9H82162",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: UNSOLICITED, CLOSING CONTRACT"
  )

  result   <- lines %>% option_block$probe()
  expected <- tibble::tibble(trade_date      = lubridate::mdy("09/01/17"),
                             reason          = as.reason("UNSOLICITED"),
                             action          = as.action("BUY"),
                             position        = as.position("CLOSE"),
                             symbol          = "XOP",
                             instrument      = as.instrument("OPTION"),
                             quantity        = 1L,
                             option_type     = as.option_type("PUT"),
                             strike          = 28,
                             expiration_date = lubridate::mdy("10/20/17"),
                             price           = 0.39,
                             principal       = 39.00,
                             commission      = 0.00,
                             transaction_fee = 0.04,
                             additional_fee  = 0.10,
                             net_amount      = 39.14,
                             cusip           = "9H82162",
                             tag_number      = "Y8611")

  expect_identical(result, expected)
})

test_that("Sell To Close option", {
  lines <- c(
    "2 S 10/18/17 10/19/17 1 2.0000000 200.00 0.00 0.06 0.10 C3380 199.84 TUA1019 1 1",
    "Desc: CALL CMG 10/20/17 325 CHIPOTLE MEXICAN GRILL INC Interest/STTax: 0.00 CUSIP: 8GGNQB5",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: UNSOLICITED, CLOSING CONTRACT"
  )

  result   <- lines %>% option_block$probe()
  expected <- tibble::tibble(trade_date      = lubridate::mdy("10/18/17"),
                             reason          = as.reason("UNSOLICITED"),
                             action          = as.action("SELL"),
                             position        = as.position("CLOSE"),
                             symbol          = "CMG",
                             instrument      = as.instrument("OPTION"),
                             quantity        = 1L,
                             option_type     = as.option_type("CALL"),
                             strike          = 325,
                             expiration_date = lubridate::mdy("10/20/17"),
                             price           = 2.00,
                             principal       = 200.00,
                             commission      = 0.00,
                             transaction_fee = 0.06,
                             additional_fee  = 0.10,
                             net_amount      = 199.84,
                             cusip           = "8GGNQB5",
                             tag_number      = "C3380")

  expect_identical(result, expected)
})

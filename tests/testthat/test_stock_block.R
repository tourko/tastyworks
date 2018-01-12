context("Confirmation - Stock block")

test_that("Buy To Open stock", {
  lines <- c(
    "2 B 12/15/17 12/19/17 100 UNG 5.3100000 531.00 5.00 0.00 0.08 I2680 536.08 TUA1219 6 1",
    "Desc: UNITED STATES NATURAL GAS FUND LP UNIT USD0.001 Interest/STTax: 0.00 CUSIP: 912318201",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: UNSOLICITED, PRODUCT DESCRIPTION UNDER SEPARATE COVER"
  )

  result   <- lines %>% stock_block$probe()
  expected <- tibble::tibble(transaction_id  = 1L+2L+3L+4L,
                             trade_date      = lubridate::mdy("12/15/17"),
                             reason          = as.reason("UNSOLICITED"),
                             action          = as.action("BUY"),
                             position        = as.position("OPEN"),
                             symbol          = "UNG",
                             instrument      = as.instrument("STOCK"),
                             quantity        = 100L,
                             price           = 5.31,
                             principal       = 531.00,
                             commission      = 5.00,
                             transaction_fee = 0.00,
                             additional_fee  = 0.08,
                             net_amount      = 536.08,
                             cusip           = "912318201",
                             tag_number      = "I2680")

  expect_identical(result, expected)
})

test_that("Sell To Close stock", {
  lines <- c(
    "2 S 12/08/17 12/12/17 100 UAL 64.3801000 6,438.01 0.00 0.16 0.08 T2198 6,437.77 TUA1212 6 1",
    "Desc: UNITED CONTINENTAL HLDGS INC Interest/STTax: 0.00 CUSIP: 910047109",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: UNSOLICITED"
  )

  result   <- lines %>% stock_block$probe()
  expected <- tibble::tibble(transaction_id  = 1L+2L+3L+4L,
                             trade_date      = lubridate::mdy("12/08/17"),
                             reason          = as.reason("UNSOLICITED"),
                             action          = as.action("SELL"),
                             position        = as.position("CLOSE"),
                             symbol          = "UAL",
                             instrument      = as.instrument("STOCK"),
                             quantity        = 100L,
                             price           = 64.3801,
                             principal       = 6438.01,
                             commission      = 0.00,
                             transaction_fee = 0.16,
                             additional_fee  = 0.08,
                             net_amount      = 6437.77,
                             cusip           = "910047109",
                             tag_number      = "T2198")

  expect_identical(result, expected)
})

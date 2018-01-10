context("Confirmation - Exercised block")

test_that("Excercised Call option", {
  lines <- c(
    "2 S 11/15/17 11/16/17 1 1.7900000 179.00 5.00 0.00 0.00 15056 174.00 TUA1116 8 1",
    "Desc: CALL VIX 11/15/17 12 CBOE VOLATILITY INDEX Interest/STTax: 0.00 CUSIP: 8BKQXT2",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: A/E 8BKQXT2 1 EXERCISED, CLOSING CONTRACT"
  )

  result   <- lines %>% exercised_block$probe()
  expected <- tibble::tibble(trade_date      = lubridate::mdy("11/15/17"),
                             reason          = as.reason("EXERCISED"),
                             action          = as.action("SELL"),
                             position        = as.position("CLOSE"),
                             symbol          = "VIX",
                             instrument      = as.instrument("OPTION"),
                             quantity        = 1L,
                             option_type     = as.option_type("CALL"),
                             strike          = 12,
                             expiration_date = lubridate::mdy("11/15/17"),
                             price           = 1.79,
                             principal       = 179.00,
                             commission      = 5.00,
                             transaction_fee = 0.00,
                             additional_fee  = 0.00,
                             net_amount      = 174.00,
                             cusip           = "8BKQXT2",
                             tag_number      = "15056")

  expect_identical(result, expected)
})

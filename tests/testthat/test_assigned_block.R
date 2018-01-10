context("Confirmation - Assigned block")

test_that("Assigned long stock", {
  lines <- c(
    "2 B 11/09/17 11/13/17 100 UAL 65.0000000 6,500.00 5.00 0.00 0.00 68332 6,505.00 TUA1113 3 1",
    "Desc: UNITED CONTINENTAL HLDGS INC Interest/STTax: 0.00 CUSIP: 910047109",
    "Currency: USD ReportedPX: MarkUp/Down:",
    "Trailer: A/E 9KJTQT2 1 ASSIGNED"
  )

  result   <- lines %>% assigned_block$probe()
  expected <- tibble::tibble(trade_date      = lubridate::mdy("11/09/17"),
                             reason          = as.reason("ASSIGNED"),
                             action          = as.action("BUY"),
                             position        = as.position("OPEN"),
                             symbol          = "UAL",
                             instrument      = as.instrument("STOCK"),
                             quantity        = 100L,
                             price           = 65.00,
                             principal       = 6500.00,
                             commission      = 5.00,
                             transaction_fee = 0.00,
                             additional_fee  = 0.00,
                             net_amount      = 6505.00,
                             cusip           = "910047109",
                             tag_number      = "68332",
                             assigned_cusip  = "9KJTQT2",
                             assigned_qty    = 1L)

  expect_identical(result, expected)
})

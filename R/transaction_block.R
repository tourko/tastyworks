transaction_block <- R6::R6Class("TransactionBlock", inherit = block,
  private = list(
    augment = function(tokens) {
      tokens %>%
        # Use .lines to construct transaction_id
        dplyr::mutate(transaction_id = .lines %>% purrr::map_int(~ sum(.x))) %>%
        # Drop ".lines" column
        dplyr::select(-.lines) %>%
        # Convert string values in the columns to the appropriate data types
        dplyr::mutate(
          # Convert to factor
          action          = as.action(action),
          trade_date      = lubridate::mdy(trade_date),
          quantity        = as.integer(quantity),
          price           = as.numeric(price),
          # Drop "," as thuosands separator and convert to numeric
          principal       = as.numeric(stringr::str_replace_all(principal, ",", "")),
          commission      = as.numeric(commission),
          transaction_fee = as.numeric(transaction_fee),
          additional_fee  = as.numeric(additional_fee),
          # Drop "," as thuosands separator and convert to numeric
          net_amount      = as.numeric(stringr::str_replace_all(net_amount, ",", "")),
          # Convert to factor
          reason          = as.reason(reason)
        )
    }
  )
)

transactions <- new.env()

transactions$read <- function(files) {
  files %>%
    # Read each confirmtaion.
    # The result is a list (1) with as many elements as there are files.
    # Each element in list (1) is yet another a list (2).
    # Each element in list (2) is a tibble that has transactions extracted by a transaction block.
    # List (2) has as many elements as there are transaction blcks.
    #
    # Example:
    # List of 70
    # $ :List of 4
    # ..$ option   :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	5 obs. of  19 variables:
    # .....
    # $ :List of 4
    # ..$ stock    :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	4 obs. of  16 variables:
    # .....
    purrr::map(confirmation$read) %>%
    # Turn the list "inside-out", so that it will look like the following:
    # List of 4:
    # $ option :List of 70
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    # $ stock :List of 70
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    # $ assigned :List of 70
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    # $ exercised :List of 70
    #  ..$ :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	0 obs. of  0 variables
    #  ......
    purrr::transpose() %>%
    # Bind the rows in each transaction block together:
    # $option
    # # A tibble: 406 x 19
    #   transaction_id trade_date      reason position buy_sell symbol instrument ...
    #            <chr>     <date>      <fctr>   <fctr>   <fctr>  <chr>     <fctr> ...
    # 1         T00009 2017-08-30 UNSOLICITED     OPEN     SELL    XOP     OPTION ...
    # 2         T00013 2017-08-30 UNSOLICITED     OPEN      BUY     FB     OPTION ...
    # ...
    #
    # $stock
    # # A tibble: 2 x 16
    #   transaction_id trade_date      reason position buy_sell symbol instrument ...
    #            <chr>     <date>      <fctr>   <fctr>   <fctr>  <chr>     <fctr> ...
    # 1         T00013 2017-12-08 UNSOLICITED    CLOSE     SELL    UAL      STOCK ...
    # 2         T00046 2017-12-15 UNSOLICITED     OPEN      BUY    UNG      STOCK ...
    # ...
    #
    # $assigned
    # # A tibble: 2 x 18
    #   transaction_id trade_date   reason position buy_sell symbol instrument ...
    #            <chr>     <date>   <fctr>   <fctr>   <fctr>  <chr>     <fctr> ...
    # 1         T00009 2017-11-09 ASSIGNED     OPEN      BUY    UAL      STOCK ...
    # 2         T00025 2017-11-21 ASSIGNED     OPEN      BUY     GE      STOCK ...
    # ...
    #
    # $exercised
    # # A tibble: 1 x 19
    #     transaction_id trade_date    reason position buy_sell symbol instrument ...
    #              <chr>     <date>    <fctr>   <fctr>   <fctr>  <chr>     <fctr> ...
    #   1         T00017 2017-11-15 EXERCISED    CLOSE     SELL    VIX     OPTION ...
    # ...
    purrr::map(~ .x %>% purrr::map_dfr(~ .x))
}

transactions$process_assigned <- function(blocks) {
  # Check if there are any assigned transactions
  if ( !purrr::is_empty(blocks$assigned) ) {
    assigned_stocks <- blocks$assigned

    # For each assigned stock create a "dummy" option transaction
    assigned_options <- blocks$option %>%
      dplyr::filter(cusip %in% blocks$assigned$assigned_cusip) %>%
      dplyr::mutate(
        transaction_id  = assigned_stocks$transaction_id,
        trade_date      = assigned_stocks$trade_date,
        reason          = assigned_stocks$reason,
        position        = as.position("CLOSE"),
        action          = as.action("REMOVE"),
        quantity        = assigned_stocks$assigned_qty,
        price           = 0,
        principal       = 0,
        commission      = 0,
        transaction_fee = 0,
        additional_fee  = 0,
        net_amount      = 0,
        tag_number      = assigned_stocks$tag_number)

    # Append assigned options to the option transaction block
    blocks$option <- blocks$option %>%
      dplyr::bind_rows(assigned_options)

    # Drop "assigned_qty" and "assigned_cusip" from assigned stock transactions
    blocks$assigned <- blocks$assigned %>%
      dplyr::select(-assigned_qty, -assigned_cusip)
  }

  return(blocks)
}

transactions$process_expired <- function(blocks) {
  # Find CUSIPs that have greater OPEN quantity than CLOSE quantity
  open_gt_close <- blocks$option %>%
    # Select "cusip", "position" and "quantity" columns
    dplyr::select(cusip, position, quantity) %>%
    # Group by CUSIP and position (OPEN and CLOSE)
    dplyr::group_by(cusip, position) %>%
    # Sum the quantities for each CUSIP and position
    # A tibble: 10 x 3
    # Groups:   cusip [?]
    #     cusip position quantity
    #     <chr>   <fctr>    <int>
    # 1 8BWGYG3     OPEN        1
    # 2 8BWGYG3    CLOSE        1
    # 3 8BWGYJ1     OPEN        1
    # 4 8BWGYJ1    CLOSE        1
    # .     ...      ...        .
    dplyr::summarise(quantity = sum(quantity)) %>%
    # Convert to a data frame with the OPEN and CLOSE vs. quantities for each CUSIP
    #      cusip  OPEN CLOSE
    # *   <chr>  <int> <int>
    # 1 8BKQXT2     1     1
    # 2 8BRRSQ6     1     1
    # 3 8BRTKX9     1     1
    # 4 8BRTNB5     0     1
    # .     ...     .     .
    tidyr::spread(position, quantity, fill = 0) %>%
    # Add OPEN and/or CLOSE columns if they don't exist
    dplyr::mutate(OPEN  = if (exists("OPEN",  where = .)) as.integer(OPEN)  else 0L) %>%
    dplyr::mutate(CLOSE = if (exists("CLOSE", where = .)) as.integer(CLOSE) else 0L) %>%
    # Find CUSIPs that have greater OPEN quantity than CLOSE quantity
    dplyr::filter(OPEN > CLOSE)

  # Get options that expired and for which number of OPEN'ed contracts
  # is greater than the number of CLOSE'ed contracts.
  expired_options <- blocks$option %>%
    dplyr::filter(cusip %in% open_gt_close$cusip, position == "OPEN", expiration_date < lubridate::today())

  # Check if there are any expired options
  if ( !purrr::is_empty(expired_options) ) {
    # For each expired option create a "dummy" option transaction
    expired_options_to_remove <- expired_options %>%
      dplyr::mutate(
        # Append "000" to the "transaction_id", so that these "dummy" transactions
        # will be listed after the normal transactions.
        transaction_id  = expired_options$transaction_id * 1000L,
        trade_date      = expired_options$expiration_date,
        reason          = as.reason("EXPIRED"),
        position        = as.position("CLOSE"),
        action          = as.action("REMOVE"),
        quantity        = expired_options$quantity,
        price           = 0,
        principal       = 0,
        commission      = 0,
        transaction_fee = 0,
        additional_fee  = 0,
        net_amount      = 0,
        tag_number      = expired_options$tag_number)

    # Append expired options to the option transaction block
    blocks$option <- blocks$option %>%
      dplyr::bind_rows(expired_options_to_remove)
  }

  return(blocks)
}

transactions$process_split_options <- function(blocks) {
  # Check if there are any split options
  if ( !purrr::is_empty(blocks$split_option) ) {
    split_options <- blocks$split_option

    # For now we can only handle "CLOSE" options
    if ( split_options %>% dplyr::filter(position == "OPEN") %>% nrow() > 0 ) {
      stop("OPEN options after stock split are not supported.")
    }

    # For each CLOSE option after split create an opposite dummy OPEN transaction
    open_after_split <- split_options %>%
      dplyr::mutate(
        transaction_id  = transaction_id - 1L,
        reason          = as.reason("SPLIT"),
        position        = as.position("OPEN"),
        action          = as.action(dplyr::if_else(action == "BUY", "SELL", "BUY")),
        commission      = 0,
        transaction_fee = 0,
        additional_fee  = 0,
        net_amount      = principal
      )

    # Append dummy open options to split_option block
    blocks$split_option <- blocks$split_option %>%
      dplyr::bind_rows(open_after_split)

    # For each OPEN dummy option after split create an opposite dummy CLOSE option before split
    close_before_split <- open_after_split %>%
      dplyr::mutate(
        transaction_id  = transaction_id,
        position        = as.position("CLOSE"),
        action          = as.action(dplyr::if_else(action == "BUY", "SELL", "BUY")),
        # Drop trailing digit from the symbol
        symbol          = stringr::str_replace(symbol, pattern = capture(one_or_more(UPPER)) %R% DGT, replacement = "\\1")
      )

    # For each dummy close_before_split we need to set cusip to that of the real option transaction
    for (idx in seq_len(nrow(close_before_split))) {
      r <- close_before_split[idx, ]
      open_before_split <- blocks$option %>%
        dplyr::filter(
          trade_date < r$trade_date,
          reason == "UNSOLICITED",
          action == dplyr::if_else(r$action == "BUY", "SELL", "BUY"),
          position == "OPEN",
          symbol == r$symbol,
          option_type == r$option_type,
          strike == r$strike,
          expiration_date == r$expiration_date
        )

      # There must be exactly one open transaction before split
      if (nrow(open_before_split) == 1) {
        close_before_split[idx, "cusip"] = open_before_split$cusip
      } else {
        close_before_split[idx, "cusip"] = NA
      }
    }

    # Append dummy close options to option block
    blocks$option <- blocks$option %>%
      dplyr::bind_rows(close_before_split)
  }

  return(blocks)
}

transactions$merge <- function(blocks) {
  blocks %>%
    # Merge transaction from all transaction blocks into one tibble
    dplyr::bind_rows() %>%
    # Arrange transactions in the same order as they appear in the confirmations
    dplyr::arrange(trade_date, transaction_id) %>%
    # No more need for "transaction_id"
    dplyr::select(-transaction_id)
}

filter_by_symbol <- function(transactions, symbol) {
  # If a stock was split, it will have the same symbol followed by a digit
  pattern = symbol %R% optional(DGT)
  transactions %>% dplyr::filter(stringr::str_detect(transactions$symbol, pattern))
}

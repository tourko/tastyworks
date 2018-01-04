#' Extract transactions from Tastywork's confirmation file(s)
#'
#' This function reads Tastywork's confirmation files, which are in PDF format,
#' extracts transactions from each file and merges them together in a single data frame.
#'
#' @param files a vector of path names to the files.
#' @return The output is a data frame, where each record represents one transaction.
#'
#' @examples
#' \dontrun{
#' ## Read a single confimation file at once
#' transactions <- read_confirmations("confirmations/2017-08-30-1NE23456-confirmation.pdf")
#'
#'
#' ## Read several confirmation files
#' files <- c("confirmations/2017-08-30-1NE23456-confirmation.pdf",
#'            "confirmations/2017-08-30-1NE23456-confirmation.pdf")
#' transactions <- read_confirmations(files)
#'
#'
#' ## Read all confimation files matching a template
#' # A template for confirmation file name
#' confirmation_pattern <- START %R% YMD %R% "-" %R%
#'  repeated(ALNUM, 8) %R% "-confirmation" %R%
#'  DOT %R% "pdf" %R% END
#'
#' # Get a list of confirmation files
#' files <- list.files(path = path.expand("confirmations"),
#'                    pattern = confirmation_pattern,
#'                    full.names = TRUE)
#' transactions <- read_confirmations(files)
#' }
#'
#' @export
read_confirmations <- function(files) {
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
    purrr::map(process_confirmation) %>%
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
    purrr::transpose(.names = names(transaction_blocks)) %>%
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
    purrr::map(~ .x %>% purrr::map_dfr(~ .x)) %>%
    # Process assigned stocks
    process$assigned() %>%
    # Process expired options
    process$expired() %>%
    # Merge transaction from all transaction blocks into one tibble
    dplyr::bind_rows() %>%
    # Arrange transactions in the same order as they appear in the confirmations
    dplyr::arrange_(~trade_date, ~transaction_id) %>%
    # No more need for "transaction_id"
    # Use one_of() to suppress "no visible binding for global variable" note
    dplyr::select(-dplyr::one_of("transaction_id"))
}

# Put functions in the list to suppress "no visible binding for global variable" note
process <- list(
  assigned = function(transactions) {
    # Check if there are any assigned transactions
    if ( !purrr::is_empty(transactions$assigned) ) {
      assigned_stocks <- transactions$assigned

      # For each assigned stock create a "dummy" option transaction
      assigned_options <- transactions$option %>%
        dplyr::filter(cusip %in% transactions$assigned$assigned_cusip) %>%
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
      transactions$option <- transactions$option %>%
        dplyr::bind_rows(assigned_options)

      # Drop "assigned_qty" and "assigned_cusip" from assigned stock transactions
      transactions$assigned <- transactions$assigned %>%
        dplyr::select(-assigned_qty, -assigned_cusip)
    }

    return(transactions)
  },

  expired = function(transactions) {
    # Find CUSIPs that have greater OPEN quantity than CLOSE quantity
    open_gt_close <- transactions$option %>%
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
    expired_options <- transactions$option %>%
      dplyr::filter(cusip %in% open_gt_close$cusip, position == "OPEN", expiration_date < lubridate::today())

    # Check if there are any expired options
    if ( !purrr::is_empty(expired_options) ) {
      # For each expired option create a "dummy" option transaction
      expired_options_to_remove <- expired_options %>%
        dplyr::mutate(
          # Append "0" to the "transaction_id", so that these "dummy" transactions
          # will be listed after the normal transactions.
          transaction_id  = stringr::str_c(expired_options$transaction_id, "0"),
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
      transactions$option <- transactions$option %>%
        dplyr::bind_rows(expired_options_to_remove)
    }

    return(transactions)
  }
)

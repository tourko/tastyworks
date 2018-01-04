# Put functions in the list to suppress "no visible binding for global variable" note
integrity_func <- list (
  find_orphants = function(transactions) {
    # Find CUSIPs that have greater CLOSE quantity than OPEN quantity
    close_gt_open <- transactions %>%
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
      # Find CUSIPs that have greater CLOSE quantity than OPEN quantity
      dplyr::filter(CLOSE > OPEN)

    if (nrow(close_gt_open) > 0) {
      message("There are CLOSE transactions that have greater quantity than OPEN transactions.")
    }

    transactions %>% dplyr::filter(cusip %in% close_gt_open$cusip)
})


#' Find orphanted transactions
#'
#' This function finds closing transactions that do not have corresponding opening transactions
#' or where the quantity of closing transactions exceeds that of the opening transactions,
#' which also indicates that some the opening transactions might be missing.
#'
#' @param transactions a data frame with the transactions to check.
#' @return The output is a data frame with the orphanted closing transactions.
#'
#' @examples
#' \dontrun{
#' find_orphants(transactions)
#' }
#'
#' @export
find_orphants <- function(transactions) {
  integrity_func$find_orphants(transactions)
}

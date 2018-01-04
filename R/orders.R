# Put functions in the list to suppress "no visible binding for global variable" note
order_func <- list(
    assemble_orders = function(transactions) {
    # Order is a collection of transactions executed on the same date,
    # for the same symbol and with the same first letter for the tag_number.
    #
    orders <- transactions %>%
      # Get the first letter of the "tag_number"
      dplyr::mutate(tag_letter = stringr::str_sub(tag_number, 1, 1)) %>%
      # "Wrap" transactions by "trade_date", "symbol" and the first letter of the "tag_number"
      tidyr::nest(-trade_date, -symbol, -reason, -tag_letter, .key = "transactions") %>%
      # Add "order_id" column
      dplyr::mutate(order_id = stringr::str_c("R", stringr::str_pad(row_number(), 5, pad = "0"))) %>%
      # Put "order_id" in front and drop "tag_letter"
      dplyr::select(order_id, dplyr::everything(), -tag_letter)

    #
    # Classify orders as either OPEN, CLOSE or ROLL.
    #
    # If all transactions in an order are OPEN, then it is an OPEN order.
    # If all transactions in an order are CLOSE, then it is a CLOSE order.
    # If there is a mix of OPEN and CLOSE transactions in an order, then it is a ROLL order.

    orders <- orders %>%
      # "Unwrap" transactions, because we would need "position" variable from the transactions.
      tidyr::unnest(transactions) %>%
      # Create a contigency matrix that shows relations between "order_id" and "position".
      # Rows contain "order_id", columns contain "position":
      #          position
      # order_id OPEN CLOSE
      # R00001    1     0
      # R00002    4     0
      # R00003    4     0
      # R00004    0     1
      # R00005    2     0
      # ......   ...   ...
      xtabs(~ order_id + position, data = .) %>%
      # Convert to a tibble:
      # A tibble: 332 x 3
      #   order_id   position n_legs
      #      <chr>      <chr>  <int>
      # 1   R00001       OPEN      1
      # 2   R00002       OPEN      4
      # 3   R00003       OPEN      4
      # 4   R00004       OPEN      0
      # 5   R00005       OPEN      2
      # .   ......       ....    ...
      dplyr::as_tibble(n = "n_legs") %>%
      # Spread "position" into two columns: CLOSE and OPEN
      # A tibble: 166 x 3
      #   order_id CLOSE  OPEN
      # *    <chr> <int> <int>
      # 1   R00001     0     1
      # 2   R00002     0     4
      # 3   R00003     0     4
      # 4   R00004     1     0
      # 5   R00005     0     2
      # .   ......   ...   ...
      tidyr::spread(position, n_legs) %>%
      # Convert OPEN and CLOSE to logical: 0 - FALSE; non-0 - TRUE
      # A tibble: 166 x 3
      #   order_id CLOSE  OPEN
      # <chr> <lgl> <lgl>
      # 1   R00001 FALSE  TRUE
      # 2   R00002 FALSE  TRUE
      # 3   R00003 FALSE  TRUE
      # 4   R00004  TRUE FALSE
      # 5   R00005 FALSE  TRUE
      # .   ......   ...   ...
      dplyr::mutate(CLOSE = as.logical(CLOSE), OPEN = as.logical(OPEN)) %>%
      # Add ROLL column, which is TRUE if both CLOSE and OPEN, otherwise FALSE
      # Also set OPEN and CLOSE to FALSE, where ROLL is TRUE: xor(..., ROLL)
      # A tibble: 166 x 4
      #   order_id CLOSE  OPEN  ROLL
      #     <chr>  <lgl> <lgl> <lgl>
      # 1   R00001 FALSE  TRUE FALSE
      # 2   R00002 FALSE  TRUE FALSE
      # 3   R00003 FALSE  TRUE FALSE
      # 4   R00004  TRUE FALSE FALSE
      # 5   R00005 FALSE  TRUE FALSE
      # .   ......   ...   ...   ...
      dplyr::mutate(ROLL = CLOSE & OPEN, OPEN = xor(OPEN, ROLL), CLOSE = xor(CLOSE, ROLL)) %>%
      # Gather OPEN, CLOSE and ROLL colums into key "order_type" and value "v".
      # Filter out rows that have FALSE in "v" and drop "v" column.
      tidyr::gather(key = order_type, value = v, -order_id) %>% dplyr::filter(v) %>% dplyr::select(-v) %>%
      # Finally, convert "order_type" to a factor and sort by "order_id":
      # A tibble: 166 x 2
      #   order_id order_type
      #      <chr>      <chr>
      # 1   R00001       OPEN
      # 2   R00002       OPEN
      # 3   R00003       OPEN
      # 4   R00004      CLOSE
      # 5   R00005       OPEN
      # .   ......        ...
      dplyr::mutate(order_type = factor(order_type, levels = c("OPEN", "CLOSE", "ROLL"))) %>% dplyr::arrange(order_id) %>%
      # Add "order_type" to the "orders" dataframe by joining the two
      dplyr::inner_join(orders, by = "order_id") %>%
      # Put "order_type" column after "trade_date"
      dplyr::select(order_id, trade_date, reason, order_type, dplyr::everything())


    #
    # Calculate credit/debit, commissions and fees for each order
    #
    orders <- orders %>%
      # Add "credit_debit" variable
      dplyr::mutate(credit_debit =
               # Itterate through the transactions in a given order
               orders$transactions %>%
               # Sum up principles multiplied by either -1 or 1 depending on whether it is BUY or SELL transaction
               purrr::map_dbl( ~ sum(.$principal * dplyr::if_else(.$action == "BUY", -1, 1) ) )
      ) %>%
      # Add "commissions" variable
      dplyr::mutate(commissions =
               # Itterate through the transactions in a given order
               orders$transactions %>%
               # Sum up commission
                 purrr::map_dbl( ~ -sum(.$commission) )
      ) %>%
      # Add "fees" variable
      dplyr::mutate(fees =
               # Itterate through the transactions in a given order
               orders$transactions %>%
               # Sum up transaction and additional fee
                 purrr::map_dbl( ~ -sum(.$transaction_fee + .$additional_fee) )
      ) %>%
      # Reorder columns
      dplyr::select(order_id, trade_date, reason, order_type, symbol, credit_debit:fees, transactions)

    return(orders)
})

#' Assemble transactions into orders.
#'
#' This function groups the transactions into orders and calculates credit/debit,
#' commissions and fees per order.
#'
#' @param transactions a data frame with the transactions.
#' @return The output is a data frame with the orders.
#'
#' @examples
#' \dontrun{
#' assemble_orders(transactions)
#' }
#'
#' @export
assemble_orders <- function(transactions) {
  order_func$assemble_orders(transactions)
}

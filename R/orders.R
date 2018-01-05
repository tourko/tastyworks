orders <- new.env()

orders$create <- function(transactions) {
  # Order is a collection of transactions executed on the same date,
  # for the same symbol and with the same first letter for the tag_number.
  transactions %>%
    # Get the first letter of the "tag_number"
    dplyr::mutate(tag_letter = stringr::str_sub(tag_number, 1, 1)) %>%
    # "Wrap" transactions by "trade_date", "symbol" and the first letter of the "tag_number"
    tidyr::nest(-trade_date, -reason, -symbol, -tag_letter, .key = "transactions") %>%
    # Drop "tag_letter"
    dplyr::select(-tag_letter) %>%
    # Add "order_id" column
    tibble::rowid_to_column(var = "order_id")
}

orders$classify <-  function(orders) {
  # If all transactions in an order are OPEN, then it is an OPEN order.
  # If all transactions in an order are CLOSE, then it is a CLOSE order.
  # If there is a mix of OPEN and CLOSE transactions in an order, then it is a ROLL order.
  orders %>%
    # "Unwrap" transactions, because we would need "position" variable from the transactions.
    tidyr::unnest(transactions) %>%
    # Create a contigency matrix that shows relations between "order_id" and "position".
    # Rows contain "order_id", columns contain "position":
    #            position
    #   order_id OPEN CLOSE
    # 1        1    1     0
    # 2        2    4     0
    # 3        3    4     0
    # 4        4    0     1
    # 5        5    2     0
    # ......   ...   ...
    xtabs(~ order_id + position, data = .) %>%
    # Convert to a tibble:
    # A tibble: 332 x 3
    #   order_id   position n_legs
    #      <int>      <chr>  <int>
    # 1        1       OPEN      1
    # 2        2       OPEN      4
    # 3        3       OPEN      4
    # 4        4       OPEN      0
    # 5        5       OPEN      2
    # .   ......       ....    ...
    dplyr::as_tibble(n = "n_legs") %>%
    # Convert "order_id" to integer
    dplyr::mutate(order_id = as.integer(order_id)) %>%
    # Spread "position" into two columns: CLOSE and OPEN
    # A tibble: 166 x 3
    #   order_id CLOSE  OPEN
    # *    <int> <int> <int>
    # 1        1     0     1
    # 2        2     0     4
    # 3        3     0     4
    # 4        4     1     0
    # 5        5     0     2
    # .   ......   ...   ...
    tidyr::spread(position, n_legs) %>%
    # Convert OPEN and CLOSE to logical: 0 - FALSE; non-0 - TRUE
    # A tibble: 166 x 3
    #   order_id CLOSE  OPEN
    #      <int> <lgl> <lgl>
    # 1        1 FALSE  TRUE
    # 2        2 FALSE  TRUE
    # 3        3 FALSE  TRUE
    # 4        4  TRUE FALSE
    # 5        5 FALSE  TRUE
    # .   ......   ...   ...
    dplyr::mutate(CLOSE = as.logical(CLOSE), OPEN = as.logical(OPEN)) %>%
    # Add ROLL column, which is TRUE if both CLOSE and OPEN, otherwise FALSE
    # Also set OPEN and CLOSE to FALSE, where ROLL is TRUE: xor(..., ROLL)
    # A tibble: 166 x 4
    #   order_id CLOSE  OPEN  ROLL
    #      <int> <lgl> <lgl> <lgl>
    # 1        1 FALSE  TRUE FALSE
    # 2        2 FALSE  TRUE FALSE
    # 3        3 FALSE  TRUE FALSE
    # 4        4  TRUE FALSE FALSE
    # 5        5 FALSE  TRUE FALSE
    # .   ......   ...   ...   ...
  dplyr::mutate(ROLL = CLOSE & OPEN, OPEN = xor(OPEN, ROLL), CLOSE = xor(CLOSE, ROLL)) %>%
    # Gather OPEN, CLOSE and ROLL colums into key "order_type" and value "v".
    # Filter out rows that have FALSE in "v" and drop "v" column.
    tidyr::gather(key = order_type, value = v, -order_id) %>% dplyr::filter(v) %>% dplyr::select(-v) %>%
    # Finally, convert "order_type" to a factor and sort by "order_id":
    # A tibble: 166 x 2
    #   order_id order_type
    #      <int>      <chr>
    # 1        1       OPEN
    # 2        2       OPEN
    # 3        3       OPEN
    # 4        4      CLOSE
    # 5        5       OPEN
    # .   ......        ...
    dplyr::mutate(order_type = factor(order_type, levels = c("OPEN", "CLOSE", "ROLL"))) %>%
    # Append "order_type" to the orders
    inner_join(orders, by = "order_id") %>%
    select(order_id, trade_date, order_type, everything())
}
orders$summarise <-  function(orders) {
  orders %>%
    # Add "credit_debit" variable
    dplyr::mutate(credit_debit = .$transactions %>%
                    # Sum up principles multiplied by either -1 or 1 depending on whether it is BUY or SELL transaction
                    purrr::map_dbl( ~ sum(.$principal * dplyr::if_else(.$action == "BUY", -1, 1) ) )
    ) %>%
    # Add "commissions" variable
    dplyr::mutate(commissions = .$transactions %>%
                    # Sum up commission
                    purrr::map_dbl( ~ -sum(.$commission) )
    ) %>%
    # Add "fees" variable
    dplyr::mutate(fees =  .$transactions %>%
                    # Sum up transaction and additional fee
                    purrr::map_dbl( ~ -sum(.$transaction_fee + .$additional_fee) )
    ) %>%
    select(order_id:symbol, credit_debit:fees, everything())
}
orders$chain <-  function(orders) {
  # Create a contigency matrix that shows relations between orders and CUSIPs.
  # Rows contain "order_id", columns contain "cusip":
  #          cusip
  # order_id 369604103 8BKQXT2 8BRRSQ6 8BRTKX9 8BRTNB5 ...
  #    1         0       0       0       0       0     ...
  #    2         0       0       0       0       0     ...
  #    3         0       0       0       0       0     ...
  #    4         0       0       0       0       0     ...
  #    5         0       0       0       0       0     ...
  #   ...       ...     ...     ...     ...     ...    ...
  m <- orders %>%
    # "Unwrap" transactions, because we would need "cusip" variable from the transactions.
    tidyr::unnest(transactions) %>%
    # Create the contigency matrix described above.
    xtabs(~ order_id + cusip, data = .)

  chains <-
    # Build a list of matrixes. Each matrix only includes orders related via transaction CUSIPs:
    #
    # [[1]]
    #          cusip
    # order_id 9H82162
    #        1       1
    #        4       1
    #
    # [[2]]
    #          cusip
    # order_id 8BWGYG3 8BWGYH3 8BWGYJ1 9BWGYG5 9BWGYG6 9BWGYH8
  #        2       1       0       1       1       0       1
  #       40       0       1       1       0       1       1
  #       57       1       0       0       1       0       0
  #       88       0       1       0       0       1       0
  #
  # [[3]]
  #          cusip
  # order_id 8H54976 8H55643 9H55256 9H55257
  #        3       1       1       1       1
  #       43       0       0       1       1
  #       66       0       1       0       0
  #
  # [[4]]
  #          cusip
  # order_id 9H82162
  #        1       1
  #        4       1
  #
  # ...
  #
  # Itterate through the rows of the contigency matrix.
  # Use index for the rows, so that we can get a row as matrix rather than as a vector.
  1:nrow(m) %>% purrr::map(function(i) {
    # Get a row as a matrix
    r <- m[              i,                , drop = FALSE]

    # Initial number of rows
    n = 1
    repeat {
      # Get columns (in the selected row(s)), which contain at least one non-0 value
      r <- m[               , colSums(r) != 0, drop = FALSE]
      # Get rows (in the selected column(s)), which contain at least one non-0 value
      r <- m[rowSums(r) != 0,                , drop = FALSE]
      # Get number of rows found
      k = nrow(r)
      # If number of rows increased compared to the previous itteration,
      # then we found new relations.
      if (k > n) {
        # Update row count and repeat the loop again.
        n = k
      } else {
        # No more rows found, hence we already found all related rows (i.e. orders).
        break
      }
    }

    r <- r[rowSums(r) != 0, colSums(r) != 0, drop = FALSE]
  }) %>%
    # The list may contain duplicated matrixes. From the example above, the orders "R00001" and "R00004" are related,
    # So we'll get identical matrixes, for rows "R00001" and "R00004" in the contigency matrix.
    # Leave only unique matrixes in the list.
    unique() %>%
    # Replace each matrix in the list with a tibble with "order_id" columns
    # [[1]]
    # # A tibble: 2 x 1
    #   order_id
    #      <chr>
    # 1        1
    # 2        4
    #
    # [[2]]
    # # A tibble: 4 x 1
    #   order_id
  #      <chr>
  # 1        2
  # 2       40
  # 3       57
  # 4       88
  #
  # ...
  #
  # "order_id" comes from the row names of the matrixes
  #
  purrr::map(~ dplyr::tibble(order_id = rownames(.x))) %>%
    # Convert "order_id" to integer
    purrr::map(~ .x %>% dplyr::mutate(order_id = as.integer(order_id))) %>%
    # Put real orders in place of "order_id" by joining with "orders" data frame
    purrr::map(~ dplyr::inner_join(.x, orders, by = "order_id"))

  # Names elements in the chain list: CHAIN_*
  names(chains) <- stringr::str_c("CHAIN", stringr::str_pad(seq_along(chains), stringr::str_length(length(chains)), pad = "0"), sep = "_")

  return(chains)
}

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
  transactions %>% orders$create() %>% orders$classify() %>% orders$summarise() %>% orders$chain()
}

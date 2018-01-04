#' Chain orders by CUSIPs.
#'
#' If an instrument with a given CUSIP is found in diffrent orders,
#' then these orders must be related and hence has to be chained together.
#'
#' @export
chain_orders <- function(orders) {
  # Create a contigency matrix that shows relations between orders and CUSIPs.
  # Rows contain "order_id", columns contain "cusip":
  #          cusip
  # order_id 369604103 8BKQXT2 8BRRSQ6 8BRTKX9 8BRTNB5 ...
  # R00001       0       0       0       0       0     ...
  # R00002       0       0       0       0       0     ...
  # R00003       0       0       0       0       0     ...
  # R00004       0       0       0       0       0     ...
  # R00005       0       0       0       0       0     ...
  # ......      ...     ...     ...     ...     ...    ...
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
    # R00001      1
    # R00004      1
    #
    # [[2]]
    #          cusip
    # order_id 8BWGYG3 8BWGYH3 8BWGYJ1 9BWGYG5 9BWGYG6 9BWGYH8
    # R00002      1       0       1       1       0       1
    # R00040      0       1       1       0       1       1
    # R00057      1       0       0       1       0       0
    # R00088      0       1       0       0       1       0
    #
    # [[3]]
    #          cusip
    # order_id 8H54976 8H55643 9H55256 9H55257
    # R00003      1       1       1       1
    # R00043      0       0       1       1
    # R00066      0       1       0       0
    #
    # [[4]]
    #          cusip
    # order_id 9H82162
    # R00001      1
    # R00004      1
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
    # 1   R00001
    # 2   R00004
    #
    # [[2]]
    # # A tibble: 4 x 1
    #   order_id
    #      <chr>
    # 1   R00002
    # 2   R00040
    # 3   R00057
    # 4   R00088
    #
    # ...
    #
    # "order_id" comes from the row names of the matrixes and
    #
    purrr::map(~ dplyr::tibble(order_id = rownames(.x))) %>%
    # Put real orders in place of "order_id" by joining with "orders" data frame
    purrr::map(~ dplyr::inner_join(.x, orders, by = "order_id"))

  # Names elements in the chain list: CHAIN_*
  names(chains) <- stringr::str_c("CHAIN", stringr::str_pad(seq_along(chains), stringr::str_length(length(chains)), pad = "0"), sep = "_")

  return(chains)
}

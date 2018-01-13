confirmation <- new.env()

confirmation$process_page <- function(page) {
  page %>%
    # Split a page into lines
    stringr::str_split(pattern = NEWLINE, simplify = TRUE) %>%
    # Trim white spaces around lines
    stringr::str_trim() %>%
    # Replace multiple white spaces with a single space
    stringr::str_replace_all(pattern = one_or_more(SPC), replacement = " ") %>%
    # Drop empty lines
    stringr::str_subset(pattern = negated_char_class(START %R% zero_or_more(SPC) %R% END))
}

confirmation$process_document <- function(doc) {
  #dplyr::glimpse(doc %>% `[`(-1))
  doc %>%
    # Drop the first page
    `[`(-1) %>%
    # Process each page
    purrr::map(~ .x %>% confirmation$process_page()) %>%
    # Merge lines from all pages into one vector
    purrr::flatten_chr()
}

confirmation$read <- function(file) {
  # Read the document
  pdftools::pdf_text(file) %>% confirmation$process_document()
}

confirmation$validate <- function(transactions, totals) {
  # Calculate transactions total
  transactions_total <- transactions %>%
    # Remove empty elements from the list
    purrr::compact() %>%
    # Calculate totals for each transaction block
    purrr::map_dfr(~ .x %>%
                     dplyr::summarise_(
                       quantity   = sum(.$quantity   * dplyr::if_else(.$action == "BUY", 1L, -1L)),
                       net_amount = sum(.$net_amount * dplyr::if_else(.$action == "BUY", -1,   1))
                     )
    ) %>%
    # Calculate totals for all blocks
    dplyr::summarise(quantity = sum(quantity), net_amount = sum(net_amount)) %>%
    # Summing introduces a rounding error, so we have to round the result for the comparisson to work correctly.
    dplyr::mutate(net_amount = round(net_amount, 2))

  # Calculate confirmation totals
  confirmation_total <- totals %>%
    # Use summarise_at() to suppress "no visible binding for global variable" note
    dplyr::summarise(quantity = sum(shares), net_amount = sum(dollars)) %>%
    # Summing introduces a rounding error, so we have to round the result for the comparisson to work correctly.
    dplyr::mutate(net_amount = round(net_amount, 2))

  # Compare confirmation totals with the transactions' totals
  identical(transactions_total, confirmation_total)
}

confirmation$process <- function(file) {
  message(paste("Proccessing confirmation", file))

  # Extract lines from the document
  lines <- confirmation$read(file)

  transactions <- list(
    option       = option_block,
    stock        = stock_block,
    assigned     = assigned_block,
    exercised    = exercised_block,
    split_option = split_option_block
  ) %>% purrr::map(~ lines %>% .x$probe())

  # Extract confrimation totals from the lines
  totals <- total_block$probe(lines)

  if ( !confirmation$validate(transactions, totals) ) {
    print(lines)
    print(transactions)
    stop("Total of the transactions does not match total of the confirmation.")
  }

  # Return the transactions
  return(transactions)
}

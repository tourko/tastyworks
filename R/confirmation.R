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

confirmation$process <- function(file) {
  message(paste("Proccessing confirmation", file))

  # Extract lines from the document
  lines <- confirmation$read(file)

  # Extract transactions from the lines
  transactions <- transaction_blocks %>%
    purrr::map(~ parse_transaction_lines(lines, .x)) %>%
    # Remove empty elements from the list
    purrr::compact()

  # Calculate transactions total
  transactions_total <- transactions %>%
    # Calculate totals for each transaction block
    purrr::map_dfr(~ .x %>%
                     dplyr::summarise_(
                       quantity   = sum(.$quantity   * dplyr::if_else(.$action == "BUY", 1L, -1L)),
                       net_amount = sum(.$net_amount * dplyr::if_else(.$action == "BUY", -1,   1))
                     )
    ) %>%
    # Use *_at() functions to suppress "no visible binding for global variable" note.
    # Calculate totals for all blocks
    dplyr::summarise_at(c("quantity", "net_amount"), sum) %>%
    # Summing introduces a rounding error, so we have to round the result for the comparisson to work correctly.
    dplyr::mutate_at("net_amount", round, 2)

  # Extract confrimation totals from the lines
  total <- parse_total_lines(lines, total_block)

  # Calculate confirmation totals
  confirmation_total <- total %>%
    # Use summarise_at() to suppress "no visible binding for global variable" note
    dplyr::summarise_at(c("quantity", "net_amount"), sum) %>%
    # Summing introduces a rounding error, so we have to round the result for the comparisson to work correctly.
    dplyr::mutate_at("net_amount", round, 2)

  # Compare confirmation totals with the transactions' totals
  if ( !identical(transactions_total, confirmation_total) ) {
    print(lines)
    print(transactions)
    stop("Total of the transactions does not match total of the confirmation.")
  }

  # Return the transactions
  return(transactions)
}


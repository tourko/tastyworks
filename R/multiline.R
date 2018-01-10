multiline <- new.env()

multiline$detect <- function(lines, patterns) {
  # Apply N-lines pattern to the lines.
  # Returns a list of N logical vectors - one for each pattern.
  # Each vector has as may elements as there are lines.
  # Elements in the vectors indicate which lines matched the pattern corresponding to the pattern.
  #
  # Example for 4-lines pattern and 53 lines:
  # List of 4
  #   $ pattern_1: int [1:53] 0 0 0 0 0 0 ...
  #   $ pattern_2: int [1:53] 0 0 0 0 0 0 ...
  #   $ pattern_3: int [1:53] 0 0 0 0 0 0 ...
  #   $ pattern_4: int [1:53] 0 0 0 0 0 0 ...
  l <- purrr::map(patterns, ~ lines %>% stringr::str_detect(pattern = .x) %>% as.integer())

  # Convert the list into a matrix with N rows and as many columns as there are lines.
  # Example:
  #           [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] ... [,53]
  # pattern_1    0    0    0    0    0    0    0    0    1     0     0     0 ...     0
  # pattern_2    0    0    0    0    0    0    0    0    0     1     0     0 ...     0
  # pattern_3    0    0    0    0    0    0    0    0    0     0     1     0 ...     0
  # pattern_4    0    0    0    0    0    0    0    0    0     0     0     1 ...     0
  m <- matrix(purrr::flatten_int(l),
              byrow = TRUE,
              nrow = length(l),
              dimnames = list(names(patterns)))

  # Find indexes of the matrix with 1's
  idx <- which(m == 1, arr.ind = TRUE)

  # Replace 1's with the "col", which gives the line number
  #           [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] ... [,53]
  # pattern_1    0    0    0    0    0    0    0    0    9     0     0     0 ...     0
  # pattern_2    0    0    0    0    0    0    0    0    0    10     0     0 ...     0
  # pattern_3    0    0    0    0    0    0    0    0    0     0    11     0 ...     0
  # pattern_4    0    0    0    0    0    0    0    0    0     0     0    12 ...     0
  for (i in seq_len(nrow(idx))) {
    row <- idx[i, "row"]
    col <- idx[i, "col"]
    m[row, col] <- col
  }

  # A multiline pattern is detected, if there are M consequetive lines matching the patterns,
  # where M is the number of patterns.
  # Such lines will form a "diagonal" with 1's in the matrix.
  # In the above example columns 10, 11, 12 and 13 form such "diagonal".
  # In order to find the lines matching all M patterns, we shift the rows,
  # so that 1's form a column.
  # Example:
  #            [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] ... [,53]
  # pattern_1     0    0    0    0    0    0    0    0    9     0     0     0    13 ...     0
  # pattern_2     0    0    0    0    0    0    0    0   10     0     0     0    14 ...     0
  # pattern_3     0    0    0    0    0    0    0    0   11     0     0     0    15 ...     0
  # pattern_4     0    0    0    0    0    0    0    0   12     0     0     0    16 ...     0
  for (i in seq_len(nrow(m))) {
    # Skip the first row
    if (i == 1) next
    # Shift rows by n = i - 1 to the left, where "i" is the row number
    m[i, ] <- data.table::shift(m[i, ], n = i - 1, fill = 0, type = "lead")
  }

  # Find columns that have at least one 0.
  no_match <- which(colSums(m == 0) > 0)
  # If there are any such columns, ...
  if ( length(no_match) > 0 ) {
    # ... drop them from the matrix
    m <- m[, -no_match, drop = FALSE]
  }

  # End result:
  #           [,1] [,2]
  # pattern_1    9   13
  # pattern_2   10   14
  # pattern_3   11   15
  # pattern_4   12   16
  return(m)
}

multiline$subset <- function(lines, patterns) {
  # Get line numbers matching the patterns
  n <- multiline$detect(lines, patterns)

  # Get the actual lines
  l <- matrix(lines[n], nrow = nrow(n), ncol = ncol(n), dimnames = dimnames(n))

  list(lines = l, numbers = n)
}

multiline$match <- function(lines, patterns, names = NULL) {
  # Get the lines matching the patterns
  s <- multiline$subset(lines, patterns)

  # Extract tokens from the lines.
  tokens <-
    # Apply patterns to the detected lines and merge the resulting list
    # into a data frame by binding the columns, so that each row has
    # all the tokens from teh multiple patterns
    purrr::imap_dfc(patterns,
                      # Limit the matching to the lines that have already been detected by the pattern
                    ~ s$lines[.y, ] %>%
                      # Extracts tokens from each line matching a pattern
                      stringr::str_match(pattern = .x) %>%
                      # The first column contains the entire matched line and we don't need it
                      .[, -1, drop = FALSE] %>%
                      # Convert matrix to a tibble
                      dplyr::as_tibble())

  if ( !purrr::is_empty(tokens) ) {
    if ( is.null(names) ) {
      # Rename the columns as v1, v2, ...
      colnames(tokens) <- stringr::str_c("v", tokens %>% ncol() %>% seq_len())
    } else {
      # Set columns names, if they were provided
      colnames(tokens) <- names
    }

    # Remove row names from the line numbers to avoid attributes being added to the tibble
    rownames(s$numbers) <- NULL
    # Add "first_line" and "last_line" to indicate the line range
    tokens <- tokens %>% dplyr::mutate(
      first_line = s$numbers[1, ],
      last_line  = s$numbers[nrow(s$numbers), ]
    )
  }

  return(tokens)
}

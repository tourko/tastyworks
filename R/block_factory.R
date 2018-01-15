block_factory <- R6::R6Class("Block",
  private = list(
    patterns = c(),
    token_names = c(),
    augment = function(tokens) { tokens }
  ),

  public = list(
    probe = function(lines) {
      tokens <- lines %>%
        multiline$match(private$patterns, private$token_names) %>%
        purrr::when(purrr::is_empty(.) ~ ., ~ private$augment(.))
    }
  )
)

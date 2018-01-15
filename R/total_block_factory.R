total_block_factory <- R6::R6Class("TotalBlock", inherit = block_factory,
  private = list(
    patterns = c(
      line1 = START %R% "TOTAL SHARES" %R%
        SPC %R% or("BOUGHT","SOLD") %R% ":" %R%
        SPC %R% capture(pattern$accounting_number) %R% # (1) Number of shares bought/sold
        SPC %R% "TOTAL DOLLARS" %R%
        SPC %R% or("BOUGHT","SOLD") %R% ":" %R%        # (2) Dollar amount
        SPC %R% capture(pattern$accounting_number)
    ),

    token_names = c(
      "shares",  # (1) Number of shares bought/sold
      "dollars"  # (2) Dollar amount
    ),

    augment = function(tokens) {
      tokens %>%
        # Drop .lines column
        dplyr::select(-.lines) %>%
        # Convert number of shares to integer and dollars to nummeric
        dplyr::mutate(shares  = as.integer(shares),
                      dollars = as.numeric(stringr::str_replace_all(dollars, ",", "")))
    }
  ),

  public = list()
)

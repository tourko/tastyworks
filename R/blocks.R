#
# Define commonly used patterns
#
pattern <- list(
  # Number in accounting format: "3.00" "123.00" "1,234.00" "1,234,567.00" "-123.00"
  accounting_number =  optional("-") %R% zero_or_more( group( digit(1,3) %R% "," ) ) %R% digit(1,3) %R% DOT %R% digit(2),

  # Price with 7 digits after the dot: "0.1300000" "10.3500000"
  # They are all less than 1000, so we can use simplified pattern.
  price_number = digit(1,3) %R% DOT %R% digit(7),

  # Commission and fee amount: "0.00" "1.00" "0.04" "0.10"
  # They are all less than 10, so we can use simplified pattern.
  commission_number = DGT %R% DOT %R% digit(2),

  # Tag number - either a capital letter or a digit followed by 4 digits: "C0998" "B6388" "15056"
  tag_number = or(UPPER,DGT) %R% digit(4),

  # Option strike number: "28" "350" "62.50" "1020"
  strike_number = digit(1,4) %R% optional( group( DOT %R% digit(2) ) ),

  # Option CUSIP format - seven-character alphanumeric: "9H82162"
  option_cusip_string = repeated(ALNUM, 7),

  # Stock CUSIP format - nine-digit string: "369604103"
  stock_cusip_string = repeated(DIGIT, 9)
)

#
# Generic block
#
block <- new.env()

block$parse <- function(lines, block) {
  lines %>%
    multiline$match(block$patterns, block$token_names) %>%
    block$augment()
}

#
# Total block of lines
#
total_block <- new.env(parent = block)

total_block$patterns <-  c(
  line1 = START %R% "TOTAL SHARES" %R%
          SPC %R% or("BOUGHT","SOLD") %R% ":" %R%
          SPC %R% capture(pattern$accounting_number) %R% # (1) Number of shares bought/sold
          SPC %R% "TOTAL DOLLARS" %R%
          SPC %R% or("BOUGHT","SOLD") %R% ":" %R%        # (2) Dollar amount
          SPC %R% capture(pattern$accounting_number)
)

total_block$token_names <-  c(
  "shares",  # (1) Number of shares bought/sold
  "dollars"  # (2) Dollar amount
)

total_block$augment <- function(tokens) {
  tokens %>%
    # Convert number of shares to integer and dollars to nummeric
    dplyr::mutate(shares  = as.integer(shares),
                  dollars = as.numeric(stringr::str_replace_all(dollars, ",", "")))
}

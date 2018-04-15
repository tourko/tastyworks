#
# Define as.<function>() that convert values to factors
#
as.reason <- function(value) {
  factor(value, levels = c("UNSOLICITED", "ASSIGNED", "EXERCISED", "EXPIRED", "SPLIT"))
}

as.instrument <- function(value) {
  factor(value, levels = c("STOCK", "OPTION"))
}

as.action <- function(value) {
  value[value == "B"] <- "BUY"
  value[value == "S"] <- "SELL"
  factor(value, levels = c("BUY", "SELL", "REMOVE"))
}

as.position <- function(value) {
  value[value == "CLOSING"] <- "CLOSE"
  factor(value, levels = c("OPEN", "CLOSE"))
}

as.option_type <- function(value) {
  factor(value, levels = c("CALL", "PUT"))
}

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

  # Stock CUSIP format - nine-digit alphanumeric: "369604103", "H8817H100"
  stock_cusip_string = repeated(ALNUM, 9)
)

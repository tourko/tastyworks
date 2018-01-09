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

total_block$patterns <- c(
  line1 = START %R% "TOTAL SHARES" %R%
          SPC %R% or("BOUGHT","SOLD") %R% ":" %R%
          SPC %R% capture(pattern$accounting_number) %R% # (1) Number of shares bought/sold
          SPC %R% "TOTAL DOLLARS" %R%
          SPC %R% or("BOUGHT","SOLD") %R% ":" %R%        # (2) Dollar amount
          SPC %R% capture(pattern$accounting_number)
)

total_block$token_names <- c(
  "shares",  # (1) Number of shares bought/sold
  "dollars"  # (2) Dollar amount
)

total_block$augment <- function(tokens) {
  tokens %>%
    # Convert number of shares to integer and dollars to nummeric
    dplyr::mutate(shares  = as.integer(shares),
                  dollars = as.numeric(stringr::str_replace_all(dollars, ",", "")))
}


#
# Option block of lines
#
option_block <- new.env(parent = block)

option_block$patterns <- c(
  # Examples of line 1 for options:
  # "2  S  08/30/17  08/31/17   1         0.7500000      75.00  1.00  0.06  0.10  E8470      73.84  TUA0831  1 1"
  # "2  B  11/21/17  11/22/17   1         1.0600000     106.00  0.00  0.04  0.10  B4169     106.14  TUA1122  1 1"
  line1 = START %R% DGT %R%                                #   -  Account Type
          SPC %R% or("B", "S", capture = TRUE) %R%         #  (1) BUY or SELL
          SPC %R% capture(MDY) %R%                         #  (2) Trade Date
          SPC %R% MDY %R%                                  #   -  Settle Date
          SPC %R% capture(one_or_more(DGT)) %R%            #  (3) Quantity
          SPC %R% capture(pattern$price_number) %R%        #  (4) Price
          SPC %R% capture(pattern$accounting_number) %R%   #  (5) Principal
          SPC %R% capture(pattern$commission_number) %R%   #  (6) Commission
          SPC %R% capture(pattern$commission_number) %R%   #  (7) Transaction fee
          SPC %R% capture(pattern$commission_number) %R%   #  (8) Additional fee
          SPC %R% capture(pattern$tag_number) %R%          #  (9) Tag number
          SPC %R% capture(pattern$accounting_number),      # (10) Net ammount

  # Examples of line 2 for options:
  # "Desc:  PUT  XOP  10/20/17  28  SPDR S&P OIL&GAS EXPL & PRDN  Interest/STTax:  0.00  CUSIP:  9H82162"
  # "Desc:  CALL AAPL 12/15/17 190  APPLE INC                     Interest/STTax:  0.00  CUSIP:  8KLVVP9"
  line2 = START %R% "Desc:" %R%
          SPC %R% capture(or("CALL", "PUT")) %R%           # (11) Option type (CALL or PUT)
          SPC %R% capture(one_or_more(UPPER)) %R%          # (12) Symbol of the underlying stock
          SPC %R% capture(MDY) %R%                         # (13) Expiration date
          SPC %R% capture(pattern$strike_number) %R%       # (14) Strike
          SPC %R% one_or_more(PRINT) %R%                   #   -  UL description
          "Interest/STTax:" %R% SPC %R% "0.00" %R%         #   -  Interest/Tax
          SPC %R% "CUSIP:" %R%
          SPC %R% capture(pattern$option_cusip_string),    # (15) Option CUSIP

  # Line 3 is the same for all transaction types.
  # "Currency: USD    ReportedPX:     MarkUp/Down:"
  line3 = START %R% "Currency: USD" %R%
          SPC %R% "ReportedPX:" %R%
          SPC %R% "MarkUp/Down:",

  # Examples of line 4 for options:
  # "Trailer:    UNSOLICITED, OPEN CONTRACT"
  # "Trailer:    UNSOLICITED, CLOSING CONTRACT"
  line4 = START %R% "Trailer:" %R%
          SPC %R% capture("UNSOLICITED") %R% "," %R%      # (16) UNSOLICITED
          SPC %R% capture(or("OPEN", "CLOSING")) %R%      # (17) OPEN or CLOSE
          SPC %R% "CONTRACT"
)

option_block$token_names <- c(
  "action",           #  (1) BUY or SELL
  "trade_date",       #  (2) Trade Date
  "quantity",         #  (3) Quantity
  "price",            #  (4) Price
  "principal",        #  (5) Principal
  "commission",       #  (6) Commission
  "transaction_fee",  #  (7) Transaction fee
  "additional_fee",   #  (8) Additional fee
  "tag_number",       #  (9) Tag number
  "net_amount",       # (10) Net ammount
  "option_type",      # (11) Option type (CALL or PUT)
  "symbol",           # (12) Symbol of the underlying stock
  "expiration_date",  # (13) Expiration date
  "strike",           # (14) Strike
  "cusip",            # (15) Option CUSIP
  "reason",           # (16) UNSOLICITED
  "position"          # (17) OPEN or CLOSE
)

option_block$augment <- function(tokens) {
  tokens %>%
    # Convert string values in the columns to the appropriate data types
    dplyr::mutate(
      # Convert to factor: "B" -> "BUY", "S" -> "SELL"
      action          = as.action(action),
      trade_date      = lubridate::mdy(trade_date),
      quantity        = as.integer(quantity),
      price           = as.numeric(price),
      # Drop "," as thuosands separator and convert to numeric
      principal       = as.numeric(stringr::str_replace_all(principal, ",", "")),
      commission      = as.numeric(commission),
      transaction_fee = as.numeric(transaction_fee),
      additional_fee  = as.numeric(additional_fee),
      # Drop "," as thuosands separator and convert to numeric
      net_amount      = as.numeric(stringr::str_replace_all(net_amount, ",", "")),
      option_type     = as.option_type(option_type),
      expiration_date = lubridate::mdy(expiration_date),
      strike          = as.numeric(strike),
      # Convert to factor
      reason          = as.reason(reason),
      # Convert to factor: "OPEN" -> "OPEN", "CLOSING" -> "CLOSE"
      position        = as.position(position),
      # Add "instrument" column and set its value to "OPTION"
      instrument      = as.instrument("OPTION")) %>%
    # Change columns' order
    dplyr::select(
      trade_date,
      reason,
      action,
      position,
      symbol,
      instrument,
      quantity,
      option_type,
      strike,
      expiration_date,
      price:additional_fee,
      net_amount,
      cusip,
      tag_number)
}

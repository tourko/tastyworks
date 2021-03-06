split_option_block <- R6::R6Class("SplitOptionBlock", inherit = option_block,
  private = list(
    patterns = c(
      # Line 1 for split option is the same as the line for option
      # Examples of line 1:
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

      # Examples of line 2 for split options:
      # "Desc: CALL UNG1 01/19/18 6 UNITED STATES NATRAL GAS FD LP ADJ 1:4 REV SPLIT DEL:25 UNG Interest/STTax: 0.00 CUSIP: 9LVZSV2"
      line2 = START %R% "Desc:" %R%
        SPC %R% capture(or("CALL", "PUT")) %R%           # (11) Option type (CALL or PUT)
        SPC %R% capture(one_or_more(UPPER) %R% DGT) %R%  # (12) Symbol of the underlying stock
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

      # Line 4 for split option is the same as the line for option
      # Examples of line 4:
      # "Trailer:    UNSOLICITED, OPEN CONTRACT"
      # "Trailer:    UNSOLICITED, CLOSING CONTRACT"
      line4 = START %R% "Trailer:" %R%
        SPC %R% capture("UNSOLICITED") %R% "," %R%      # (16) UNSOLICITED
        SPC %R% capture(or("OPEN", "CLOSING")) %R%      # (17) OPEN or CLOSE
        SPC %R% "CONTRACT"
    )
  )
)

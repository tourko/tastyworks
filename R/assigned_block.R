assigned_block <- R6::R6Class("AssignedBlock", inherit = stock_block,
   private = list(
     patterns = c(
       # Line 1 for assigned stock is the same as the line for stocks
       # Exampples of line 1:
       # "2  B   11/21/17  11/24/17  100 GE  24.0000000  2,400.00  5.00  0.00  0.00  A8103   2,405.00  TUA1124 3 1"
       # "2  S   12/08/17  12/12/17  100 UAL 64.3801000  6,438.01  0.00  0.16  0.08  T2198   6,437.77  TUA1212 6 1"
       line1 = START %R% DGT %R%                                #   -  Account Type
         SPC %R% or("B", "S", capture = TRUE) %R%         #  (1) BUY or SELL
         SPC %R% capture(MDY) %R%                         #  (2) Trade Date
         SPC %R% MDY %R%                                  #   -  Settle Date
         SPC %R% capture(one_or_more(DGT)) %R%            #  (3) Quantity
         SPC %R% capture(one_or_more(UPPER)) %R%          #  (4) Symbol of the stock
         SPC %R% capture(pattern$price_number) %R%        #  (5) Price
         SPC %R% capture(pattern$accounting_number) %R%   #  (6) Principal
         SPC %R% capture(pattern$commission_number) %R%   #  (7) Commission
         SPC %R% capture(pattern$commission_number) %R%   #  (8) Transaction fee
         SPC %R% capture(pattern$commission_number) %R%   #  (9) Additional fee
         SPC %R% capture(pattern$tag_number) %R%          # (10) Tag number
         SPC %R% capture(pattern$accounting_number),      # (11) Net ammount

       # Line 2 for assigned stock is the same as the line for stocks
       # Examples of line 2:
       # "Desc:   GENERAL ELECTRIC COMPANY COM                         Interest/STTax:  0.00  CUSIP:  369604103"
       line2 = START %R% "Desc:" %R%
         SPC %R% one_or_more(PRINT) %R%                   #   -  UL description
         "Interest/STTax:" %R% SPC %R% "0.00" %R%         #   -  Interest/Tax
         SPC %R% "CUSIP:" %R%
         SPC %R% capture(pattern$stock_cusip_string),     # (12) Stock CUSIP

       # Line 3 is the same for all transaction types.
       # "Currency: USD    ReportedPX:     MarkUp/Down:"
       line3 = START %R% "Currency: USD" %R%
         SPC %R% "ReportedPX:" %R%
         SPC %R% "MarkUp/Down:",

       # Example of line 4 for assigned stock:
       # Trailer:    A/E 9GMSJJ4 1 ASSIGNED
       line4 = START %R% "Trailer:" %R%
         SPC %R% "A/E" %R%
         SPC %R% capture(pattern$option_cusip_string) %R% # (13) CUSIP of the assigned option
         SPC %R% capture(one_or_more(DGT)) %R%            # (14) Assigned quantity
         SPC %R% capture("ASSIGNED")                      # (15) ASSIGNED
     ),

     token_names = c(
       "action",           #  (1) BUY or SELL
       "trade_date",       #  (2) Trade Date
       "quantity",         #  (3) Quantity
       "symbol",           #  (4) Symbol of the stock
       "price",            #  (5) Price
       "principal",        #  (6) Principal
       "commission",       #  (7) Commission
       "transaction_fee",  #  (8) Transaction fee
       "additional_fee",   #  (9) Additional fee
       "tag_number",       # (10) Tag number
       "net_amount",       # (11) Net ammount
       "cusip",            # (12) Stock CUSIP
       "assigned_cusip",   # (13) CUSIP of the assigned option
       "assigned_qty",     # (14) Assigned quantity
       "reason"            # (15) ASSIGNED
     ),

     augment = function(tokens) {
       tokens %>%
         # Apply the same augment function as to the stock
         super$augment() %>%
         # Convert "assigned_qty" to integer
         dplyr::mutate(assigned_qty = as.integer(assigned_qty))
     }
   )
)

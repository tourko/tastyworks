#' Read transactions from the Tastywork's confirmations
#'
#' @description This function reads Tastywork's confirmation files, which are in PDF format,
#' extracts transactions from each file and merges them together into a single data frame.
#'
#' @param files a vector of path names to the files.
#' @param add.expired logical that controls whether bogus transactions for expired options should be added.
#' @param check.integrity logical that controls whether to check transactions integrity.
#'
#' @details Tastyworks generates a confirmation only if there was any trading activity druring the day.
#' That makes it difficult to detect any missing confirmations. And there are known cases when
#' confirmations were missing.
#'
#' Specifying check.integrity = TRUE helps to identify if there are \emph{potentially} any missing transactions.
#' It checks whether the quantity of traded stocks/options in the \emph{closing} transaction is greater than
#' the quantity in the corresponding \emph{opening} transaction. This is the symptom of the missing confirmations.
#'
#' @return The output is a data frame, where each record represents one transaction. The data frame has
#' the following variables:
#' \describe{
#'   \item{\strong{trade_date} : \emph{Date, format "YYYY-MM-DD"}}{
#'   Date, when the transaction was executed
#'   }
#'
#'   \item{\strong{reason} : \emph{Factor w/ 5 levels "UNSOLICITED", "ASSIGNED", "EXERCISED", "EXPIRED", "SPLIT"}}{
#'   Reason for the transaction:
#'   \itemize{
#'     \item UNSOLICITED - the transaction was initiated by a customer
#'     \item ASSIGNED - stock assignment due to exercised short option
#'     \item EXERCISED - stock assignment due to long option expiring in the money
#'     \item EXPIRED - option expired out of the money
#'     \item SPLIT - stock split
#'   }}
#'
#'   \item{\strong{action} : \emph{Factor w/ 3 levels "BUY", "SELL", "REMOVE"}}{
#'   Action that created the transaction:
#'   \itemize{
#'     \item BUY - purchase of stock or option
#'     \item SELL - sell of stock or option
#'     \item REMOVE - option was removed because it has expired
#'   }}
#'
#'   \item{\strong{position} : \emph{Factor w/ 2 levels "OPEN", "CLOSE"}}{
#'   Position indicates whether it is an OPEN or a CLOSE transaction
#'   }
#'
#'   \item{\strong{symbol} : \emph{chr}}{
#'   Ticker symbol of the instrument
#'   }
#'
#'   \item{\strong{instrument} : \emph{Factor w/ 2 levels "STOCK", "OPTION"}}{
#'   Instrument type - either STOCK or OPTION
#'   }
#'
#'   \item{\strong{quantity} : \emph{int}}{
#'   A positive integer indicating the quantity of the traded stock/option
#'   }
#'
#'   \item{\strong{option_type} : \emph{Factor w/ 2 levels "CALL", "PUT"}}{
#'   Option type - either CALL or PUT or NA for the stock
#'   }
#'
#'   \item{\strong{strike} : \emph{num}}{
#'   Optin strike or NA for the stock
#'   }
#'
#'   \item{\strong{expiration_date} : \emph{Date, format "YYYY-MM-DD"}}{
#'   Expiration date of the option or NA for the stock
#'   }
#'
#'   \item{\strong{price} : \emph{num}}{
#'   Price in USD of one unit of the trading instrument
#'   }
#'
#'   \item{\strong{principal} : \emph{num}}{
#'   Principal is the quntity multiplied by the price, i.e. transaction amount in USD excluding commission and fees
#'   }
#'
#'   \item{\strong{commission} : \emph{num}}{
#'   Commission in USD paid to Tastyworks
#'   }
#'
#'   \item{\strong{transaction_fee} : \emph{num}}{
#'   Transaction fees in USD
#'   }
#'
#'   \item{\strong{additional_fee} : \emph{num}}{
#'   Additional fees in USD
#'   }
#'
#'   \item{\strong{net_amount} : \emph{num}}{
#'   Transaction amount in USD including commission and fees
#'   }
#'
#'   \item{\strong{cusip} : \emph{chr}}{
#'   CUSIP numbers that identify the transaction's financial security
#'   }
#'
#'   \item{\strong{tag_number} : \emph{chr}}{
#'   Tag number uniquely identifies the transaction
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' ## Read a single confimation file:
#' transactions <- read_confirmations("2017-08-30-1NE23456-confirmation.pdf")
#' }
#'
#' @examples
#' \dontrun{
#' ## Read several confirmation files:
#' files <- c("2017-08-30-1NE23456-confirmation.pdf",
#'            "2017-08-30-1NE23456-confirmation.pdf")
#' transactions <- read_confirmations(files)
#' }
#'
#' @examples
#' \dontrun{
#' ## Read all confimation files in a "confirmations":
#' # A template for confirmation file name (YYYY-MM-DD-xxxxxxxx-confirmation.pdf)
#' confirmation_pattern <-
#'  START %R% YMD %R% "-" %R%
#'  repeated(ALNUM, 8) %R% "-confirmation" %R%
#'  DOT %R% "pdf" %R% END
#'
#' # Get a list of confirmation files
#' files <- list.files(path = path.expand("confirmations"),
#'                     pattern = confirmation_pattern,
#'                     full.names = TRUE)
#'
#' # Read the files
#' transactions <- read_confirmations(files)
#' }
#'
#' @export
read_confirmations <- function(files, add.expired = FALSE, check.integrity = FALSE) {
  transactions$read(files) %>%
    # Process assigned stocks
    transactions$process_assigned() %>%
    # Process options for the stocks that were split
    transactions$process_split_options() %>%
    # Process expired options, if add.expired == TRUE
    purrr::when(add.expired ~ transactions$process_expired(.), ~ .) %>%
    # Merge transaction blocks in one data frame
    transactions$merge() %>%
    # Check transactions integrity, if check.integrity == TRUE
    purrr::when(check.integrity ~ transactions$check_integrity(.), ~ .)
}

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

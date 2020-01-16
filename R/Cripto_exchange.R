#' Criptocurrency Exchange Rate
#'
#' @param cripto Specify the symbol of the cryptocurrency
#' @param exchange Specifiy the symbol of the market
#'
#' @author
#' Erick Cuevas Fernández
#'
#'
#' @importFrom
#' jsonlite fromJSON
#'
#' @return
#' This function return  the realtime exchange rate for any pair of digital currency.
#'
#'
#' @examples
#'
#' Cripto_exchange("BTC", "MXN")
#'
#' @rdname Cripto_exchange
#' @export Cripto_exchange
Cripto_exchange <- function(cripto = "LTC", exchange = "MXN"){

   message(paste("This function give you the price market of", cripto, "in the",
                  exchange, "currency"))

   apikey <- "ZEYMZB1JKHF0UY5Q"


   URL <- paste0("https://www.alphavantage.co/query?function=CURRENCY_EXCHANGE_RATE&from_currency=",
              cripto,
              "&to_currency=",
              exchange,
              "&apikey=", apikey)

   price <- jsonlite::fromJSON(URL)


   price_1 <- data.frame(price[[1]])
   col_names <- c("From_Currency_Code", "From_Currency_Name", "To_Currency_Code",
                  "To_Currency_Name", "Exchange_Rate", "Last_Refreshed",
                 "Time_Zone", "Bid_Price", "Ask_Price")
   colnames(price_1) <- col_names

   return(price_1)

}

#' Criptocurrency Time Series
#'
#' @param cripto Specify the symbol of the cryptocurrency
#' @param market Specifiy the symbol of the market
#' @param temp DAILY, WEEKLY or MOTHLY
#'
#' @author
#' Erick Cuevas Fernández
#'
#' @return
#' Return a data.frame the daily historical time series for a digital currency (e.g., BTC) traded on a specific market (e.g., CNY/Chinese Yuan), refreshed daily at midnight (UTC). Prices and volumes are quoted in both the market-specific currency and USD.
#'
#' @importFrom
#' stringr str_to_lower
#'
#' @importFrom
#' jsonlite fromJSON
#'
#'
#' @examples
#' Cripto_time_series(cripto = "ETH", temp = "MONTHLY")
#'
#'
#' @rdname Cripto_time_series
#' @export
Cripto_time_series <- function(cripto = "LTC", market = "MXN",
                               temp = "WEEKLY") {

  message(paste("This function give you the price market of", cripto, "in",
                market, stringr::str_to_lower(temp)))

  apikey <- "ZEYMZB1JKHF0UY5Q"


  URL <- paste0("https://www.alphavantage.co/query?function=DIGITAL_CURRENCY_",
    temp,
    "&symbol=",
    cripto,
    "&market=",
    market,
    "&apikey=", apikey)

  price <- jsonlite::fromJSON(URL)

  message(paste(price[[1]][[3]], "in", price[[1]][[5]], "market",
                stringr::str_to_lower(temp)))

  fixer <- price[[2]]
  df <- data.frame()
  for (i in 1:length(fixer)){

    a <- data.frame(names(fixer[i]),fixer[[i]])
    df <- rbind(df, a)

  }

  names_col <- c("Date",paste0("Open_", market), "Open_USD",
                 paste0("High_", market), "High_USD",
                 paste0("Low_", market), "Low_USD",
                 paste0("Close_", market), "Close_USD",
                 "Volume", "Market_cap_USD")

  colnames(df) <- names_col

  df$Date <- lubridate::as_date(df$Date)

  for (i in 2:11) {

    df[,i] <- as.character(df[,i])
    df[,i] <- as.numeric(df[,i])

  }


  return(df)

}



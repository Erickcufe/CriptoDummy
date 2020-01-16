#' Criptocurrency Arima (Modelo Autorregresivo Integrado de Media Movil)
#'
#' @param df A data.frame with time series
#' @param temp Interval of time series (DAILY, WEEKLY, MONTHLY)
#' @param n_predict The intervals to predict value
#'
#' @author
#' Erick Cuevas Fernández
#'
#' @return
#' One plot of Time Series prediction using ARIMA
#'
#' @importFrom
#' forecast forecast auto.arima
#'
#'
#' @import
#' lubridate
#'
#' @examples
#'
#' ETH <- Cripto_time_series(cripto = "ETH", temp = "MONTHLY")
#' Cripto_Arima(ETH, "MONTHLY", 6)
#'
#' @rdname Cripto_Arima
#' @export
Cripto_Arima <- function(df, temp, n_predict){

  df <- df[order(df[,1], decreasing = FALSE),]

  start_date <- df$Date[1]
  start_date_1 <- c(lubridate::year(start_date),
                    lubridate::month(start_date),
                    lubridate::day(start_date))

  if(temp=="DAILY"){

    interval <- 365
    start_date_1 <- c(lubridate::year(start_date),
                      lubridate::month(start_date),
                      lubridate::day(start_date))

  }

  if(temp=="WEEKLY"){

    interval <- 52
    start_date_1 <- c(lubridate::year(start_date),
                      lubridate::month(start_date),
                      lubridate::day(start_date))

  }

  if(temp=="MONTHLY"){

    interval <- 12
    start_date_1 <- c(lubridate::year(start_date),
                      lubridate::month(start_date))

  }


  df.ts <- ts(df[,8], start = start_date_1,
              frequency = interval)

  df.arima <- forecast::auto.arima(df.ts, parallel = TRUE)

  df.fore <- forecast::forecast(df.arima, h = n_predict)

  plot(df.fore, col="red", fcol = "green")

  return(df.fore)

}

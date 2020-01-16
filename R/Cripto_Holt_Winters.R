#' Criptocurrency Holt Winters
#'
#' @param df A data.frame with time series
#' @param temp Interval of time series (DAILY, WEEKLY, MONTHLY)
#' @param n_predict The intervals to predict value
#'
#' @author
#' Erick Cuevas Fernández
#'
#' @return
#' Two plots of Time Series prediction using HoltWinters
#'
#' @importFrom
#' forecast forecast
#'
#' @import
#' lubridate
#'
#' @examples
#'
#' ETH <- Cripto_time_series(cripto = "ETH", temp = "MONTHLY")
#' Cripto_Holt_Winters(ETH, "MONTHLY", 6)
#'
#' @rdname Cripto_Holt_Winters
#' @export
Cripto_Holt_Winters <- function(df, temp, n_predict){


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

  df.hw <- HoltWinters(df.ts)

  plot(df.hw, col="blue", col.predicted = "red")

  df.fore <- forecast::forecast(df.hw, h = n_predict)

  plot(df.fore)

 return(df.fore)

}

# r <- Cripto_Holt_Winters(df, "WEEKLY")

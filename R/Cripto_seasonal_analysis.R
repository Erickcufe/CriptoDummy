#' Criptocurrency Seasonal Analysis
#'
#' @param df A data.frame with time series
#' @param temp Interval of time series (DAILY, WEEKLY, MONTHLY)
#'
#' @author
#' Erick Cuevas Fernández
#'
#' @return
#' Two plots of Time Series decomposition and a List with the results of decompisitions
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
#' Cripto_seasonal_analysis(ETH, "MONTHLY")
#'
#' @rdname Cripto_seasonal_analysis
#' @export
Cripto_seasonal_analysis <- function(df, temp){

  df <- df[order(df[,1], decreasing = FALSE),]

  start_date <- df$Date[1]
  start_date_1 <- c(lubridate::year(start_date),
                    lubridate::month(start_date),
                    lubridate::day(start_date))

  if(temp=="DAILY"){

    interval <- 365

  }

  if(temp=="WEEKLY"){

    interval <- 52

  }

  if(temp=="MONTHLY"){

    interval <- 12

  }


  df.ts <- ts(df[,8], start = start_date_1,
              frequency = interval)


  df.ts.log <- log(df.ts)

  df.stl <- stl(log(df.ts), s.window = "period")
  plot(df.stl, main = "Decomposition by Loess")

  df.dec <- decompose(df.ts.log)
  plot(df.dec)

  df.season.adjusted <- df.ts.log - df.dec$seasonal
  results <- list("Seasonal Decomposition of Time Series by Loess" = df.stl,
                  "Classical Seasonal Decomposition by Moving Averages" = df.dec,
                  "Seasonal Adjusted" = df.season.adjusted)

  return(results)

}

# jj <- Cripto_seasonal_analysis(df, "WEEKLY")


Cripto_seasonal_analysis <- function(df, temp){

  start_date <- df$Date[nrow(df)]
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


  df.ts <- ts(df$Close_MXN, start = start_date_1,
              frequency = interval)

  plot(df.ts)

  df.ts.log <- log(df.ts)
  plot(df.ts.log)

  df.stl <- stl(log(df.ts), s.window = "period")
  plot(df.stl)

  df.dec <- decompose(df.ts.log)
  plot(df.dec)

}

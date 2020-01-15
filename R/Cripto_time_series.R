
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

    a <- data.frame(fixer[[i]])
    rownames(a) <- names(fixer[i])
    df <- rbind(df, a)

  }

  names_col <- c(paste0("Open_", market), "Open_USD",
                 paste0("High_", market), "High_USD",
                 paste0("Low_", market), "Low_USD",
                 paste0("Close_", market), "Close_USD",
                 "Volume", "Market_cap_USD")

  colnames(df) <- names_col

  return(df)

}




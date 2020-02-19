#' Crypto Plot of Predictions
#'
#' @param df A list with the predictions
#' @param temp Units of time predicted
#' @param n_predicted Numer of units predicted
#' @param crypto A Crypto Symbol
#'
#' @author
#' Erick Cuevas Fernandez
#'
#' @return
#' A list with a data.frame with the predictions, and two plots of the predictions
#'
#' @import
#' ggplot2
#'
#' @import
#' lubridate
#'
#' @importFrom
#' stringr str_to_lower
#'
#' @examples
#'
#' LTC_monthly <- Cripto_time_series(cripto = "LTC", market = "MXN", temp = "MONTHLY")
#' LTC_hw3months <- Cripto_Holt_Winters(df = LTC_monthly, temp = "MONTHLY", n_predict = 3)
#' plots_LTC <- Cripto_plot_predict(df = LTC_hw3months, temp = "MONTHLY", n_predicted = 3, crypto = "LTC")
#'
#' @rdname Cripto_plot_predict
#' @export
Cripto_plot_predict <- function(df, temp="DAILY", n_predicted=10,
                                crypto){

  message(paste("Plot the predictions of", n_predicted, stringr::str_to_lower(temp)))

  mean_1 <- unclass(df$mean)
  low_1 <- unclass(df$lower)
  high_1 <- unclass(df$upper)
  actual <- CriptoDummy::Cripto_time_series(cripto = crypto, temp = temp)
  actual_1 <- as.numeric(as.character(actual$Open_MXN[1]))
  min_actual <- actual$Low_MXN[1]
  max_actual <- actual$High_MXN[1]
  close_actual <- actual$Close_MXN[1]

  if (temp=="DAILY"){

    today <- lubridate::today()

    days <- list()
    for (i in 1:n_predicted) {

      a <- today + i
      days[i] <- as.character(a)

    }

    days_good <- unlist(days)
    df_2 <- data.frame(Days = days_good)

  }

  if (temp == "WEEKLY"){

    today <- lubridate::today()

    week_1 <- list()
    for (i in 1:n_predicted) {

      a <- today + (i*7)
      week_1[i] <- as.character(a)

    }

    days_good <- unlist(week_1)
    df_2 <- data.frame(Weeks = days_good)

  }

  if (temp == "MONTHLY"){

    today <- lubridate::today()
    days_good <- seq(today, by = "month", length = n_predicted)
    days_good <- as.character(days_good)
    df_2 <- data.frame(Months = days_good)

  }


  table_pred <- data.frame()
  for (i in 1:n_predicted) {

    lower <- t(low_1[i,])
    upper <- t(high_1[i,])
    together <- c(lower, mean_1[i], upper)
    temp_df <- data.frame(Price_predicted = together, Dates = df_2[i,])
    table_pred <- rbind(table_pred, temp_df)

  }
  results <- data.frame(Results = mean_1, Dates = seq_along(1:n_predicted))


  plot_1 <- ggplot2::ggplot(table_pred, aes(Dates, Price_predicted)) +
      geom_boxplot(fill = "grey", alpha= 0.4) +
      theme_bw() +
      geom_point(size= 3, alpha= 0.6) +
      geom_hline(yintercept = actual_1,  linetype ="dotdash", color = "darkgreen")+
      geom_hline(yintercept = min_actual,  linetype ="dotdash", color = "blue")+
      geom_hline(yintercept = max_actual,  linetype ="dotdash", color = "red")+
      scale_x_discrete(name = "Future Dates")  +
      ggtitle(paste("Predictions", today), subtitle = paste0(crypto, "-MXN")) +
      ylab("Price ($ MXN)") +
      theme(axis.text.x = element_text(angle = 60, hjust = 0.9, size = 15),
            axis.text.y =element_text(size=20),
            axis.text.y.right = element_text(size=20),
            plot.title = element_text(hjust = 0.5, size=30),
            legend.title = element_text(size = 20),
            axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20),
            plot.subtitle = element_text(hjust = 0.5, size = 15))

  plot_2 <- ggplot2::ggplot(results, aes(Dates, Results)) +
    geom_smooth(method = "gam", color = "black", linetype = "dotted",
                alpha = 0.2)+
      geom_line() +
      theme_bw() +
      geom_point(size= 3, alpha= 0.6) +
      geom_hline(yintercept = actual_1,  linetype ="dotdash", color = "green") +
      geom_hline(yintercept = min_actual,  linetype ="dotdash", color = "blue")+
      geom_hline(yintercept = max_actual,  linetype ="dotdash", color = "red")+
      geom_hline(yintercept = close_actual,  linetype ="dotdash", color = "darkorange")+
      ggtitle(paste("Predictions", today), subtitle = paste0(crypto, "-MXN")) +
      ylab("Price ($ MXN)") +
      scale_x_discrete(name = "Future Dates", limits= seq_along(1:n_predicted),
                       label = days_good)+
      theme(axis.text.x = element_text(angle = 60, hjust = 0.9, size = 15),
            axis.text.y =element_text(size=20),
            axis.text.y.right = element_text(size=20),
            plot.title = element_text(hjust = 0.5, size=30),
            legend.title = element_text(size = 20),
            axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20),
            plot.subtitle = element_text(hjust = 0.5, size = 15))

  all_together <- list(Results = table_pred, plot_1, plot_2)

  return(all_together)

}

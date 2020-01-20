Cripto_plot_predict <- function(df, temp="DAILY", n_predicted=10,
                                crypto = "BTC"){

  message(paste("Plot the predictions of", n_predicted, stringr::str_to_lower(temp)))

  mean_1 <- unclass(df$mean)
  low_1 <- unclass(df$lower)
  high_1 <- unclass(df$upper)
  # plot(predictions)
  actual <- CriptoDummy::Cripto_exchange()
  actual_1 <- as.numeric(as.character(actual$Exchange_Rate))
  # results <- predictions - actual_1

  if (temp=="DAILY"){

    # colnames <- c("Change", "Days")
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


  }


  table_pred <- data.frame()
  for (i in 1:n_predicted) {

    lower <- t(low_1[i,])
    upper <- t(high_1[i,])
    together <- c(lower, mean_1[i], upper)
    temp_df <- data.frame(Price_predicted = together, Dates = df_2[i,])
    table_pred <- rbind(table_pred, temp_df)

  }
  results <- data.frame(Results = mean_1, Days = seq_along(1:n_predicted))
  # results_2 <- cbind(df_2, Predictions = predictions)

  # market_predict <- data.frame(predictions, df_2)

  # library(gridExtra)

  plot_1 <- ggplot(table_pred, aes(Dates, Price_predicted)) +
      geom_boxplot(fill = "grey", alpha= 0.4) +
      theme_bw() +
      geom_point(size= 3, alpha= 0.6) +
      geom_hline(yintercept = actual_1,  linetype ="dotdash", color = "red") +
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
      # annotation_custom(tableGrob(market_predict), xmin=7, xmax=10, ymin=-20, ymax=-10)

  plot_2 <- ggplot(results, aes(Days, Results)) +
    geom_smooth(method = "gam", color = "black", linetype = "dotted",
                alpha = 0.2)+
    # geom_smooth(method = "lm", fill= "orange") +
      geom_line() +
      theme_bw() +
      geom_point(size= 3, alpha= 0.6) +
      geom_hline(yintercept = actual_1,  linetype ="dotdash", color = "red", size = 1) +
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

  # table_grid <- gridExtra::grid.table(results)

  all_together <- list(Results = table_pred, plot_1, plot_2)

  return(all_together)

}

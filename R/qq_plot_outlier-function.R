qq_plot_outlier <- function(value, variable) {
  
  # Create a data frame with your variable value
  
  df <-
    data.frame(value = value) |>
    mutate(
      q1 = quantile(value, 0.25, na.rm = TRUE)
      , q3 = quantile(value, 0.75, na.rm = TRUE)
      , iqr = q3 - q1
      , outlier = 
        ifelse(
          value < q1 - 1.5 * iqr | value > q3 + 1.5 * iqr
          ,"yes"
          ,"no"
        )
    ) |>
    arrange(value) |>
    mutate(theoretical = qnorm(ppoints(length(value))))
  
  # Compute the slope and intercept for the reference line
  probs <- c(0.25, 0.75)
  q_sample <- quantile(df$value, probs, na.rm = TRUE)
  q_theoretical <- qnorm(probs)
  slope <- diff(q_sample) / diff(q_theoretical)
  intercept <- q_sample[1] - slope * q_theoretical[1]
  
  # Create the QQ plot with colored points and custom reference line
  df|>
    ggplot(aes(theoretical, value, color = outlier)) +
    geom_point(na.rm = TRUE) +
    geom_abline(slope = slope, intercept = intercept, color = "red") +
    coord_flip() +
    xlab("Normal Quantiles") +
    ylab(variable)
  
}
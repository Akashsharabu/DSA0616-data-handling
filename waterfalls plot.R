library(waterfalls)
profit_loss_data <- data.frame(
  labels = c("Jan", "Feb", "Mar", "Apr", "May"),
  values = c(100, -20, 50, -10, 80)
)
waterfall(profit_loss_data, fill_by_sign = TRUE)

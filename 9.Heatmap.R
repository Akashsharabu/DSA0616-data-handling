temperature_data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May"),
  City_A = c(5, 6, 7, 8, 9),
  City_B = c(10, 11, 12, 13, 14),
  City_C = c(15, 16, 17, 18, 19)
)
library(reshape2)
melted_data <- melt(temperature_data, id.vars = "Month")
ggplot(melted_data, aes(x = Month, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Monthly Temperatures",
       x = "Month",
       y = "City") +
  theme_minimal()

library(ggplot2)

sales_data <- data.frame(
  product = c("A", "B", "C", "D", "E"),
  before = c(200, 300, 400, 500, 600),
  after = c(250, 350, 450, 550, 650)
)

ggplot(sales_data, aes(y = product)) +
  geom_segment(aes(x = before, xend = after, yend = product), color = "blue", size = 1) +
  geom_point(aes(x = before), color = "red", size = 4) +
  geom_point(aes(x = after), color = "green", size = 4) +
  labs(
    title = "Sales Before and After Campaign",
    x = "Sales",
    y = "Product"
  ) +
  theme_minimal()

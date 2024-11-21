library(ggplot2)

temperature_data <- data.frame(
  City = c("City1", "City1", "City1", "City2", "City2", "City2"),
  Temperature = c(20, 21, 19, 22, 23, 24)
)

ggplot(temperature_data, aes(x = Temperature, fill = City)) +
  geom_density(alpha = 0.7) +  
  facet_wrap(~ City, ncol = 1, scales = "free_y") + 
  labs(title = "Temperature Distributions",
       x = "Temperature",
       y = "Density") +
  theme_minimal()

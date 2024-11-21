# Load necessary libraries
library(ggplot2)
library(plotly)
library(akima)
consumer_data <- data.frame(
  Product = c("A", "B", "C", "D", "E"),
  Price = c(50, 70, 60, 45, 55),
  Rating = c(4.2, 3.8, 4.0, 4.5, 3.9),
  AgeGroup = c("25-35", "35-45", "18-25", "45-55", "25-35")
)
consumer_data$AgeGroupNumeric <- as.numeric(factor(consumer_data$AgeGroup, 
                                                   levels = c("18-25", "25-35", "35-45", "45-55")))
cor_matrix <- cor(consumer_data[, c("Price", "Rating", "AgeGroupNumeric")])
print("Correlation Matrix:")
print(cor_matrix)
scatter_plot <- plot_ly(consumer_data, 
                        x = ~Price, 
                        y = ~AgeGroupNumeric, 
                        z = ~Rating, 
                        color = ~Rating, 
                        colors = colorRamp(c("blue", "red")), 
                        type = "scatter3d", 
                        mode = "markers") %>%
  layout(title = "3D Scatter: Price vs Age Group (Rating as Color)",
         scene = list(
           xaxis = list(title = "Price ($)"),
           yaxis = list(title = "Age Group (Numeric)"),
           zaxis = list(title = "Rating")))
scatter_plot
interp_data <- with(consumer_data, interp(Price, AgeGroupNumeric, Rating))
surface_plot <- plot_ly(x = interp_data$x, 
                        y = interp_data$y, 
                        z = interp_data$z, 
                        type = "surface", 
                        colorscale = "Viridis") %>%
  layout(title = "3D Surface: Rating vs Price and Age Group",
         scene = list(
           xaxis = list(title = "Price ($)"),
           yaxis = list(title = "Age Group (Numeric)"),
           zaxis = list(title = "Rating")))
surface_plot
interp_price <- with(consumer_data, interp(Price, seq(min(AgeGroupNumeric), max(AgeGroupNumeric), length.out=5), Rating))
surface_price <- plot_ly(x = interp_price$x, 
                         y = interp_price$y, 
                         z = interp_price$z, 
                         type = "surface", 
                         colorscale = "Blues") %>%
  layout(title = "3D Surface: Rating vs Price",
         scene = list(
           xaxis = list(title = "Price ($)"),
           yaxis = list(title = "Constant Age Group"),
           zaxis = list(title = "Rating")))
surface_price
interp_agegroup <- with(consumer_data, interp(seq(min(Price), max(Price), length.out=5), AgeGroupNumeric, Rating))
surface_agegroup <- plot_ly(x = interp_agegroup$x, 
                            y = interp_agegroup$y, 
                            z = interp_agegroup$z, 
                            type = "surface", 
                            colorscale = "Reds") %>%
  layout(title = "3D Surface: Rating vs Age Group",
         scene = list(
           xaxis = list(title = "Constant Price"),
           yaxis = list(title = "Age Group (Numeric)"),
           zaxis = list(title = "Rating")))
surface_agegroup

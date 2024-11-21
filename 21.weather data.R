library(ggplot2)
library(plotly)
library(akima)
weather_data <- data.frame(
  Date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
  Temperature = c(10, 12, 8, 15, 14),
  Humidity = c(75, 70, 80, 65, 72),
  WindSpeed = c(15, 12, 18, 20, 16)
)
cor_matrix <- cor(weather_data[, c("Temperature", "Humidity", "WindSpeed")])
print("Correlation Matrix:")
print(cor_matrix)
scatter_plot <- plot_ly(weather_data, 
                        x = ~Humidity, 
                        y = ~WindSpeed, 
                        z = ~Temperature, 
                        color = ~Temperature, 
                        colors = colorRamp(c("blue", "red")), 
                        type = "scatter3d", 
                        mode = "markers") %>%
  layout(title = "3D Scatter: Wind Speed vs Humidity (Temperature as Color)")
scatter_plot
interp_data <- with(weather_data, interp(Humidity, WindSpeed, Temperature))
surface_plot <- plot_ly(x = interp_data$x, 
                        y = interp_data$y, 
                        z = interp_data$z, 
                        type = "surface", 
                        colorscale = "Viridis") %>%
  layout(title = "3D Surface: Temperature vs Humidity and Wind Speed",
         scene = list(
           xaxis = list(title = "Humidity (%)"),
           yaxis = list(title = "Wind Speed (km/h)"),
           zaxis = list(title = "Temperature (°C)")))
surface_plot
interp_humidity <- with(weather_data, interp(Humidity, seq(min(WindSpeed), max(WindSpeed), length.out=5), Temperature))
surface_humidity <- plot_ly(x = interp_humidity$x, 
                            y = interp_humidity$y, 
                            z = interp_humidity$z, 
                            type = "surface", 
                            colorscale = "Blues") %>%
  layout(title = "3D Surface: Temperature vs Humidity",
         scene = list(
           xaxis = list(title = "Humidity (%)"),
           yaxis = list(title = "Constant Wind Speed"),
           zaxis = list(title = "Temperature (°C)")))
surface_humidity
interp_windspeed <- with(weather_data, interp(seq(min(Humidity), max(Humidity), length.out=5), WindSpeed, Temperature))
surface_windspeed <- plot_ly(x = interp_windspeed$x, 
                             y = interp_windspeed$y, 
                             z = interp_windspeed$z, 
                             type = "surface", 
                             colorscale = "Reds") %>%
  layout(title = "3D Surface: Temperature vs Wind Speed",
         scene = list(
           xaxis = list(title = "Constant Humidity"),
           yaxis = list(title = "Wind Speed (km/h)"),
           zaxis = list(title = "Temperature (°C)")))
surface_windspeed

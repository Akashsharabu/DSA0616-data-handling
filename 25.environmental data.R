library(ggplot2)
library(plotly)
library(akima)
environment_data <- data.frame(
  Location = c("A", "B", "C", "D", "E"),
  Temperature = c(15, 20, 18, 12, 17),
  Humidity = c(65, 70, 68, 60, 72),
  CO2Levels = c(400, 450, 420, 380, 430)
)
cor_matrix <- cor(environment_data[, c("Temperature", "Humidity", "CO2Levels")])
print("Correlation Matrix:")
print(cor_matrix)
scatter_plot <- plot_ly(environment_data, 
                        x = ~Temperature, 
                        y = ~Humidity, 
                        z = ~CO2Levels, 
                        color = ~CO2Levels, 
                        colors = colorRamp(c("blue", "red")), 
                        type = "scatter3d", 
                        mode = "markers") %>%
  layout(title = "3D Scatter: Temperature vs Humidity (CO2 Levels as Color)",
         scene = list(
           xaxis = list(title = "Temperature (°C)"),
           yaxis = list(title = "Humidity (%)"),
           zaxis = list(title = "CO2 Levels (ppm)")))
scatter_plot
interp_data <- with(environment_data, interp(Temperature, Humidity, CO2Levels))
surface_plot <- plot_ly(x = interp_data$x, 
                        y = interp_data$y, 
                        z = interp_data$z, 
                        type = "surface", 
                        colorscale = "Viridis") %>%
  layout(title = "3D Surface: CO2 Levels vs Temperature and Humidity",
         scene = list(
           xaxis = list(title = "Temperature (°C)"),
           yaxis = list(title = "Humidity (%)"),
           zaxis = list(title = "CO2 Levels (ppm)")))
surface_plot
interp_temp <- with(environment_data, interp(Temperature, seq(min(Humidity), max(Humidity), length.out=5), CO2Levels))
surface_temp <- plot_ly(x = interp_temp$x, 
                        y = interp_temp$y, 
                        z = interp_temp$z, 
                        type = "surface", 
                        colorscale = "Blues") %>%
  layout(title = "3D Surface: CO2 Levels vs Temperature",
         scene = list(
           xaxis = list(title = "Temperature (°C)"),
           yaxis = list(title = "Constant Humidity"),
           zaxis = list(title = "CO2 Levels (ppm)")))
surface_temp
interp_humidity <- with(environment_data, interp(seq(min(Temperature), max(Temperature), length.out=5), Humidity, CO2Levels))
surface_humidity <- plot_ly(x = interp_humidity$x, 
                            y = interp_humidity$y, 
                            z = interp_humidity$z, 
                            type = "surface", 
                            colorscale = "Reds") %>%
  layout(title = "3D Surface: CO2 Levels vs Humidity",
         scene = list(
           xaxis = list(title = "Constant Temperature"),
           yaxis = list(title = "Humidity (%)"),
           zaxis = list(title = "CO2 Levels (ppm)")))
surface_humidity

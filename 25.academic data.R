library(ggplot2)
library(plotly)
library(akima)
academic_data <- data.frame(
  Student = c("A", "B", "C", "D", "E"),
  MathScore = c(85, 72, 90, 78, 88),
  ScienceScore = c(78, 85, 80, 75, 82),
  Attendance = c(95, 92, 98, 85, 93)
)
cor_matrix <- cor(academic_data[, c("MathScore", "ScienceScore", "Attendance")])
print("Correlation Matrix:")
print(cor_matrix)
scatter_plot <- plot_ly(academic_data, 
                        x = ~MathScore, 
                        y = ~Attendance, 
                        z = ~ScienceScore, 
                        color = ~ScienceScore, 
                        colors = colorRamp(c("blue", "red")), 
                        type = "scatter3d", 
                        mode = "markers") %>%
  layout(title = "3D Scatter: Math Score vs Attendance (Science Score as Color)",
         scene = list(
           xaxis = list(title = "Math Score"),
           yaxis = list(title = "Attendance (%)"),
           zaxis = list(title = "Science Score")))
scatter_plot
interp_data <- with(academic_data, interp(MathScore, Attendance, ScienceScore))
surface_plot <- plot_ly(x = interp_data$x, 
                        y = interp_data$y, 
                        z = interp_data$z, 
                        type = "surface", 
                        colorscale = "Viridis") %>%
  layout(title = "3D Surface: Science Score vs Math Score and Attendance",
         scene = list(
           xaxis = list(title = "Math Score"),
           yaxis = list(title = "Attendance (%)"),
           zaxis = list(title = "Science Score")))
surface_plot
interp_math <- with(academic_data, interp(MathScore, seq(min(Attendance), max(Attendance), length.out=5), ScienceScore))
surface_math <- plot_ly(x = interp_math$x, 
                        y = interp_math$y, 
                        z = interp_math$z, 
                        type = "surface", 
                        colorscale = "Blues") %>%
  layout(title = "3D Surface: Science Score vs Math Score",
         scene = list(
           xaxis = list(title = "Math Score"),
           yaxis = list(title = "Constant Attendance"),
           zaxis = list(title = "Science Score")))
surface_math
interp_attendance <- with(academic_data, interp(seq(min(MathScore), max(MathScore), length.out=5), Attendance, ScienceScore))
surface_attendance <- plot_ly(x = interp_attendance$x, 
                              y = interp_attendance$y, 
                              z = interp_attendance$z, 
                              type = "surface", 
                              colorscale = "Reds") %>%
  layout(title = "3D Surface: Science Score vs Attendance",
         scene = list(
           xaxis = list(title = "Constant Math Score"),
           yaxis = list(title = "Attendance (%)"),
           zaxis = list(title = "Science Score")))
surface_attendance

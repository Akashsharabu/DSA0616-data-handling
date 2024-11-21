library(circlize)
chord_data <- data.frame(
  From = c("A", "A", "B", "B", "C"),
  To = c("B", "C", "C", "D", "D"),
  Value = c(10, 20, 15, 25, 30)
)
chordDiagram(chord_data)

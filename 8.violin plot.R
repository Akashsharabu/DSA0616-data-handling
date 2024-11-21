scores_group_data <- data.frame(
  Group = c("X", "X", "X", "X", "Y", "Y", "Y", "Y"),
  Score = c(80, 85, 78, 92, 88, 76, 80, 84)
)
ggplot(scores_group_data, aes(x = Group, y = Score, fill = Group)) +
  geom_violin() +
  labs(title = "Scores by Group",
       x = "Group",
       y = "Score") +
  theme_minimal()

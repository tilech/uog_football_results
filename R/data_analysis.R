data_freq <- data %>%
  group_by(FTR) %>%
  summarize(freq = n(),
            freq_pctg = n() / nrow(data))

ggplot(data = data, aes(x = factor(FTR, levels = c("H", "D", "A")))) +
  geom_bar(fill = "#008080", color = "black", width = 0.6) +  # Custom colors & bar width
  scale_x_discrete(labels = c("H" = "Home Win", "D" = "Draw", "A" = "Away Win")) +
  labs(x = NULL, y = "Frequency") +  # Improved labels
  theme_minimal(base_size = 14) +  # Slightly larger base font
  theme(
    axis.text.x = element_text(face = "bold")  # Bold y-axis labels
  )

data_home <- data %>%
  group_by(HomeTeam) %>%
  summarize(home_games = n())

data_away <- data %>%
  group_by(AwayTeam) %>%
  summarize(away_games = n())

data_join <- data_home %>%
  left_join(data_away, by = join_by(HomeTeam == AwayTeam)) %>%
  rename(Team = HomeTeam) %>%
  mutate(total_games = home_games + away_games)

ggplot(data = data_join, aes(x = reorder(Team, -total_games), y = total_games, fill = Team)) +
  geom_col(show.legend = FALSE, width = 0.7, color = "black") +  # Adds black borders for contrast
  scale_fill_manual(values = team_colors) +
  labs(x = NULL, y = "Total Games") +
  theme_minimal(base_size = 14) +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold")
  )

data_long <- data %>%
  gather(key = "variable", value = "goals", FTHG, FTAG)

# Recode the 'variable' column if necessary
data_long$variable <- recode(data_long$variable, 
                             "FTHG" = "Home", 
                             "FTAG" = "Away")

ggplot(data = data_long, aes(x = goals, fill = factor(variable, levels = c("Home", "Away")))) +  # Control factor order
  geom_bar(position = position_dodge2(), color = "black", width = 0.6) +  # Side-by-side bars
  labs(x = NULL, y = "Frequency") + 
  scale_x_continuous(breaks = seq(min(c(data$FTHG, data$FTAG)), max(c(data$FTHG, data$FTAG)), by = 1)) +  # Ensure integer breaks
  theme_minimal(base_size = 14) +  
  theme(
    axis.text.x = element_text(face = "bold"),  # Bold x-axis labels
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    legend.title = element_blank(),  # Remove the legend title
    legend.text = element_text(size = 12),  # Increase legend text size for clarity
    legend.key.size = unit(1, "cm")  # Adjust size of legend keys
  ) +
  scale_fill_manual(values = c("Home" = "#008080", "Away" = "#FF6347"),  # Correct color for home and away
                    labels = c("Home Goals", "Away Goals"))  # Correct legend labels


ggplot(data, aes(x = Date, y = FTHG + FTAG)) +
  geom_point(stat = "summary", fun = mean, color = "#008080") +
  theme_minimal(base_size = 14) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(x = NULL, y = "Total Goals")

analysis_data <- data[,5:20]
numeric_data <- analysis_data[, sapply(analysis_data, is.numeric)]
cor_matrix <- cor(numeric_data, use = "complete.obs")
ggcorrplot(cor_matrix, 
           method = "square",  # Use square tiles
           type = "lower",  # Show only lower triangle to avoid redundancy
           lab = TRUE,  # Show correlation values inside the squares
           lab_size = 3,  # Adjust label size
           colors = c("#BB4444", "#FFFFFF", "#4477AA"),  # Red-White-Blue gradient
           ggtheme = theme_minimal())

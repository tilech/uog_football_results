data_plot <- dixon_coles_model$grid_search_plot$data
ggplot(data=data_plot, aes(x=time_decay, y=rps)) +
  geom_line()+
  geom_point() +
  scale_x_continuous(breaks=seq(0.0, 0.002, 0.0002)) +
  theme_minimal() +
  labs(x="Time Decay", y = "RPS")

data_plot <- dixon_coles_model$params_plot$data
ggplot(data_plot, aes(x = Defense, y = Attack, label = Team)) + 
  geom_point(aes(color = Team), size = 3) +    # Only the points have color
  geom_text_repel(vjust = -0.5, hjust = 0.5, max.overlaps = 10) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_vline(xintercept = 0, linewidth = 1) +
  labs(x = "Defense Strength", y = "Attack Strength") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = team_colors) +
  theme(legend.position="none")

data_plot <- dixon_coles_model$scatter_comp_plot$data
ggplot(data_plot, aes(x = rank.actual, y = rank.predicted, label = team)) +
  geom_point(aes(color = team), size = 3) +
  geom_text_repel(vjust = 1.5, hjust = 0.5, size = 3, max.overlaps = 10) +  # Keep text in default color
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = team_colors) +  # Use team colors for points
  labs(x = "Actual Rank", 
       y = "Predicted Rank",
       color = "Team") +  # Keep only the color legend
  theme_minimal(base_size = 18) +
  theme(legend.position="none")

data_plot <- dixon_coles_model$bar_comp_plot$data
ggplot(data_plot, aes(x = reorder(team, points.actual))) +
  geom_point(aes(y = points.actual, shape = "Actual", color = team), size = 3) +
  geom_point(aes(y = points.predicted, shape = "Predicted", color = team), size = 3) +
  geom_segment(aes(y = points.actual, yend = points.predicted, xend = reorder(team, points.actual), color = team), size = 1) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_shape_manual(values = c("Actual" = 16, "Predicted" = 17)) +
  labs(x = "",
       y = "Points",
       shape = NULL) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 18))

data_plot <- baio_blangiardo_model$grid_search_plot$data
n<-dim(data_plot)[1]
data_plot<-data_plot[1:(n-1),]
ggplot(data=data_plot, aes(x=start_season, y=rps)) +
  geom_line()+
  geom_point() +
  scale_x_continuous(breaks=seq(min(data_plot$start_season), max(data_plot$start_season), 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(x=NULL, y = "RPS")

data_plot <- baio_blangiardo_model$ad_plot$attack$data
ggplot(data_plot, aes(x = Team, y = AttackStrength)) +
  geom_boxplot(fill = "red", alpha = 0.7, coef = 10) +
  labs(x = "", y = "Attack Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_plot <- baio_blangiardo_model$ad_plot$defense$data
ggplot(data_plot, aes(x = Team, y = DefenseStrength)) +
  geom_boxplot(fill = "blue", alpha = 0.7, coef = 10) +
  labs(x = "", y = "Defense Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_plot <- baio_blangiardo_model$scatter_comp_plot$data
ggplot(data_plot, aes(x = rank.actual, y = rank.predicted, label = team)) +
  geom_point(aes(color = team), size = 3) +
  geom_text_repel(vjust = 1.5, hjust = 0.5, size = 3, max.overlaps = 10) +  # Keep text in default color
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = team_colors) +  # Use team colors for points
  labs(x = "Actual Rank", 
       y = "Predicted Rank",
       color = "Team") +  # Keep only the color legend
  theme_minimal(base_size = 18) +
  theme(legend.position="none")

data_plot <- baio_blangiardo_model$bar_comp_plot$data
ggplot(data_plot, aes(x = reorder(team, points.actual))) +
  geom_point(aes(y = points.actual, shape = "Actual", color = team), size = 3) +
  geom_point(aes(y = points.predicted, shape = "Predicted", color = team), size = 3) +
  geom_segment(aes(y = points.actual, yend = points.predicted, xend = reorder(team, points.actual), color = team), size = 1) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_shape_manual(values = c("Actual" = 16, "Predicted" = 17)) +
  labs(x = "",
       y = "Points",
       shape = NULL) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 18))

data_plot <- bayesian_model$grid_search_plot$data
n<-dim(data_plot)[1]
data_plot<-data_plot[1:(n-1),]
ggplot(data=data_plot, aes(x=start_season, y=rps)) +
  geom_line()+
  geom_point() +
  scale_x_continuous(breaks=seq(min(data_plot$start_season), max(data_plot$start_season), 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(x=NULL, y = "RPS")

data_plot <- bayesian_model$ad_plot$attack$data
ggplot(data_plot, aes(x = Team, y = AttackStrength)) +
  geom_boxplot(fill = "red", alpha = 0.7, coef = 10) +
  labs(x = "", y = "Attack Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_plot <- bayesian_model$ad_plot$defense$data
ggplot(data_plot, aes(x = Team, y = DefenseStrength)) +
  geom_boxplot(fill = "blue", alpha = 0.7, coef = 10) +
  labs(x = "", y = "Defense Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_plot <- bayesian_model$hta_plot$data
ggplot(data_plot, aes(x = Team, y = HomeTeamAdvantage)) +
  geom_boxplot(fill = "green", alpha = 0.7, coef = 10) +
  labs(x = "", y = "Home Team Advantage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_plot <- bayesian_model$scatter_comp_plot$data
ggplot(data_plot, aes(x = rank.actual, y = rank.predicted, label = team)) +
  geom_point(aes(color = team), size = 3) +
  geom_text_repel(vjust = 1.5, hjust = 0.5, size = 3, max.overlaps = 10) +  # Keep text in default color
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = team_colors) +  # Use team colors for points
  labs(x = "Actual Rank", 
       y = "Predicted Rank",
       color = "Team") +  # Keep only the color legend
  theme_minimal(base_size = 18) +
  theme(legend.position="none")

data_plot <- bayesian_model$bar_comp_plot$data
ggplot(data_plot, aes(x = reorder(team, points.actual))) +
  geom_point(aes(y = points.actual, shape = "Actual", color = team), size = 3) +
  geom_point(aes(y = points.predicted, shape = "Predicted", color = team), size = 3) +
  geom_segment(aes(y = points.actual, yend = points.predicted, xend = reorder(team, points.actual), color = team), size = 1) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_shape_manual(values = c("Actual" = 16, "Predicted" = 17)) +
  labs(x = "",
       y = "Points",
       shape = NULL) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 18))

data_plot <- neural_network_model$scatter_comp_plot$data
ggplot(data_plot, aes(x = rank.actual, y = rank.predicted, label = team)) +
  geom_point(aes(color = team), size = 3) +
  geom_text_repel(vjust = 1.5, hjust = 0.5, size = 3, max.overlaps = 10) +  # Keep text in default color
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = team_colors) +  # Use team colors for points
  labs(x = "Actual Rank", 
       y = "Predicted Rank",
       color = "Team") +  # Keep only the color legend
  theme_minimal(base_size = 18) +
  theme(legend.position="none")

data_plot <- neural_network_model$bar_comp_plot$data
ggplot(data_plot, aes(x = reorder(team, points.actual))) +
  geom_point(aes(y = points.actual, shape = "Actual", color = team), size = 3) +
  geom_point(aes(y = points.predicted, shape = "Predicted", color = team), size = 3) +
  geom_segment(aes(y = points.actual, yend = points.predicted, xend = reorder(team, points.actual), color = team), size = 1) +
  scale_color_manual(values = team_colors, guide = "none") +
  scale_shape_manual(values = c("Actual" = 16, "Predicted" = 17)) +
  labs(x = "",
       y = "Points",
       shape = NULL) +
  theme_minimal(base_size = 18) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.15),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 18))


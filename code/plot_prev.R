source('data_prev.R')

##-------------Paired plot: x = subject, y = value, lines connect before/after--
df_plot = df_rst%>%
  arrange(prev_as50) %>%
  mutate(ID = row_number()*2) %>%
  pivot_longer(cols = c(prev_as50, prev_overall),
               names_to = "condition", values_to = "value") %>%
  mutate(condition = recode(condition,
                            "prev_as50" = "Home",
                            "prev_overall" = "Overall"),
         anchor_inside = ifelse(anchor_inside, "Inside", "Outside")) 
df_plot$anchor_inside <- factor(df_plot$anchor_inside, 
                                levels = c("Outside", "Inside"))

ggplot(df_plot, aes(x = ID, y = value, group = IIntID)) +
  geom_line(color = "black", linewidth = 0.2) +
  geom_point(aes(shape = condition), size = 1) +
  facet_grid(. ~ anchor_inside, scales = "free_x", space = "free_x") +
  scale_shape_manual(values = c("Home" = 16, "Overall" = 1))+
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.9, 0.1),        # Moves legend inside the plot: (1=right, 0=bottom)
    legend.justification = c(0.9, 0.1), 
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5), # White background with border
    ) +
  labs(y = "Exposure", x = "Rank(Study Participants)", shape = NULL)
ggsave("plot/ttest_home_overall.png", width = 7, height = 3)

library(ggplot2)
library(ggridges)
library(dplyr)
library(patchwork)

df <- read.csv("sentiment_cluster.csv") %>%
  filter(cluster != 3)

# ---- Ridgeline plot ----
intervals <- df %>%
  group_by(cluster) %>%
  summarise(mean_compound = mean(compound), .groups = "drop")

df <- df %>% left_join(intervals, by = "cluster")

p1 <- ggplot(df, aes(x = compound, y = factor(cluster, levels = rev(unique(cluster))), 
                     fill = mean_compound)) +
  geom_density_ridges(alpha = 0.8, scale = 0.8, rel_min_height = 0.02, color = "white") +
  geom_point(
    data = intervals,
    aes(x = mean_compound, y = factor(cluster, levels = rev(unique(cluster)))),
    color = "black", size = 2.5
  ) +
  geom_text(
    data = intervals,
    aes(x = mean_compound, y = factor(cluster, levels = rev(unique(cluster))), 
        label = sprintf("%.2f", mean_compound)),
    vjust = -0.8, size = 4, color = "black"
  ) +
  scale_fill_gradient2(
    low = "tomato", mid = "gray80", high = "skyblue", 
    midpoint = 0, limits = c(-1, 1), name = "Mean Sentiment   ",
    guide = guide_colorbar(
      barwidth = 10,
      title.position = "left"
    )
  ) +
  labs(
    x = "VADER Sentiment Score",
    y = "Cluster",
    title = "Sentiment Distribution per Cluster",
    subtitle = "Color shows mean sentiment per cluster"
  ) +
  theme_ridges(center_axis_labels = TRUE) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

# ---- Stacked horizontal bar chart ----
df <- df %>%
  mutate(sentiment = case_when(
    compound >= 0.05  ~ "Positive",
    compound <= -0.05 ~ "Negative",
    TRUE              ~ "Neutral"
  ))

sent_counts <- df %>%
  group_by(cluster, sentiment) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percentage = count / sum(count) * 100)

# Reverse order
sent_counts$cluster <- factor(sent_counts$cluster, 
                              levels = rev(sort(unique(sent_counts$cluster))))

p2 <- ggplot(sent_counts, aes(y = cluster, x = percentage, fill = factor(sentiment, levels = rev(sort(unique(sentiment)))))) +
  geom_bar(stat = "identity") +
  labs(x = "Percentage (%)", y = NULL, title = "Sentiment Breakdown", fill = "") +
  scale_fill_manual(values = c("Negative" = "tomato", 
                               "Neutral" = "gray70", 
                               "Positive" = "skyblue")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# ---- Combine plots ----
options(repr.plot.width = 16, repr.plot.height = 8)
p1 + p2 + plot_layout(widths = c(1, 1))
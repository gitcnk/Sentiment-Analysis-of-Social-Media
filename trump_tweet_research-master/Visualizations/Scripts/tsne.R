# Run after the dendrogram script

library(Rtsne)
library(ggplot2)

# Remove duplicates to prevent t-SNE error
X_unique <- X[!duplicated(X), ]
pyclusters_unique <- pyclusters[!duplicated(X)]

# Run t-SNE
set.seed(37)
tsne_result <- Rtsne(X_unique, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 8000)

plot_df <- data.frame(
  TSNE1 = tsne_result$Y[,1],
  TSNE2 = tsne_result$Y[,2],
  Cluster = factor(pyclusters_unique)
)
plot_df$ClusterLabel <- paste0("Cluster ", plot_df$Cluster)

# Symmetric range for grid
x_center <- mean(range(plot_df$TSNE1))
y_center <- mean(range(plot_df$TSNE2))
half_x <- max(abs(plot_df$TSNE1 - x_center)) * 1.05
half_y <- max(abs(plot_df$TSNE2 - y_center)) * 1.05
x_lim <- c(x_center - half_x, x_center + half_x)
y_lim <- c(y_center - half_y, y_center + half_y)

# Define breaks for evenly spaced grid lines
n_grid <- 8
x_breaks <- seq(x_lim[1], x_lim[2], length.out = n_grid)
y_breaks <- seq(y_lim[1], y_lim[2], length.out = n_grid)

# Plot
ggplot(plot_df, aes(x = TSNE1, y = TSNE2, color = ClusterLabel)) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_manual(values = cols) +
  scale_x_continuous(limits = x_lim, breaks = x_breaks) +
  scale_y_continuous(limits = y_lim, breaks = y_breaks) +
  theme_minimal(base_size = 15) +
  theme(
    panel.border = element_blank(),         
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(face = "bold", size = 14),
    legend.key.size = unit(2, "lines"),
    legend.box.margin = margin(10, 0, 10, -10),
    legend.title = element_blank(),
    legend.key.width = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    color = guide_legend(
      title = NULL,
      keyheight = unit(1.6, "lines"),
      default.unit = "lines",
      override.aes = list(size = 4)
    )
  )
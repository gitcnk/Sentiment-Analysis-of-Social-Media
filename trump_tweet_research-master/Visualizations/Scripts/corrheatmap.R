library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)
library(grid)

reorder_topic_columns <- function(clusterid) {
  keywords_file <- sprintf("./HC_text/HC_%s_text/keywords.tsv", clusterid)
  composition_file <- sprintf("./HC_text/HC_%s_text/composition.tsv", clusterid)
  
  if (!file.exists(keywords_file) || !file.exists(composition_file)) {
    stop(sprintf("Files not found: %s or %s", keywords_file, composition_file))
  }
  
  keywords <- read_tsv(keywords_file, col_names = FALSE, show_col_types = FALSE) %>%
    mutate(X1 = as.integer(X1), X2 = as.numeric(X2))
  
  if (nrow(keywords) == 0) stop("keywords.tsv is empty")
  
  # Sort keywords by descending X2 (Dirichlet parameter)
  keywords_sorted <- keywords %>% arrange(desc(X2))
  new_order <- keywords_sorted$X1
  
  composition <- read_tsv(composition_file, col_names = FALSE, show_col_types = FALSE)
  n_topic <- nrow(keywords_sorted)
  topic_cols <- 3:(2 + n_topic)
  
  # Validate number of topic columns
  if (ncol(composition) < 2 + n_topic) {
    stop(sprintf("composition.tsv has fewer topic columns (%d) than expected (%d)", 
                 ncol(composition) - 2, n_topic))
  }
  
  # Reorder topic columns based on new_order (X1 is 0-based)
  composition_reordered <- composition %>%
    mutate(across(all_of(topic_cols), as.numeric)) %>%
    select(1:2, all_of(topic_cols[new_order + 1]))
  
  return(list(composition = composition_reordered, n_topic = n_topic))
}

clr_transform <- function(mat) {
  # Apply CLR to each row (sample)
  apply(mat, 1, function(x) {
    x <- x + 1e-4  # Pseudo-count
    gm <- exp(mean(log(x)))  # Geometric mean for this row
    log(x / gm)
  }) |> t()  # Transpose to keep original dimensions (n_tweet x n_topic)
}

library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(scales)
library(grid)  # for unit()

plot_topic_heatmap_clr <- function(clusterid) {
  res <- reorder_topic_columns(clusterid)
  composition <- res$composition
  n_topic <- res$n_topic
  
  # Rename topic columns for clarity / consistent ordering
  colnames(composition)[3:(2 + n_topic)] <- paste0("Topic ", 1:n_topic)
  
  # CLR transform (uses your clr_transform)
  topic_matrix <- as.matrix(composition[, 3:(2 + n_topic)])
  topic_clr <- clr_transform(topic_matrix)
  
  # Correlation on CLR data
  cor_matrix <- cor(topic_clr, use = "pairwise.complete.obs", method = "pearson")
  
  # Melt to long
  cor_long <- reshape2::melt(cor_matrix, varnames = c("Topic1", "Topic2"), value.name = "Correlation")
  cor_long$Topic1 <- factor(cor_long$Topic1, levels = paste0("Topic ", 1:n_topic))
  cor_long$Topic2 <- factor(cor_long$Topic2, levels = paste0("Topic ", 1:n_topic))
  
  # Create plot_value: only color lower triangle (Topic1 > Topic2)
  cor_long$plot_value <- cor_long$Correlation
  cor_long$plot_value[as.numeric(cor_long$Topic1) <= as.numeric(cor_long$Topic2)] <- NA
  
  # Interpolated PuOr palette
  puor_pal <- colorRampPalette(brewer.pal(11, "PuOr"))(200)
  
  # Sizes / fonts — increase for readability
  base_font <- 14      # base font size for theme
  tile_label_size <- 5 # geom_text size for numbers
  
  p <- ggplot(cor_long, aes(x = Topic1, y = Topic2)) +
    # colored tiles for lower triangle; NA tiles (upper triangle & diagonal) will use na.value
    geom_tile(aes(fill = plot_value), color = "white", size = 0.25) +
    # numeric labels everywhere (lower and upper triangles + diagonal)
    geom_text(aes(label = sprintf("%.2f", Correlation)),
              size = tile_label_size,
              color = "#222222",
              na.rm = FALSE) +
    scale_fill_gradientn(
      colors = puor_pal,
      limits = c(-1, 1),
      na.value = "#f5f5f5",  # slightly grey background for upper triangle & diagonal
      breaks = seq(-1, 1, by = 0.5),
      guide = guide_colorbar(
        title = "Correlation",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(0.6, "cm"),
        barheight = unit(6.5, "cm"),
        ticks = TRUE,
        frame.colour = NA,
        title.theme = element_text(margin = margin(b = 12))  # <- add space below title
      )
    ) +
    coord_fixed() +
    labs(x = NULL, y = NULL, fill = "Correlation") +
    theme_minimal(base_size = base_font) +
    theme(
      # Remove outer border box and backgrounds
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      # Axis text / legend text colors & sizes
      axis.text.x = element_text(angle = 0, hjust = 0.5, color = "#222222", face = "bold", size = base_font),
      axis.text.y = element_text(color = "#222222", face = "bold", size = base_font),
      # Hide grid
      panel.grid = element_blank(),
      # Put legend (colorbar) to the right (vertical) and style
      legend.position = "right",
      legend.title = element_text(color = "#222222", size = base_font, face = "bold"),
      legend.text = element_text(color = "#222222", size = base_font),
      # Tidy margins
      plot.margin = margin(6, 6, 6, 6)
    )
  
  return(p)
}

plot_topic_heatmap_clr(7)

# Load required libraries
library(tm)          # for text cleaning + stopwords
library(dplyr)       # for data manipulation
library(ggplot2)     # for plotting
library(ggwordcloud) # for word clouds
library(RColorBrewer)# for color palettes
library(patchwork)   # for combining plots

# ---- User settings ----
cluster_id <- 7                 # set cluster ID
regex_list <- c(                # three regex filters for top-left, top-right, bottom-left
  "China|Beijing|Xi",
  "North Korea|Kim Jong|Kim Prepared|Chairman Kim",
  "Russia|Putin"
)
names(regex_list) <- c("China Filter", "North Korea Filter", "Russia Filter")
top_n <- 10                     # number of top words to highlight
min_freq <- 2                   # minimum frequency for words

# Panel-specific parameters
panel_params <- list(
  "China Filter" = list(grid_size = .8, max_size = 10),
  "North Korea Filter" = list(grid_size = .8, max_size = 8),
  "Russia Filter" = list(grid_size = .8, max_size = 8),
  "Unfiltered" = list(grid_size = .5, max_size = 8)
)

# File (tab-separated; 3rd column contains the text)
file_name <- paste0("HC_", cluster_id, "_text.csv")
data <- read.delim(file_name, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
texts_all <- data[[3]]

# ---- Helper: clean texts and compute word frequencies ----
build_word_freq <- function(text_vec, top_n = 10, min_freq = 1, pal_name = "Set2") {
  if (length(text_vec) == 0) {
    return(data.frame(word = character(0), freq = numeric(0), color = character(0)))
  }
  corpus <- Corpus(VectorSource(text_vec))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  dtm <- TermDocumentMatrix(corpus)
  m <- as.matrix(dtm)
  freq <- sort(rowSums(m), decreasing = TRUE)
  freq <- freq[freq >= min_freq]
  if (length(freq) == 0) {
    return(data.frame(word = character(0), freq = numeric(0), color = character(0)))
  }
  
  wf <- data.frame(word = names(freq), freq = as.numeric(freq), stringsAsFactors = FALSE)
  pal <- brewer.pal(8, pal_name)
  wf$color <- rep("grey70", nrow(wf))
  n_highlight <- min(top_n, nrow(wf))
  if (n_highlight > 0) {
    highlight_cols <- pal[rep(seq_along(pal), length.out = n_highlight)]
    wf$color[seq_len(n_highlight)] <- highlight_cols
  }
  return(wf)
}

# ---- Build data for each panel ----
plot_dfs <- list()

# Three filtered panels
for (nm in names(regex_list)) {
  regex <- regex_list[[nm]]
  rows_with_keyword <- grepl(regex, texts_all, ignore.case = TRUE)
  texts_filtered <- texts_all[rows_with_keyword]
  texts_filtered <- gsub(paste0("\\b(", regex, ")\\b"), "", texts_filtered, ignore.case = TRUE)
  wf <- build_word_freq(texts_filtered, top_n = top_n, min_freq = min_freq)
  wf$panel <- nm
  plot_dfs[[nm]] <- wf
}

# Unfiltered panel
wf_unf <- build_word_freq(texts_all, top_n = top_n, min_freq = min_freq)
wf_unf$panel <- "Unfiltered"
plot_dfs[["Unfiltered"]] <- wf_unf

# ---- Create individual plots ----
plots <- list()
set.seed(37)
for (nm in names(plot_dfs)) {
  df <- plot_dfs[[nm]]
  p <- ggplot(df, aes(label = word, size = freq, color = I(color))) +
    geom_text_wordcloud(
      shape = "square",
      rm_outside = TRUE,
      angle = 0,
      grid_size = panel_params[[nm]]$grid_size
    ) +
    scale_size_area(max_size = panel_params[[nm]]$max_size) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, face = "bold")
    ) +
    labs(title = nm)
  plots[[nm]] <- p
}

# ---- Combine plots into 2x2 grid ----
(plots[["China Filter"]] + plots[["North Korea Filter"]]) /
  (plots[["Russia Filter"]] + plots[["Unfiltered"]]) +
  plot_layout(heights = c(1, 1), guides = "collect") &
  theme(plot.margin = margin(2, 2, 2, 2))  # top, right, bottom, left
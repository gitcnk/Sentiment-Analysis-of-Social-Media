# Load required libraries
library(tm)
library(ggwordcloud)
library(ggplot2)
library(RColorBrewer)

# ---- User settings ----
cluster_id <- 6        # <-- set cluster ID here
regex_filter <- ""     # <-- keywords to remove, "|" separated
top_n <- 10            # <-- number of top words to highlight
min_freq <- 2          # <-- minimum frequency to include in cloud

# File name
file_name <- paste0("HC_", cluster_id, "_text.csv")

# ---- Read data ----
data <- read.delim(file_name, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
texts <- data[[3]]

# ---- Apply regex filter to remove specific words ----
if (regex_filter != "") {
  filtered_regex <- paste0("\\b(", regex_filter, ")\\b")
  texts <- gsub(filtered_regex, "", texts, ignore.case = TRUE)
}

# ---- Build corpus ----
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# ---- Term frequencies ----
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
freq <- sort(rowSums(m), decreasing = TRUE)
freq <- freq[freq >= min_freq]
word_freq <- data.frame(word = names(freq), freq = freq, stringsAsFactors = FALSE)

# ---- Highlight top words ----
pal <- brewer.pal(8, "Set2")
word_freq$color <- "grey70"
if (nrow(word_freq) > 0 && top_n > 0) {
  n_highlight <- min(top_n, nrow(word_freq))
  top_colors <- pal[rep(seq_along(pal), length.out = n_highlight)]
  word_freq$color[seq_len(n_highlight)] <- top_colors
}

# ---- Plot word cloud (compact, rectangular, no overlap) ----
set.seed(37)
ggplot(word_freq, aes(label = word, size = freq, color = color)) +
  geom_text_wordcloud(
    shape = "square", 
    grid_size = 1,       # smaller = more compact
    rm_outside = TRUE,   # remove words outside plotting area
    angle = 0            # keep horizontal for readability
  ) +
  scale_size_area(max_size = 1) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )

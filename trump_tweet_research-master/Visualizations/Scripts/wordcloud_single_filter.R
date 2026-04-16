# Load required libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

# ---- User settings ----
cluster_id   <- 8                        # change as needed
regex_filter <- "China|Beijing|Xi"           # keywords (| separated)
top_n        <- 10                       # number of top words to highlight
min_freq     <- 2                        # minimum freq to show in cloud (adjust)

# File (tab-separated; 3rd column contains the text)
file_name <- paste0("HC_", cluster_id, "_text.csv")

# ---- Read data ----
data <- read.delim(file_name, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
texts_all <- data[[3]]

# ---- Prepare texts for left panel: select rows that contain the regex and remove the matched words ----
rows_with_keyword <- grepl(regex_filter, texts_all, ignore.case = TRUE)
texts_left_raw <- texts_all[rows_with_keyword]

# remove matched keywords as whole words (use word boundaries)
if (length(texts_left_raw) > 0) {
  regex_word <- paste0("\\b(", regex_filter, ")\\b")
  texts_left_clean <- gsub(regex_word, "", texts_left_raw, ignore.case = TRUE)
} else {
  texts_left_clean <- character(0)
}

# ---- Helper: clean texts and compute word frequencies and color mapping ----
build_word_freq <- function(text_vec, top_n = 10, remove_digits = TRUE, min_freq = 1, pal_name = "Set2") {
  if (length(text_vec) == 0) return(data.frame(word = character(0), freq = numeric(0), color = character(0), stringsAsFactors = FALSE))
  
  # Corpus & cleaning
  corpus <- Corpus(VectorSource(text_vec))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  if (remove_digits) corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Term frequencies
  dtm <- TermDocumentMatrix(corpus)
  m <- as.matrix(dtm)
  freq <- sort(rowSums(m), decreasing = TRUE)
  freq <- freq[freq >= min_freq]   # apply min frequency threshold
  if (length(freq) == 0) return(data.frame(word = character(0), freq = numeric(0), color = character(0), stringsAsFactors = FALSE))
  
  wf <- data.frame(word = names(freq), freq = as.numeric(freq), stringsAsFactors = FALSE)
  
  # Colors: default grey, highlight top_n
  pal <- brewer.pal(8, pal_name)
  wf$color <- rep("grey70", nrow(wf))
  n_highlight <- min(top_n, nrow(wf))
  if (n_highlight > 0) {
    highlight_cols <- pal[rep(seq_along(pal), length.out = n_highlight)]
    wf$color[seq_len(n_highlight)] <- highlight_cols
  }
  
  return(wf)
}

# ---- Build word frequency tables for left (filtered) and right (unfiltered) panels ----
wf_left  <- build_word_freq(texts_left_clean, top_n = top_n, min_freq = min_freq)
wf_right <- build_word_freq(texts_all,       top_n = top_n, min_freq = min_freq)

# ---- Two-panel plot: left = filtered, right = unfiltered ----
# Use base plotting layout because wordcloud() draws in base device
old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(0, 0, 1.5, 0))   # small margins, room for title

set.seed(37)  # reproducible placement

# Left panel: filtered (only texts containing regex; regex terms removed)
if (nrow(wf_left) > 0) {
  wordcloud(
    words = wf_left$word,
    freq = wf_left$freq,
    min.freq = min_freq,
    random.order = FALSE,
    rot.per = 0.15,
    colors = wf_left$color,
    scale = c(3, 0.5),
    ordered.colors = TRUE,
    use.r.layout = FALSE
  )
  title(main = "Filtered", cex.main = 0.7)
} else {
  plot.new()
  title(main = paste0("Filtered: no texts match [", regex_filter, "]"), cex.main = 1)
}

# Right panel: unfiltered (all texts; regex not removed)
if (nrow(wf_right) > 0) {
  wordcloud(
    words = wf_right$word,
    freq = wf_right$freq,
    min.freq = min_freq,
    random.order = FALSE,
    rot.per = 0.15,
    colors = wf_right$color,
    scale = c(2, 0.5),
    ordered.colors = TRUE,
    use.r.layout = FALSE
  )
  title(main = "Unfiltered", cex.main = 0.7)
} else {
  plot.new()
  title(main = "Unfiltered: no words to display", cex.main = 0.7)
}

# restore original graphic parameters
par(old_par)

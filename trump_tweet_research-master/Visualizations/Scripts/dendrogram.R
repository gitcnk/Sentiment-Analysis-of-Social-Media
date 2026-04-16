library(dendextend)
library(RColorBrewer)

X <- as.matrix(read.csv("embeddings_pca.csv", header = FALSE))

clusters <- X |> dist() |> hclust(method = "ward.D2")

k <- 8
membership <- cutree(clusters, k = k)

cols <- brewer.pal(k, "Set2")

# --- Build dendrogram ---
dend <- as.dendrogram(clusters)
dend <- set(dend, "branches_k_color", k = k, value = cols)
dend <- set(dend, "labels", NULL)
dend <- set(dend, "branches_lwd", 2)

# --- Plot dendrogram ---
op <- par(no.readonly = TRUE)
par(mar = c(6, 4, 4, 2) + 0.1)
plot(dend, axes = FALSE)

# --- Leaf order (needed to map membership to positions) ---
ord <- order.dendrogram(dend)

# Extract branch colors from the colored dendrogram (in leaf order)
branch_cols <- get_leaves_branches_col(dend)

# Map branch colors to clusters (take unique color per cluster)
cluster_colors <- tapply(branch_cols, membership[ord], unique)

# Cluster sizes
cluster_sizes <- table(membership)

# Baseline for brackets
usr <- par("usr")
y_base <- usr[3] + 0.02 * diff(usr[3:4])

for (cl in 1:k) {
  # Leaf positions for this cluster (in plotting order 1..n)
  leaf_pos <- which(membership[ord] == cl)
  x1 <- min(leaf_pos)
  x2 <- max(leaf_pos)
  
  # Bracket depth
  y1 <- y_base
  y2 <- y_base - 0.02 * diff(usr[3:4])
  
  # Draw bracket using the extracted cluster color
  lines(c(x1, x1, x2, x2), c(y1, y2, y2, y1),
        col = cluster_colors[cl], lwd = 2, xpd = NA)
  
  # Add cluster size label using the extracted cluster color
  text(mean(c(x1, x2)), y2 - 0.025 * diff(usr[3:4]),
       labels = cluster_sizes[cl], col = "#333333", font = 2, xpd = NA)
}

legend("topright", 
       legend = paste("Cluster", 1:k), 
       col = cols,
       pch = 19,
       pt.cex = 1.2,
       title.col = "#333333",  # Dark gray title color
       bty = "n",              # Box around legend
       cex = 0.9,              # Slightly larger text
       text.font = 2,          # Bold text
       y.intersp = 1.4,        # Increased spacing between legend items
       inset = c(0.03, 0),
       xpd = NA)               # Allow legend outside plot area

par(op)

mapping <- c(
  "1" = 7,
  "2" = 8,
  "3" = 4,
  "4" = 1,
  "5" = 5,
  "6" = 6,
  "7" = 2,
  "8" = 3
)
pyclusters <- sapply(membership, function(x) mapping[as.character(x)])
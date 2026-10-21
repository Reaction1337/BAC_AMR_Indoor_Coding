# ============================================================
# Lower-triangle Spearman correlation heatmap
# Show Spearman rho and p value
# ============================================================

library(readxl)
library(ggplot2)


# ============================================================
# 1. Read Excel data
# ============================================================



# ============================================================
# 2. Select four variables
# ============================================================



# ============================================================
# 3. Calculate Spearman correlation matrix
# ============================================================

corr_matrix <- cor(
  corr_data,
  method = "spearman",
  use = "pairwise.complete.obs"
)

print("Spearman correlation matrix:")
print(corr_matrix)


# ============================================================
# 4. Calculate p-value matrix
# ============================================================

n <- ncol(corr_data)

p_matrix <- matrix(
  NA,
  nrow = n,
  ncol = n
)

colnames(p_matrix) <- variable_order
rownames(p_matrix) <- variable_order


for (i in 1:n) {
  
  for (j in 1:n) {
    
    # Only calculate for off-diagonal cells
    if (i != j) {
      
      # Keep only complete observations for the two variables
      complete_cases <- complete.cases(
        corr_data[, c(i, j)]
      )
      
      x <- corr_data[complete_cases, i]
      y <- corr_data[complete_cases, j]
      
      # Spearman correlation test
      test <- cor.test(
        x,
        y,
        method = "spearman",
        exact = FALSE
      )
      
      p_matrix[i, j] <- test$p.value
    }
  }
}


print("P-value matrix:")
print(p_matrix)


# ============================================================
# 5. Create lower-triangle data
# ============================================================

plot_data <- expand.grid(
  row = 1:n,
  col = 1:n
)

# Add correlation values
plot_data$rho <- as.vector(corr_matrix)

# Add p values
plot_data$p_value <- as.vector(p_matrix)

# Keep LOWER triangle
plot_data <- plot_data[
  plot_data$row > plot_data$col,
]


# ------------------------------------------------------------
# IMPORTANT:
# Reverse y-axis position so that row 1 is at the TOP
# This makes row > col appear in the LOWER triangle
# ------------------------------------------------------------

plot_data$y <- n - plot_data$row + 1
plot_data$x <- plot_data$col


# ============================================================
# 6. Create labels
# ============================================================

plot_data$label <- paste0(
  "ρ = ",
  sprintf("%.2f", plot_data$rho),
  "\n",
  "p = ",
  ifelse(
    plot_data$p_value < 0.001,
    "< 0.001",
    sprintf("%.3f", plot_data$p_value)
  )
)


# ============================================================
# 7. Add diagonal cells
# ============================================================

diag_data <- data.frame(
  x = 1:n,
  y = n:1
)


# ============================================================
# 8. Plot
# ============================================================

p <- ggplot() +
  
  # ----------------------------------------------------------
# Lower triangle
# ----------------------------------------------------------

geom_tile(
  data = plot_data,
  aes(
    x = x,
    y = y,
    fill = rho
  ),
  width = 1,
  height = 1,
  color = "white",
  linewidth = 0.8
) +
  
  
  # ----------------------------------------------------------
# Correlation coefficients + p values
# ----------------------------------------------------------

geom_text(
  data = plot_data,
  aes(
    x = x,
    y = y,
    label = label
  ),
  size = 3.5,
  color = "black",
  lineheight = 0.9
) +
  
  
  # ----------------------------------------------------------
# Diagonal
# ----------------------------------------------------------

geom_tile(
  data = diag_data,
  aes(
    x = x,
    y = y
  ),
  fill = "grey92",
  color = "white",
  linewidth = 0.8
) +
  
  
  # ----------------------------------------------------------
# Color scale
# ----------------------------------------------------------

scale_fill_gradient2(
  low = "#FFF3E0",
  mid = "white",
  high = "#E65100",
  midpoint = 0,
  limits = c(-1, 1),
  name = "Spearman \u03c1"
) +
  
  
  # ----------------------------------------------------------
# X axis
# ----------------------------------------------------------

scale_x_continuous(
  breaks = 1:4,
  labels = c(
    "BAC-product\nrichness",
    "BAC concentration\nin dust",
    "MIC BAC\nvalue",
    "Efflux-related genes\nrichness"
  ),
  limits = c(0.5, 4.5),
  expand = c(0, 0)
) +
  
  
  # ----------------------------------------------------------
# Y axis
# ----------------------------------------------------------

scale_y_continuous(
  breaks = 4:1,
  labels = c(
    "BAC-product\nrichness",
    "BAC concentration\nin dust",
    "MIC BAC\nvalue",
    "Efflux-related genes\nrichness"
  ),
  limits = c(0.5, 4.5),
  expand = c(0, 0)
) +
  
  
  # ----------------------------------------------------------
# Square cells
# ----------------------------------------------------------

coord_fixed() +
  
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_classic(
  base_size = 12,
  base_family = "sans"
) +
  
  theme(
    
    axis.title = element_blank(),
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      color = "black",
      size = 10
    ),
    
    axis.text.y = element_text(
      color = "black",
      size = 10
    ),
    
    axis.ticks = element_blank(),
    
    legend.title = element_text(
      size = 10,
      color = "black"
    ),
    
    legend.text = element_text(
      size = 9,
      color = "black"
    )
  )


# ============================================================
# 9. Display
# ============================================================

print(p)


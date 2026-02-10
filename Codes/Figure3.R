# Figure 3
# Correlation between total BAC concentration in dust
# and antimicrobial tolerance (MIC)
#
# Analysis:
#   - MIC values are log2-transformed
#   - Pearson correlation analysis
#   - Scatter plot with color-coded MIC values
#
# Requirements:
#   - R (>= 4.2.0 recommended)
#   - Packages: readxl, ggplot2, dplyr, scales
#
# Input data:
#   Excel file containing columns:
#   "Total" (total BAC concentration, µg/g)
#   "MIC"   (minimum inhibitory concentration)
#
# Note:
#   Set the relative path to the data file before running.
############################################################

# ==============================
# Load required libraries
# ==============================
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

# ==============================
# User-defined file path
# ==============================
# Relative path to the input data file
data_file <- "data/BAC_conc_MIC.xlsx"

# ==============================
# Read and preprocess data
# ==============================
df <- read_excel(data_file)

# Log2 transformation of MIC values
df <- df %>%
  mutate(MIC_log2 = log2(MIC))

# ==============================
# Pearson correlation analysis
# ==============================
pearson_res <- cor.test(df$Total, df$MIC_log2, method = "pearson")

r_val <- round(as.numeric(pearson_res$estimate), 3)
p_val <- signif(pearson_res$p.value, 3)

cat("Pearson correlation analysis\n")
cat("r =", r_val, "\n")
cat("p-value =", p_val, "\n\n")

# ==============================
# Determine plotting ranges
# ==============================
x_min <- min(df$Total, na.rm = TRUE)
x_max <- max(df$Total, na.rm = TRUE)
y_min <- min(df$MIC_log2, na.rm = TRUE)
y_max <- max(df$MIC_log2, na.rm = TRUE)

# ==============================
# Generate scatter plot
# ==============================
fig3 <- ggplot(df, aes(x = Total, y = MIC_log2, color = MIC_log2)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_gradient(
    low = "pink",
    high = "red",
    name = "MIC (log2)"
  ) +
  scale_x_continuous(
    limits = c(x_min, x_max),
    breaks = pretty_breaks(n = 5)
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    x = expression(BAC~Concentration~in~Dust~(mu*g/g)),
    y = expression(log[2]~MIC)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(fig3)

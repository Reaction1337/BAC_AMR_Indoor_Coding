############################################################
# Supplementary Figures 1 & 2
# LC–MS analysis of benzalkonium chloride (BAC) homologs
#
# Figure 1:
# Stacked bar chart showing total BAC concentrations
# across 24 samples (C12, C14, C16).
#
# Figure 2:
# Distribution of BAC homolog concentrations using
# custom boxplot-style visualization (10th–90th percentiles).
#
# Requirements:
#   - R (>= 4.2.0 recommended)
#   - Packages: tidyverse, readxl
#
# Input data:
#   Excel file containing columns:
#   "Sample ID", "C12", "C14", "C16"
#
# Note:
#   Set the relative path to the data file before running.
############################################################

# ==============================
# Load required libraries
# ==============================
library(tidyverse)
library(readxl)

# ==============================
# User-defined file path
# ==============================
# Set the relative path to the LC–MS data file
data_file <- "data/LCMS_BAC_data.xlsx"

# ==============================
# Read and preprocess data
# ==============================
bac_data <- read_excel(data_file) %>%
  rename(Sample = `Sample ID`)

# ==========================================================
# Figure 1: Stacked bar chart of total BAC concentration
# ==========================================================

# Calculate total BAC concentration and sort samples
total_bac <- bac_data %>%
  mutate(Total = C12 + C14 + C16) %>%
  arrange(desc(Total)) %>%
  select(Sample, Total)

# Convert to long format and enforce sample order
bac_long <- bac_data %>%
  pivot_longer(
    cols = c(C12, C14, C16),
    names_to = "Chain",
    values_to = "Concentration"
  ) %>%
  mutate(Sample = factor(Sample, levels = total_bac$Sample))

# Calculate stacking positions (C12 at bottom)
bac_stacked <- bac_long %>%
  group_by(Sample) %>%
  arrange(factor(Chain, levels = c("C12", "C14", "C16"))) %>%
  mutate(
    ymin = cumsum(lag(Concentration, default = 0)),
    ymax = ymin + Concentration
  ) %>%
  ungroup()

# Plot Figure 1
fig1 <- ggplot(bac_stacked, aes(x = Sample)) +
  geom_rect(aes(
    ymin = ymin,
    ymax = ymax,
    xmin = as.numeric(Sample) - 0.4,
    xmax = as.numeric(Sample) + 0.4,
    fill = Chain
  )) +
  scale_fill_manual(
    values = c(
      "C12" = "#08306B",
      "C14" = "#4292C6",
      "C16" = "#DEEBF7"
    ),
    name = "BAC homolog"
  ) +
  labs(
    x = "Sample ID",
    y = expression(BAC~Concentration~(mu*g/g))
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(fig1)

# ==========================================================
# Figure 2: Distribution of BAC homolog concentrations
# ==========================================================

# Convert to long format
bac_long_dist <- bac_data %>%
  pivot_longer(
    cols = c(C12, C14, C16),
    names_to = "Chain",
    values_to = "Concentration"
  )

# Calculate summary statistics
stats <- bac_long_dist %>%
  group_by(Chain) %>%
  summarise(
    p10 = quantile(Concentration, 0.10, na.rm = TRUE),
    p25 = quantile(Concentration, 0.25, na.rm = TRUE),
    median = median(Concentration, na.rm = TRUE),
    p75 = quantile(Concentration, 0.75, na.rm = TRUE),
    p90 = quantile(Concentration, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# Plot Figure 2
fig2 <- ggplot() +
  # Interquartile range (25th–75th)
  geom_rect(
    data = stats,
    aes(
      xmin = as.numeric(factor(Chain)) - 0.3,
      xmax = as.numeric(factor(Chain)) + 0.3,
      ymin = p25,
      ymax = p75,
      fill = Chain
    ),
    alpha = 0.5
  ) +
  # Median line
  geom_segment(
    data = stats,
    aes(
      x = as.numeric(factor(Chain)) - 0.3,
      xend = as.numeric(factor(Chain)) + 0.3,
      y = median,
      yend = median
    ),
    color = "black",
    linewidth = 1
  ) +
  # Whiskers (10th–90th percentiles)
  geom_errorbar(
    data = stats,
    aes(
      x = as.numeric(factor(Chain)),
      ymin = p10,
      ymax = p90
    ),
    width = 0.1,
    color = "black"
  ) +
  # Individual data points
  geom_jitter(
    data = bac_long_dist,
    aes(
      x = as.numeric(factor(Chain)),
      y = Concentration
    ),
    width = 0.15,
    color = "black",
    size = 2,
    alpha = 0.8
  ) +
  scale_x_continuous(
    breaks = 1:3,
    labels = c("C12", "C14", "C16")
  ) +
  scale_fill_manual(
    values = c(
      "C12" = "#08306B",
      "C14" = "#4292C6",
      "C16" = "#DEEBF7"
    )
  ) +
  labs(
    x = "BAC homologs",
    y = expression(BAC~Concentration~(mu*g/g))
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

print(fig2)

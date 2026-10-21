# ============================================================
# stacked bar plot
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

# ============================================================
# 1. Read Excel
# ============================================================



# ============================================================
# 2. Convert to long format
# ============================================================



# ============================================================
# 3. Preserve original sample order
# ============================================================

df_long$Sample <- factor(
  df_long$Sample,
  levels = unique(df_long$Sample)
)


# ============================================================
# 4. Calculate percentage within each sample
# ============================================================

df_long <- df_long %>%
  group_by(Sample) %>%
  mutate(
    Percentage = Count / sum(Count) * 100
  ) %>%
  ungroup()


# ============================================================
# 5. Set resistance mechanism order
#
# Efflux Pumps = bottom of stacked bar
# ============================================================

df_long$`Resistance Mechanism (by Type)` <- factor(
  df_long$`Resistance Mechanism (by Type)`,
  levels = c(
    "Target replacement",
    "Reduced permeability",
    "Inactivation",
    "Target alteration",
    "Efflux Pumps"
  )
)


# ============================================================
# 6. Create percentage labels for Efflux Pumps only
# ============================================================

df_long <- df_long %>%
  mutate(
    Efflux_Label = ifelse(
      `Resistance Mechanism (by Type)` == "Efflux Pumps",
      paste0(
        sprintf("%.1f", Percentage),
        "%"
      ),
      ""
    )
  )


# ============================================================
# 7. Format bacteria names
#
# Replace "_" with a normal space.
#
# Example:
# Escherichia_coli
# ->
# Escherichia coli
# ============================================================

sample_labels <- levels(df_long$Sample)

sample_labels_formatted <- gsub(
  "_",
  " ",
  sample_labels
)

names(sample_labels_formatted) <- sample_labels


# ============================================================
# 8. EST / ACS-inspired color palette
# ============================================================

mechanism_colors <- c(
  "Target replacement"   = "#4B5563",
  "Reduced permeability" = "#6B7280",
  "Inactivation"         = "#374151",
  "Target alteration"    = "#1F2937",
  "Efflux Pumps"         = "#7A1519"
)


# ============================================================
# 9. Create plot
# ============================================================

p <- ggplot(
  df_long,
  aes(
    x = Sample,
    y = Percentage,
    fill = `Resistance Mechanism (by Type)`
  )
) +
  
  
  # ==========================================================
# 9.1 Stacked bars
# ==========================================================

geom_col(
  position = "stack",
  width = 0.72,
  color = "white",
  linewidth = 0.25
) +
  
  
  # ==========================================================
# 9.2 Percentage labels for Efflux Pumps only
# ==========================================================

geom_text(
  data = df_long %>%
    filter(
      `Resistance Mechanism (by Type)` == "Efflux Pumps"
    ),
  
  aes(
    label = Efflux_Label,
    y = Percentage / 2
  ),
  
  color = "white",
  fontface = "bold",
  size = 3.8,
  family = "sans"
) +
  
  
  # ==========================================================
# 9.3 Manual color scale
# ==========================================================

scale_fill_manual(
  values = mechanism_colors,
  drop = FALSE
) +
  
  
  # ==========================================================
# 9.4 X-axis
#
# No wrapping
# No rotation
# ==========================================================

scale_x_discrete(
  labels = sample_labels_formatted,
  drop = FALSE
) +
  
  
  # ==========================================================
# 9.5 Y-axis
# ==========================================================

scale_y_continuous(
  breaks = seq(0, 100, 20),
  labels = NULL,
  expand = c(0, 0)
) +
  
  
  # ==========================================================
# 9.6 Labels
# ==========================================================

labs(
  x = NULL,
  y = NULL,
  fill = "Resistance mechanism\n(Relative contribution)"
) +
  
  
  # ==========================================================
# 9.7 Clean publication-style theme
# ==========================================================

# ============================================================
# 9.7 Clean publication-style theme
# ============================================================

theme_classic(
  base_size = 13
) +
  
  theme(
    
    # --------------------------------------------------------
    # X-axis bacteria names
    #
    # angle = 90 -> vertical labels
    # hjust = 1   -> align labels close to axis
    # face = italic -> scientific names italicized
    # --------------------------------------------------------
    
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = "black",
      size = 9.5,
      face = "italic",
      lineheight = 1
    ),
    
    # --------------------------------------------------------
    # Remove Y-axis text and ticks
    # --------------------------------------------------------
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    # --------------------------------------------------------
    # Clean black frame
    # --------------------------------------------------------
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    
    axis.line = element_blank(),
    
    # --------------------------------------------------------
    # Legend
    # --------------------------------------------------------
    
    legend.title = element_text(
      face = "bold",
      color = "black",
      size = 11
    ),
    
    legend.text = element_text(
      color = "black",
      size = 10.5
    ),
    
    legend.key.size = unit(
      0.55,
      "cm"
    ),
    
    legend.spacing.y = unit(
      0.08,
      "cm"
    ),
    
    legend.background = element_blank(),
    
    # --------------------------------------------------------
    # Background
    # --------------------------------------------------------
    
    panel.background = element_blank(),
    
    plot.background = element_blank(),
    
    # --------------------------------------------------------
    # Plot margins
    #
    # Increase bottom margin because labels are vertical
    # --------------------------------------------------------
    
    plot.margin = margin(
      8,
      10,
      25,
      8
    )
  )
# ============================================================
# 10. Display
# ============================================================

print(p)


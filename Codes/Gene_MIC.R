library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

# =========================================================
# File paths
# =========================================================





# =========================================================
# Isolation order
# =========================================================
isolation_order <- mic_data %>%
  arrange(MIC) %>%
  pull(Isolation)


# =========================================================
# Gene groups
# =========================================================
qac_genes <- c(
  "qacB",
  "qacG",
  "qacJ"
)

other_genes <- setdiff(
  unique(gene_long$Resistance_Gene),
  qac_genes
)

# Other genes first, QAC genes second
gene_order <- c(
  other_genes,
  qac_genes
)


# =========================================================
# Prepare ARG data
# =========================================================
gene_plot <- gene_long %>%
  mutate(
    Isolation = factor(
      Isolation,
      levels = isolation_order
    )
  ) %>%
  complete(
    Resistance_Gene,
    Isolation
  ) %>%
  mutate(
    
    # Missing genes = non-exist
    Status = ifelse(
      is.na(Status),
      "non-exist",
      Status
    ),
    
    # Gene group
    Gene_Group = ifelse(
      Resistance_Gene %in% qac_genes,
      "A",
      "B"
    ),
    
    # B on top, A on bottom
    Gene_Group = factor(
      Gene_Group,
      levels = c("B", "A")
    ),
    
    # Gene order
    Resistance_Gene = factor(
      Resistance_Gene,
      levels = rev(gene_order)
    )
  )


# =========================================================
# Colors
# =========================================================

# MIC
col_mic <- "#222222"

# Heatmap background
col_cell   <- "#FFFFFF"
col_grid   <- "#E6E6E6"
col_border <- "#222222"

# ARG status
col_nonexist <- "#2166A5"  # blue outline
col_exist    <- "#2166A5"  # blue filled
col_MA       <- "#FFD92F"  # Light oral
col_MC       <- "#B2182B"  # Dark red 


# =========================================================
# ARG heatmap
# =========================================================
p_gene <- ggplot(
  gene_plot,
  aes(
    x = Isolation,
    y = Resistance_Gene,
    color = Status,
    shape = Status,
    fill = Status
  )
) +
  
  # -------------------------------------------------------
# White heatmap cells
# -------------------------------------------------------
geom_tile(
  aes(
    x = Isolation,
    y = Resistance_Gene
  ),
  width = 0.94,
  height = 0.94,
  fill = col_cell,
  color = col_grid,
  linewidth = 0.25,
  inherit.aes = FALSE
) +
  
  # -------------------------------------------------------
# Status markers
# Shape 21 allows separate outline and fill control
# -------------------------------------------------------
geom_point(
  size = 3.1,
  stroke = 0.9
) +
  
  # -------------------------------------------------------
# Colors: outline colors
# -------------------------------------------------------
scale_color_manual(
  values = c(
    "non-exist" = col_nonexist,
    "exist"     = col_exist,
    "exist-ma"  = col_MA,
    "exist-mc"  = col_MC
  ),
  breaks = c(
    "non-exist",
    "exist",
    "exist-ma",
    "exist-mc"
  ),
  labels = c(
    "Not-detected",
    "Detected",
    "detected, MGE-associated",
    "detected, MGE-carried"
  )
) +
  
  # -------------------------------------------------------
# Shapes: all circles
# -------------------------------------------------------
scale_shape_manual(
  values = c(
    "non-exist" = 21,
    "exist"     = 21,
    "exist-ma"  = 21,
    "exist-mc"  = 21
  ),
  breaks = c(
    "non-exist",
    "exist",
    "exist-ma",
    "exist-mc"
  ),
  labels = c(
    "Not-detected",
    "Detected",
    "detected, MGE-associated",
    "detected, MGE-carried"
  )
) +
  
  # -------------------------------------------------------
# Fill colors
# Not-detected = white fill + yellow outline
# Other statuses = solid fill
# -------------------------------------------------------
scale_fill_manual(
  values = c(
    "non-exist" = "#FFFFFF",
    "exist"     = col_exist,
    "exist-ma"  = col_MA,
    "exist-mc"  = col_MC
  ),
  breaks = c(
    "non-exist",
    "exist",
    "exist-ma",
    "exist-mc"
  ),
  labels = c(
    "Not-detected",
    "Detected",
    "detected, MGE-associated",
    "detected, MGE-carried"
  )
) +
  
  # -------------------------------------------------------
# X scale
# -------------------------------------------------------
scale_x_discrete(
  expand = expansion(
    add = c(0.25, 0.25)
  )
) +
  
  # -------------------------------------------------------
# Y scale
# -------------------------------------------------------
scale_y_discrete(
  expand = expansion(
    add = c(0.25, 0.25)
  )
) +
  
  # -------------------------------------------------------
# A / B group labels
# -------------------------------------------------------
facet_grid(
  rows = vars(Gene_Group),
  scales = "free_y",
  space = "free_y",
  switch = "y",
  labeller = as_labeller(
    c(
      B = "efflux-related\nresistance genes",
      A = "qac-associated\nresistance genes"
    )
  )
) +
  
  # -------------------------------------------------------
# Legend labels
# -------------------------------------------------------
labs(
  x = NULL,
  y = "ARG (curated CARD short name)",
  color = NULL,
  shape = NULL,
  fill = NULL
) +
  
  # -------------------------------------------------------
# Legend: one combined color legend
# Not-detected is explicitly hollow
# -------------------------------------------------------
guides(
  color = "none",
  shape = "none",
  fill = guide_legend(
    order = 1,
    override.aes = list(
      shape = c(21, 21, 21, 21),
      colour = c(
        col_nonexist,
        col_exist,
        col_MA,
        col_MC
      ),
      fill = c(
        "#FFFFFF",
        col_exist,
        col_MA,
        col_MC
      ),
      size = 4,
      stroke = 1.2
    )
  )
) +
  
  theme_classic() +
  
  theme(
    
    # =====================================================
    # X axis
    # =====================================================
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # =====================================================
    # Gene names
    # =====================================================
    axis.text.y = element_text(
      size = 8.3,
      color = "black",
      margin = margin(r = 6)
    ),
    
    # =====================================================
    # Overall y-axis title
    # =====================================================
    axis.title.y = element_text(
      size = 10,
      face = "bold",
      margin = margin(r = 10)
    ),
    
    # =====================================================
    # A / B group titles
    # =====================================================
    strip.placement = "outside",
    
    strip.background = element_blank(),
    
    strip.text.y.left = element_text(
      angle = 0,
      size = 8.5,
      face = "bold",
      color = "black",
      lineheight = 1.05
    ),
    
    # =====================================================
    # No default grid
    # =====================================================
    panel.grid = element_blank(),
    
    # =====================================================
    # Panel border
    # =====================================================
    panel.border = element_rect(
      color = col_border,
      fill = NA,
      linewidth = 0.6
    ),
    
    # =====================================================
    # Space between A and B
    # =====================================================
    panel.spacing.y = unit(
      0.18,
      "lines"
    ),
    
    # =====================================================
    # Legend
    # =====================================================
    legend.position = "right",
    
    legend.text = element_text(
      size = 8.5,
      color = "black"
    ),
    
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    
    # =====================================================
    # Margins
    # =====================================================
    plot.margin = margin(
      t = 3,
      r = 3,
      b = 1,
      l = 3
    )
  )


# =========================================================
# MIC bar chart
# =========================================================
p_mic <- ggplot(
  mic_data,
  aes(
    x = factor(
      Isolation,
      levels = isolation_order
    ),
    y = MIC
  )
) +
  
  # -------------------------------------------------------
# MIC bars
# -------------------------------------------------------
geom_col(
  fill = col_mic,
  width = 0.72
) +
  
  # -------------------------------------------------------
# MIC Y axis: 0–512
# Only 0, 128, 256, 512
# -------------------------------------------------------
scale_y_continuous(
  limits = c(0, 512),
  breaks = c(
    0,
    128,
    256,
    512
  ),
  expand = expansion(
    mult = c(0, 0)
  )
) +
  
  # -------------------------------------------------------
# X axis
# -------------------------------------------------------
scale_x_discrete(
  expand = expansion(
    add = c(0.25, 0.25)
  )
) +
  
  labs(
    x = "Isolates",
    y = "BAC MIC value (μg/mL)"
  ) +
  
  theme_classic() +
  
  theme(
    
    # =====================================================
    # X labels
    # =====================================================
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 7.5,
      color = "black"
    ),
    
    # =====================================================
    # Y labels
    # =====================================================
    axis.text.y = element_text(
      size = 8.5,
      color = "black"
    ),
    
    axis.title.x = element_text(
      size = 10,
      face = "bold"
    ),
    
    axis.title.y = element_text(
      size = 10,
      face = "bold"
    ),
    
    # =====================================================
    # Horizontal grid
    # =====================================================
    panel.grid.major.y = element_line(
      color = "#E6E6E6",
      linewidth = 0.35
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # =====================================================
    # Border
    # =====================================================
    panel.border = element_rect(
      color = col_border,
      fill = NA,
      linewidth = 0.6
    ),
    
    # =====================================================
    # Margins
    # =====================================================
    plot.margin = margin(
      t = 0,
      r = 1,
      b = 2,
      l = 1
    )
  )


# =========================================================
# Combine
# =========================================================
final_plot <- p_gene /
  plot_spacer() /
  p_mic +
  
  plot_layout(
    heights = c(
      5,
      0.65,
      2.8
    )
  )


# =========================================================
# Display plot
# =========================================================
final_plot



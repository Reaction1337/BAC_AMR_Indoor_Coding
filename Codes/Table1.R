
# Table 1
# Identification and phenotypic characteristics of
# selected BAC-tolerant bacterial isolates
#
# Table content:
#   - Sample ID
#   - MIC value
#   - Colony morphology (visual inspection)
#   - BLASTn-based taxonomic assignment
#   - NCBI taxonomic ID
#   - Sequence coverage and identity
#
# Output:
#   - HTML-formatted table suitable for direct inclusion
#     in Supplementary Materials or copying into Word
#
# Requirements:
#   - R (>= 4.2.0 recommended)
#   - Packages: knitr, kableExtra
############################################################

# ==============================
# Load required libraries
# ==============================
library(knitr)
library(kableExtra)

# ==============================
# Construct table data
# ==============================
# Note:
# E-values are omitted as all reported matches were significant.
df <- data.frame(
  "Sample ID" = c("27.1", "409.1", "376.C", "207.1", "441.1"),
  "MIC value (µg/mL)" = c(128, 128, 4, 128, 256),
  "Colony morphology" = c(
    "Circular, entire margin, convex, medium-sized, smooth, shiny, pigmented",
    "Circular, entire margin, convex, small to medium-sized, smooth, shiny, pigmented",
    "Circular, entire margin, convex, small-sized, smooth, shiny, pigmented",
    "Irregular to circular, undulate margin, raised, large-sized, rough, dull, non-pigmented",
    "Circular, entire margin, convex, medium-sized, smooth, shiny, non-pigmented"
  ),
  "BLASTn result" = c(
    "<i>Pseudomonas lutea</i>",
    "<i>Pseudomonas coleopterorum</i>",
    "<i>Micrococcus aloeverae</i>",
    "<i>Bacillus amyloliquefaciens</i>",
    "<i>Enterobacter quasihormaechei</i>"
  ),
  "Taxonomic ID" = c(
    "NCBI:txid243924",
    "NCBI:txid1605838",
    "NCBI:txid1391911",
    "NCBI:txid1390",
    "NCBI:txid2529382"
  ),
  "Coverage / Identity" = c(
    "92% / 93.15%",
    "95% / 96.57%",
    "95% / 97.67%",
    "92% / 98.52%",
    "98% / 95.36%"
  ),
  check.names = FALSE
)

# ==============================
# Generate HTML-formatted table
# ==============================
# The table can be viewed in RStudio Viewer and
# copied directly into a manuscript or supplement.
kable(
  df,
  format = "html",
  escape = FALSE,
  caption = "Supplementary Table 1. Identification and phenotypic characteristics of selected BAC-tolerant isolates."
) %>%
  kable_styling(
    full_width = FALSE,
    position = "left",
    font_size = 12
  ) %>%
  # Bold header with thick bottom rule
  row_spec(
    row = 0,
    bold = TRUE,
    extra_css = "border-bottom:3px solid black;"
  ) %>%
  # Thick bottom rule for table
  row_spec(
    row = nrow(df),
    extra_css = "border-bottom:3px solid black;"
  ) %>%
  # Column width and emphasis adjustments
  column_spec(1, width = "80px", bold = TRUE) %>%    # Sample ID
  column_spec(2, width = "120px", bold = TRUE,
              extra_css = "text-align:center;") %>% # MIC
  column_spec(3, width = "360px") %>%                # Colony morphology
  column_spec(4, width = "190px") %>%                # BLASTn result
  column_spec(5, width = "150px") %>%                # Taxonomic ID
  column_spec(6, width = "150px")                    # Coverage / Identity

# ============================================================================== #
# PLOT MODEL PERFORMANCE HEATMAPS
# ============================================================================== #
# Display seven model evaluation metrics across species, SPDE meshes and
# likelihood families. Highlight the selected configuration for each species.
# Run 02_prepare_metrics_comparison_table.R before running this script.
# ============================================================================== #

# ------------------------------------------------------------------------------ #
#### 1. SELECT REGION AND LOAD PREPARED METRICS ####
# ------------------------------------------------------------------------------ #

# Select "EAST" or "WEST".
region <- "EAST"

# Load the combined table produced by the metrics preparation script.
metrics_file <- here::here("05_OUTPUTS", "COMBINED_METRICS", region,
                           "combined_metrics_all_species.rds")

combined_metrics_for_plot <- readRDS(metrics_file)

# ------------------------------------------------------------------------------ #
#### 2. DEFINE LIKELIHOOD LABELS ####
# ------------------------------------------------------------------------------ #

# Named labels match the original family codes; the prepared data normally
# already contains their display labels.
family_labs <- c(
  tweedie = "Tweedie",
  deltagamma = "Δ-Gamma",
  deltalognormal = "Δ-Lognormal",
  deltagammapoissonlink = "Δ-Gamma-P",
  deltalognormalpoissonlink = "Δ-Lognormal-P"
)

# ------------------------------------------------------------------------------ #
#### 3. SELECT CONFIGURATIONS TO HIGHLIGHT ####
# ------------------------------------------------------------------------------ #

# Selected model configurations for the EAST region
highlight_cells_east <- tibble::tribble(
  ~species_clean,                  ~mesh, ~family,
  "Chelidonichthys\ncuculus",       "M4",  "Δ-Lognormal",
  "Chelidonichthys\nlucerna",       "M5",  "Δ-Lognormal-P",
  "Conger\nconger",                 "M5",  "Δ-Lognormal",
  "Dicentrarchus\nlabrax",          "M5",  "Tweedie",
  "Hippocampus\nhippocampus",       "M5",  "Δ-Lognormal-P",
  "Solea\nsolea",                   "M5",  "Δ-Lognormal-P",
  "Trachurus\ntrachurus",           "M5",  "Δ-Lognormal-P",
  "Zeus\nfaber",                    "M4",  "Δ-Gamma-P"
)

# Selected model configurations for the WEST region
highlight_cells_west <- tibble::tribble(
  ~species_clean,                  ~mesh, ~family,
  "Capros\naper",                   "M4",  "Δ-Lognormal",
  "Chelidonichthys\ncuculus",       "M3",  "Tweedie",
  "Conger\nconger",                 "M3",  "Δ-Gamma",
  "Dicentrarchus\nlabrax",          "M5",  "Tweedie",
  "Engraulis\nencrasicolus",        "M4",  "Δ-Lognormal-P",
  "Micromesistius\npoutassou",      "M5",  "Δ-Gamma",
  "Zeus\nfaber",                    "M1",  "Tweedie"
)

# Select highlighted models according to the study region
highlight_cells <- switch(region,
                          EAST = highlight_cells_east,
                          WEST = highlight_cells_west)


# ------------------------------------------------------------------------------ #
#### 4. DEFINE HEATMAP FUNCTION ####
# ------------------------------------------------------------------------------ #

# Build one heatmap for the requested metric
make_heatmap <- function(metric_name,
                         show_strip = FALSE,
                         show_x = FALSE,
                         show_y = FALSE) {
  
  # Reverse the colour direction for R² (higher values are better)
  direction <- if (metric_name %in% c("R²")) 1 else -1
  
  # Keep only the metric displayed in this panel
  data <- combined_metrics_for_plot %>%
    dplyr::filter(metric == metric_name)
  
  # Use fixed legend breaks for delta log-likelihood metrics
  breaks <- if (metric_name %in% c("ΔΣLL random CV", "ΔΣLL block CV")) {
    c(0, 25, 50, 75, 100)
  } else {
    ggplot2::waiver()
  }
  labels <- if (metric_name %in% c("ΔΣLL random CV", "ΔΣLL block CV")) {
    c("0", "25", "50", "75", ">99")
    
  } else {
    ggplot2::waiver()
  }
  
  
  # Outline the selected model configuration in each species facet
  ggplot2::ggplot(data, aes(mesh, family, fill = value)) +
    ggplot2::geom_tile(colour = "grey85", linewidth = 0.25) +
    ggplot2::geom_tile(data = highlight_cells, aes(x = mesh, y = family),
                       inherit.aes = FALSE, fill = NA, colour = "black", linewidth = 1) +
    ggfittext::geom_fit_text(aes(label = label), contrast = TRUE, grow = FALSE,
                             min.size = 3) +
    ggplot2::facet_wrap(~ species_clean, nrow = 1) +
    ggplot2::scale_y_discrete(labels = family_labs) +
    scico::scale_fill_scico(
      palette = "vik", 
      direction = direction,
      name = metric_titles[[metric_name]],
      na.value = "white",
      breaks = breaks,
      labels = labels,
      guide = guide_colourbar(direction = "vertical",
                              barwidth  = unit(0.4, "cm"),
                              barheight = unit(1.7, "cm"),
                              title.hjust = 0)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = if (show_x) "SPDE mesh" else NULL,
                  y = if (show_y) "Likelihood" else NULL) +
    ggplot2::theme(
      legend.position = "right",
      legend.title.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.box.just = "left",
      panel.grid = element_blank(),
      strip.background = if (show_strip)
        element_rect(fill = "grey90") else element_blank(),
      strip.text = if (show_strip)
        element_text(face = "italic", size = 12) else element_blank(),
      axis.title.x = if (show_x)
        element_text(margin = margin(t = 12)) else element_blank(),
      axis.title.y = if (show_y)
        element_text(margin = margin(r = 12)) else element_blank(),
      axis.text.x = if (show_x)
        element_text(size = 11) else element_blank(),
      axis.ticks.x = if (show_x)
        element_line() else element_blank(),
      axis.text.y = element_text(size = 11)
    )
}

# Legend titles for each metric
metric_titles <- c("ΔcAIC" = "ΔcAIC",
                   "R²" = "R²",
                   "median coeff var" = "median CV",
                   "ΔΣLL random CV" = "ΔΣLL",
                   "NRMSE random" = "NRMSE",
                   "ΔΣLL block CV" = "ΔΣLL",
                   "NRMSE block" = "NRMSE")


# ------------------------------------------------------------------------------ #
#### 5. CREATE HEATMAP PANELS ####
# ------------------------------------------------------------------------------ #

p1 <- make_heatmap("ΔcAIC", show_y = FALSE)
p2 <- make_heatmap("R²", show_x = TRUE)
p3 <- make_heatmap("median coeff var", show_y = TRUE)
p4 <- make_heatmap("ΔΣLL random CV", show_strip = TRUE)
p5 <- make_heatmap("NRMSE random")
p6 <- make_heatmap("ΔΣLL block CV", show_strip = TRUE)
p7 <- make_heatmap("NRMSE block", show_x = TRUE)


# ------------------------------------------------------------------------------ #
#### 6. DISPLAY COMBINED HEATMAPS ####
# ------------------------------------------------------------------------------ #

# Display model-fit and random cross-validation metrics
(p4 / p5 / p3 / p1 / p2) + patchwork::plot_layout(heights = c(1, 1, 1, 1, 1))

# Display block cross-validation metrics
(p6 / p7 ) + patchwork::plot_layout(heights = c(1, 1))




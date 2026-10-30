# ============================================================
# Fig. 2: Assumed causal structure underlying the two mediation models
# (directed acyclic graphs; no data are read)
#   (A) Model 1: T1 psychological distress (K6 >= 13) as exposure
#   (B) Model 2: childhood adversity (ACE-10 >= 4) as exposure
#
# Requires the dagitty package in addition to ggplot2 and patchwork.
#
# Design principles:
#   1. Keep the same four-variable causal structure in both panels.
#   2. Change only the exposure/conditioning roles between models.
#   3. Keep explanatory prose in the figure legend, not inside the diagrams.
# Output: figures/fig_dag_models.pdf/.png
# ============================================================

suppressPackageStartupMessages({
  library(dagitty)
  library(ggplot2)
  library(patchwork)
})

figdir <- file.path(getwd(), "figures")

font_family <- "sans"
ink <- "#171717"
muted <- "#6A6A6A"
context_ink <- "#9A9A9A"
conditioned_fill <- "#E6E6E6"

# This object is the structural source of truth for both panels.
core_dag <- dagitty(
  "dag {
    ACE -> K6
    ACE -> M
    ACE -> Y
    K6 -> M
    K6 -> Y
    M -> Y
  }"
)

stopifnot(isAcyclic(core_dag))
stopifnot(isTRUE(isAdjustmentSet(
  core_dag,
  Z = "ACE",
  exposure = "K6",
  outcome = "Y"
)))

core_edges <- edges(core_dag)[, c("v", "w")]

# Fixed positions make the chronology immediately visible and keep the two
# panels directly comparable. Curvatures separate longer causal paths.
node_positions <- data.frame(
  name = c("ACE", "K6", "M", "Y"),
  x = c(0.0, 2.1, 4.2, 6.3),
  y = 0
)

edge_style <- data.frame(
  v = c("ACE", "K6", "M", "ACE", "ACE", "K6"),
  w = c("K6", "M", "Y", "M", "Y", "Y"),
  x = c(0.72, 2.82, 4.92, 0.64, 0.62, 2.74),
  y = c(0, 0, 0, 0.10, 0.08, -0.08),
  xend = c(1.10, 3.38, 5.39, 3.40, 5.40, 5.39),
  yend = c(0, 0, 0, 0.10, 0.08, -0.08),
  curvature = c(0, 0, 0, -0.28, -0.32, 0.27)
)

edge_data <- merge(core_edges, edge_style, by = c("v", "w"), sort = FALSE)
stopifnot(nrow(edge_data) == nrow(core_edges))

add_edges <- function(plot, edge_df) {
  for (i in seq_len(nrow(edge_df))) {
    e <- edge_df[i, ]
    arrow <- grid::arrow(angle = 24, length = grid::unit(e$arrow_length, "mm"), type = "closed")
    if (e$curvature == 0) {
      plot <- plot + geom_segment(
        data = e, aes(x = x, y = y, xend = xend, yend = yend),
        linewidth = e$edge_width, lineend = "round", colour = e$edge_colour, arrow = arrow
      )
    } else {
      plot <- plot + geom_curve(
        data = e, aes(x = x, y = y, xend = xend, yend = yend),
        curvature = e$curvature, linewidth = e$edge_width, lineend = "round",
        colour = e$edge_colour, arrow = arrow
      )
    }
  }
  plot
}

make_panel <- function(heading, labels, exposure, conditioned) {
  nodes <- node_positions
  nodes$label <- unname(labels[nodes$name])
  nodes$status <- "other"
  nodes$status[nodes$name == exposure] <- "exposure"
  nodes$status[nodes$name == conditioned] <- "conditioned"

  # Focal direct/mediated paths are thick and dark; the remaining assumed
  # relations are thin and gray.
  focal_keys <- if (exposure == "K6") c("K6->M", "M->Y", "K6->Y") else c("ACE->M", "M->Y", "ACE->Y")
  panel_edges <- edge_data
  panel_edges$is_focal <- paste(panel_edges$v, panel_edges$w, sep = "->") %in% focal_keys
  panel_edges$edge_colour <- ifelse(panel_edges$is_focal, ink, context_ink)
  panel_edges$edge_width <- ifelse(panel_edges$is_focal, 0.88, 0.38)
  panel_edges$arrow_length <- ifelse(panel_edges$is_focal, 2.35, 2.05)
  panel_edges <- panel_edges[order(panel_edges$is_focal), ]
  stopifnot(sum(panel_edges$is_focal) == 3L)

  time_labels <- data.frame(x = node_positions$x, y = 1.02,
                            label = c("Childhood", "T1", "T2", "T3"))

  p <- ggplot() +
    annotate("text", x = -0.72, y = 1.48, label = heading, hjust = 0,
             family = font_family, fontface = "bold", size = 4.05, colour = ink) +
    geom_text(data = time_labels, aes(x = x, y = y, label = label),
              family = font_family, size = 3.05, colour = muted)

  p <- add_edges(p, panel_edges)

  # The exposure node gets a thick border. Border width is set per layer
  # (label.size) so that it renders in ggplot2 < 3.5, where geom_label has
  # no linewidth aesthetic.
  node_layer <- function(d, border) {
    geom_label(
      data = d, aes(x = x, y = y, label = label, fill = status),
      family = font_family, size = 3.45, lineheight = 0.96, colour = ink,
      label.size = border,
      label.padding = grid::unit(0.36, "lines"), label.r = grid::unit(0.18, "lines")
    )
  }
  p +
    node_layer(nodes[nodes$status != "exposure", ], 0.55) +
    node_layer(nodes[nodes$status == "exposure", ], 1.25) +
    scale_fill_manual(values = c(exposure = "white", conditioned = conditioned_fill, other = "white"),
                      guide = "none") +
    coord_cartesian(xlim = c(-0.78, 7.05), ylim = c(-0.86, 1.68), clip = "off") +
    theme_void(base_family = font_family) +
    theme(plot.margin = margin(3, 7, 3, 7))
}

ge <- "≥"
labels_model1 <- c(
  ACE = "Childhood adversity\nACE-10 count",
  K6 = paste0("Psychological distress\nT1 K6 ", ge, " 13"),
  M = "Emotional-support\nLLM use at T2",
  Y = "Psychological distress\nT3 K6 score"
)
labels_model2 <- c(
  ACE = paste0("Childhood adversity\nACE-10 ", ge, " 4"),
  K6 = "Psychological distress\nT1 K6 score",
  M = "Emotional-support\nLLM use at T2",
  Y = "Psychological distress\nT3 K6 score"
)

panel_a <- make_panel("(A) Model 1: T1 psychological distress as exposure",
                      labels_model1, exposure = "K6", conditioned = "ACE")
panel_b <- make_panel("(B) Model 2: childhood adversity as exposure",
                      labels_model2, exposure = "ACE", conditioned = "K6")

main_figure <- panel_a / panel_b +
  plot_layout(heights = c(1, 1)) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

dir.create(figdir, showWarnings = FALSE)
ggsave(file.path(figdir, "fig_dag_models.pdf"), main_figure,
       width = 7.15, height = 5.15, units = "in", device = cairo_pdf, bg = "white")
ggsave(file.path(figdir, "fig_dag_models.png"), main_figure,
       width = 7.15, height = 5.15, units = "in", dpi = 400, bg = "white")
cat("Done!\n")

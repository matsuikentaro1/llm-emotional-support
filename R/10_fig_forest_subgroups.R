# ============================================================
# Fig. 4: Forest plots of subgroup estimates for the mediation models
# Two blocks stacked vertically with patchwork:
#   (A) Model 1 (K6 >= 13 at T1): path a / path b / ACME (1 x 3)
#   (B) Model 2 (ACE-10 >= 4 at T1): path a / path b / ACME (1 x 3)
# Rows within each panel: Overall, age strata (18-34, 35-49, 50-64, 65+),
# sex (males, females). Rows are grouped by left-hand strips
# ("Overall" / "Age group" / "Sex") via facet_grid(category ~ effect).
# Within each column (path a / path b / ACME) the x-axis range is shared
# between blocks (A) and (B) so that estimates are visually comparable
# across the two models (geom_blank with the pooled range per effect).
#
# All values are read from the analysis output (no hand-typed numbers):
#   analysis/mediation_k6exposure_13_results.txt  (Model 1: MAIN, SUBGROUP MALE/FEMALE)
#   analysis/mediation_3wave_results.txt          (Model 2: MAIN, SUBGROUP MALE/FEMALE)
#     -> path a = exposure coefficient in the mediator model (lm),
#        path b = mediator coefficient in the outcome model (lm),
#        ACME + bootstrap 95% CI from the mediate() summary.
#   analysis/Suppl_Table_4_age_stratified.csv     (age strata)
#     -> columns Model, Stratum, N, path_a_beta, path_a_se,
#        path_b_beta, path_b_se, ACME, ACME_lo, ACME_hi
# Path a / path b 95% CIs are Wald intervals (beta +/- 1.96 x SE), as in
# Table 2 and Supplementary Table 3 of the manuscript.
# Block headings use the >= glyph (rendered with Arial via cairo_pdf / ragg);
# the y-axis label for the oldest stratum stays ASCII ("65 years or older").
# Monochrome, Arial. Output: figures/fig_forest_subgroups.pdf/.png
# ============================================================

library(ggplot2)
library(patchwork)

apath  <- file.path(getwd(), "analysis")
figdir <- file.path(getwd(), "figures")
z <- qnorm(0.975)

# ---- helpers: parse the text logs written by scripts 01/02 ------------------
num <- function(x) as.numeric(x)

section_lines <- function(lines, header_regex) {
  starts <- grep("^===== ", lines)
  s <- grep(header_regex, lines)
  s <- s[s %in% starts][1]
  if (is.na(s)) stop("Section not found: ", header_regex)
  e <- starts[starts > s]
  e <- if (length(e)) e[1] - 1 else length(lines)
  lines[s:e]
}

coef_row <- function(lines, block_index, var) {
  # block_index: 1 = mediator model, 2 = outcome model
  starts <- grep("^Coefficients:", lines)
  if (length(starts) < block_index) stop("Coefficient block ", block_index, " not found")
  blk <- lines[starts[block_index]:length(lines)]
  row <- blk[grep(paste0("^", var, "\\s"), blk)][1]
  if (is.na(row)) stop("Coefficient row not found: ", var)
  f <- strsplit(trimws(row), "\\s+")[[1]]
  c(est = num(f[2]), se = num(f[3]))
}

acme_row <- function(lines) {
  row <- lines[grep("^ACME\\s", lines)][1]
  f <- strsplit(trimws(row), "\\s+")[[1]]
  c(est = num(f[2]), lo = num(f[3]), hi = num(f[4]))
}

sample_n <- function(lines) {
  row <- lines[grep("^Sample Size Used:", lines)][1]
  num(gsub("[^0-9]", "", row))
}

read_log <- function(file, exposure_var, model_label) {
  lines <- readLines(file.path(apath, file), warn = FALSE)
  secs <- list(
    Overall = "^===== MAIN:",
    Males   = "^===== SUBGROUP: MALE \\(",
    Females = "^===== SUBGROUP: FEMALE \\("
  )
  do.call(rbind, lapply(names(secs), function(st) {
    sl <- section_lines(lines, secs[[st]])
    a  <- coef_row(sl, 1, exposure_var)
    b  <- coef_row(sl, 2, "llm_mental")
    ac <- acme_row(sl)
    data.frame(
      model = model_label, stratum = st, n = sample_n(sl),
      effect = c("Path a", "Path b", "ACME"),
      est = c(a["est"], b["est"], ac["est"]),
      lo  = c(a["est"] - z * a["se"], b["est"] - z * b["se"], ac["lo"]),
      hi  = c(a["est"] + z * a["se"], b["est"] + z * b["se"], ac["hi"])
    )
  }))
}

lab1 <- "Model 1 (K6 ≥ 13 at T1)"
lab2 <- "Model 2 (ACE-10 ≥ 4 at T1)"

m1 <- read_log("mediation_k6exposure_13_results.txt", "k6t1_13", lab1)
m2 <- read_log("mediation_3wave_results.txt", "ace10_4", lab2)

# ---- age strata (tabulated CSV) --------------------------------------------
age <- read.csv(file.path(apath, "Suppl_Table_4_age_stratified.csv"))
age_df <- do.call(rbind, lapply(seq_len(nrow(age)), function(i) {
  r <- age[i, ]
  data.frame(
    model   = ifelse(grepl("H1", r$Model), lab1, lab2),
    stratum = ifelse(r$Stratum == "65+", "65 years or older", paste(r$Stratum, "years")),
    n = r$N,
    effect = c("Path a", "Path b", "ACME"),
    est = c(r$path_a_beta, r$path_b_beta, r$ACME),
    lo  = c(r$path_a_beta - z * r$path_a_se, r$path_b_beta - z * r$path_b_se, r$ACME_lo),
    hi  = c(r$path_a_beta + z * r$path_a_se, r$path_b_beta + z * r$path_b_se, r$ACME_hi)
  )
}))

df <- rbind(m1, m2, age_df)
rownames(df) <- NULL

order_levels <- rev(c("Overall", "18-34 years", "35-49 years", "50-64 years",
                      "65 years or older", "Males", "Females"))
df$stratum <- factor(df$stratum, levels = order_levels)
df$model   <- factor(df$model, levels = c(lab1, lab2))
df$effect  <- factor(df$effect, levels = c("Path a", "Path b", "ACME"),
                     labels = c("Path a: exposure to LLM use, beta (95% CI)",
                                "Path b: LLM use to K6 at T3, beta (95% CI)",
                                "ACME (K6 points at T3, 95% CI)"))
df$grp <- ifelse(df$stratum == "Overall", "overall", "sub")
df$category <- factor(
  ifelse(df$stratum == "Overall", "Overall",
         ifelse(df$stratum %in% c("Males", "Females"), "Sex", "Age group")),
  levels = c("Overall", "Age group", "Sex"))

# pooled x-range per effect (both models) -> identical x-axes in (A) and (B)
xrng <- do.call(rbind, lapply(split(df, df$effect), function(d)
  data.frame(effect = d$effect[1], x = c(min(d$lo), max(d$hi)))))

# ---- plot: one 1 x 3 block per model, stacked with patchwork -------------
forest_block <- function(d, heading) {
  ggplot(d, aes(x = est, y = stratum)) +
    geom_blank(data = xrng, aes(x = x), inherit.aes = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.4) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.22, linewidth = 0.5,
                  orientation = "y") +
    geom_point(aes(shape = grp), size = 2.3, fill = "black") +
    scale_shape_manual(values = c(overall = 23, sub = 15), guide = "none") +
    scale_y_discrete(labels = function(x) ifelse(x == "Overall", "", x)) +
    facet_grid(category ~ effect, scales = "free", space = "free_y", switch = "y") +
    labs(x = "Estimate (95% CI)", y = NULL, title = heading) +
    theme_bw(base_size = 10, base_family = "Arial") +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "grey92", color = "black"),
          strip.text = element_text(face = "bold", size = 8.5),
          strip.text.y.left = element_text(angle = 0, hjust = 1),
          strip.placement = "outside",
          strip.switch.pad.grid = grid::unit(0.12, "in"),
          axis.text = element_text(color = "black"),
          axis.title.x = element_text(size = 9),
          plot.title = element_text(face = "bold", size = 11, hjust = 0),
          plot.title.position = "plot",
          panel.spacing.x = grid::unit(0.2, "in"),
          panel.spacing.y = grid::unit(0.08, "in"))
}

pA <- forest_block(df[df$model == lab1, ], paste0("(A) ", lab1))
pB <- forest_block(df[df$model == lab2, ], paste0("(B) ", lab2))
p <- pA / pB + plot_layout(heights = c(1, 1)) &
  theme(plot.margin = margin(4, 6, 4, 6))

dir.create(figdir, showWarnings = FALSE)
ggsave(file.path(figdir, "fig_forest_subgroups.pdf"), p, width = 10, height = 7,
       device = cairo_pdf)
ggsave(file.path(figdir, "fig_forest_subgroups.png"), p, width = 10, height = 7,
       dpi = 300, device = ragg::agg_png)

# printed check against the manuscript numbers (Table 2 / Suppl. Table 3)
chk <- df
chk$est <- sprintf("%.3f", chk$est); chk$lo <- sprintf("%.3f", chk$lo); chk$hi <- sprintf("%.3f", chk$hi)
print(chk[order(chk$model, chk$effect, chk$stratum), c("model", "effect", "stratum", "n", "est", "lo", "hi")],
      row.names = FALSE)
cat("Done!\n")

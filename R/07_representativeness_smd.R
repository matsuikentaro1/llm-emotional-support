# ============================================================
# Supplementary Table 1: comparison of the analytic sample with the
# other eligible T1 respondents (valid T1 respondents not included in
# the analytic sample) on T1 characteristics.
#
# Continuous: mean (SD), standardized mean difference (SMD).
# Categorical: n (%), per-level SMD (binary-indicator SMD).
# Input:  data/attrition_t1.csv
# Output: analysis/Suppl_Table_representativeness.csv
# ============================================================

in_path  <- file.path(getwd(), "data", "attrition_t1.csv")
out_dir  <- file.path(getwd(), "analysis")
out_path <- file.path(out_dir, "Suppl_Table_representativeness.csv")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

continuous <- c(age = "Age, years", K6_T1 = "K6 at T1", ace10_count = "ACE-10 count")
binary <- c(female = "Female", ace10_4 = "ACE-10 >= 4", smoking = "Current smoking",
            alcohol = "Alcohol use", physical_illness = "Physical illness",
            psychiatric_illness = "Psychiatric illness")
categorical <- list(
  edu_3cat        = list(label = "Education",
                         levels = c("high_school", "vocational", "university")),
  income_5cat     = list(label = "Household income",
                         levels = c("low", "mid", "mid_high", "high", "unknown")),
  marital_3cat    = list(label = "Marital status",
                         levels = c("married", "never", "separated")),
  employment_4cat = list(label = "Employment",
                         levels = c("regular", "non_regular", "self_employed", "not_working")))

d <- read.csv(in_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
              na.strings = c("", "NA"))
g1 <- d[d$in_analytic == 1, ]
g0 <- d[d$in_analytic == 0, ]

smd_cont <- function(x1, x0) {
  pooled <- sqrt((var(x1) + var(x0)) / 2)
  if (pooled > 0) (mean(x1) - mean(x0)) / pooled else 0
}
smd_prop <- function(p1, p0) {
  pooled <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  if (pooled > 0) (p1 - p0) / pooled else 0
}
f2 <- function(x) sprintf("%.2f", x)
pct <- function(k, n) sprintf("%d (%.1f%%)", as.integer(k), 100 * k / n)
n_note <- function(n_obs, n_all) if (n_obs < n_all) sprintf(" [n=%d]", n_obs) else ""

out <- data.frame(Characteristic = character(), g1 = character(), g0 = character(),
                  SMD = character(), stringsAsFactors = FALSE)
add <- function(...) out <<- rbind(out, data.frame(..., stringsAsFactors = FALSE))

for (v in names(continuous)) {
  x1 <- na.omit(g1[[v]]); x0 <- na.omit(g0[[v]])
  add(Characteristic = paste0(continuous[[v]], ", mean (SD)"),
      g1 = sprintf("%s (%s)", f2(mean(x1)), f2(sd(x1))),
      g0 = paste0(sprintf("%s (%s)", f2(mean(x0)), f2(sd(x0))), n_note(length(x0), nrow(g0))),
      SMD = sprintf("%.3f", smd_cont(x1, x0)))
}

for (v in names(binary)) {
  x1 <- na.omit(g1[[v]]); x0 <- na.omit(g0[[v]])
  p1 <- mean(x1); p0 <- mean(x0)
  add(Characteristic = paste0(binary[[v]], ", n (%)"),
      g1 = pct(sum(x1), length(x1)),
      g0 = paste0(pct(sum(x0), length(x0)), n_note(length(x0), nrow(g0))),
      SMD = sprintf("%.3f", smd_prop(p1, p0)))
}

for (v in names(categorical)) {
  v1 <- na.omit(g1[[v]]); v0 <- na.omit(g0[[v]])
  add(Characteristic = paste0(categorical[[v]]$label, ", n (%)"), g1 = "",
      g0 = if (length(v0) < nrow(g0)) sprintf("[n=%d]", length(v0)) else "", SMD = "")
  for (lev in categorical[[v]]$levels) {
    c1 <- sum(v1 == lev); c0 <- sum(v0 == lev)
    add(Characteristic = paste0("  ", lev),
        g1 = pct(c1, length(v1)), g0 = pct(c0, length(v0)),
        SMD = sprintf("%.3f", smd_prop(c1 / length(v1), c0 / length(v0))))
  }
}

names(out) <- c("Characteristic",
                sprintf("Analytic sample (n=%d)", nrow(g1)),
                sprintf("Not in analytic sample (n=%d)", nrow(g0)),
                "SMD")
write.csv(out, out_path, row.names = FALSE, fileEncoding = "UTF-8")
print(out, row.names = FALSE, right = FALSE)
cat("\nwrote", out_path, "\n")

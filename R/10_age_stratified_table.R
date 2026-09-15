# ============================================================
# Age-stratified causal mediation analysis (Supplementary Table 3)
#
# Models:     H1 (K6 >= 13 at T1) and H2 (ACE-10 >= 4 at T1)
# Mediator:   LLM emotional support frequency (T2, continuous 1-5)
# Outcome:    K6 at T3 (continuous, 0-24)
# Age strata: 18-34, 35-49, 50-64, 65+ (strata with N < 200 are skipped)
#
# Same covariates, seed (42) and bootstrap settings (5000 sims) as
# scripts 01 and 02, which also print the per-stratum estimates in
# their result logs.
# Output: analysis/Suppl_Table_4_age_stratified.csv (and .xlsx),
#         read by script 11 to draw Fig. 4.
# ============================================================

library(mediation)

data_path   <- file.path(getwd(), "data", "3wave_analysis.csv")
output_path <- file.path(getwd(), "analysis")
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

d <- read.csv(data_path, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
cat("N (raw) =", nrow(d), "\n")

# ---- Prepare variables ----
d$k6t1_13 <- as.integer(d$K6_T1 >= 13)

d$edu_3cat        <- factor(d$edu_3cat,        levels = c("high_school", "vocational", "university"))
d$income_5cat     <- factor(d$income_5cat,     levels = c("mid", "low", "mid_high", "high", "unknown"))
d$marital_3cat    <- factor(d$marital_3cat,    levels = c("married", "never", "separated"))
d$employment_4cat <- factor(d$employment_4cat, levels = c("regular", "non_regular", "self_employed", "not_working"))

# Age strata
d$age_4cat <- cut(d$age, breaks = c(-Inf, 34, 49, 64, Inf),
                  labels = c("18-34", "35-49", "50-64", "65+"))

vars_needed <- c("K6_T1", "K6_T3", "ace10_4", "ace10_count", "k6t1_13",
                 "llm_mental", "age", "age_4cat", "female",
                 "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
                 "smoking", "alcohol", "physical_illness", "psychiatric_illness")
d_cc <- d[complete.cases(d[, vars_needed]), ]
cat("Complete cases:", nrow(d_cc), "\n")
cat("\nAge stratum sample sizes:\n")
print(table(d_cc$age_4cat))

SIMS <- 5000

run_age_stratum <- function(stratum, exposure_var, model_label) {
  d_s <- d_cc[d_cc$age_4cat == stratum, ]
  n_s <- nrow(d_s)
  cat(sprintf("\n----- %s | Age %s | N=%d -----\n", model_label, stratum, n_s))
  if (n_s < 200) {
    cat("  Skipped (N < 200, unstable)\n")
    return(NULL)
  }
  cov_str <- "K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat + smoking + alcohol + physical_illness + psychiatric_illness"
  if (exposure_var == "k6t1_13") {
    cov_str <- "ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat + smoking + alcohol + physical_illness + psychiatric_illness"
  }
  med_formula <- as.formula(sprintf("llm_mental ~ %s + %s", exposure_var, cov_str))
  out_formula <- as.formula(sprintf("K6_T3 ~ %s + llm_mental + %s", exposure_var, cov_str))
  environment(med_formula) <- globalenv()
  environment(out_formula) <- globalenv()
  # bquote + eval-in-globalenv so mediate()'s bootstrap update() can re-fit
  assign("d_s", d_s, envir = globalenv())
  med_call <- bquote(lm(.(med_formula), data = d_s))
  out_call <- bquote(lm(.(out_formula), data = d_s))
  med <- eval(med_call, envir = globalenv())
  out <- eval(out_call, envir = globalenv())

  set.seed(42)
  res <- mediate(med, out, treat = exposure_var, mediator = "llm_mental",
                 boot = TRUE, sims = SIMS)
  cat(sprintf("  ACME = %.4f [%.4f, %.4f]  p = %.4f  Prop = %.3f%%\n",
              res$d0, res$d0.ci[1], res$d0.ci[2], res$d0.p, res$n0 * 100))
  return(list(label = model_label, stratum = stratum, N = n_s,
              path_a = summary(med)$coefficients[exposure_var, ],
              path_b = summary(out)$coefficients["llm_mental", ],
              res = res))
}

results <- list()
for (stratum in c("18-34", "35-49", "50-64", "65+")) {
  results[[paste0("H1_", stratum)]] <- run_age_stratum(stratum, "k6t1_13", "H1 (K6>=13)")
  results[[paste0("H2_", stratum)]] <- run_age_stratum(stratum, "ace10_4", "H2 (ACE-10>=4)")
}

# ---- Build table ----
rows <- list()
for (r in results) {
  if (is.null(r)) next
  rows[[length(rows) + 1]] <- data.frame(
    Model = r$label,
    Stratum = r$stratum,
    N = r$N,
    path_a_beta = r$path_a[1], path_a_se = r$path_a[2], path_a_p = r$path_a[4],
    path_b_beta = r$path_b[1], path_b_se = r$path_b[2], path_b_p = r$path_b[4],
    ACME = r$res$d0, ACME_lo = r$res$d0.ci[1], ACME_hi = r$res$d0.ci[2],
    ACME_p = r$res$d0.p,
    Prop_Med = r$res$n0,
    stringsAsFactors = FALSE
  )
}
tbl <- do.call(rbind, rows)
write.csv(tbl, file.path(output_path, "Suppl_Table_4_age_stratified.csv"), row.names = FALSE)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(tbl, file.path(output_path, "Suppl_Table_4_age_stratified.xlsx"), rowNames = FALSE)
}
cat("\nSupplementary Table 4 saved.\n")
print(tbl)

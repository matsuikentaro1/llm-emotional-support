# ============================================================
# Supplementary Table 2: Mediation by individual LLM use cases (T2)
#
# Rationale:
#   Map the harmful mediator-outcome pathway across all nine LLM use
#   cases. We test each Q37S3.1-9 use case individually as its own
#   T2 mediator (use 1-8 = non-emotional-support; use 9 = emotional
#   support, identical model spec to the main analysis full-sample
#   continuous row), plus three aggregate indicators (sum, max,
#   binary current AI user) for completeness. Full sample only;
#   subgroups remain in Tables 2 and 3.
#
# Exposures (rows):
#   H1. K6 >= 13 at T1 (binary)
#   H2. ACE-10 >= 4 at T1 (binary)
#
# Mediators (columns), per exposure:
#   M01-M08. llm_use_1..8 (continuous, 1-5)  = Q37S3.1..8 (eight non-
#            emotional-support use cases)
#   M09.     llm_mental (continuous, 1-5)    = Q37S3.9 (emotional support;
#            primary mediator in the main analysis)
#   M10.     llm_gen_sum (continuous, 9-45)  = sum(Q37S3.1..9)
#   M11.     llm_gen_max (continuous, 1-5)   = max(Q37S3.1..9)
#   M12.     llm_gen_any (binary, 0/1)       = (llm_gen_max >= 2)
#            (any of nine T2 use-purpose items reported >= "Rarely")
#
# Outcome:   K6 at T3 (continuous, 0-24)
# Covariates for H1: ACE-10 count, age, sex, education (3-cat),
#                    income (5-cat incl. unknown), marital (3-cat),
#                    employment (4-cat), smoking, alcohol, physical illness.
# Covariates for H2: K6 at T1, age, sex, education (3-cat),
#                    income (5-cat incl. unknown), marital (3-cat),
#                    employment (4-cat), smoking, alcohol, physical illness.
# ============================================================

library(mediation)
library(car)

data_path   <- file.path(getwd(), "data", "3wave_analysis.csv")
output_path <- file.path(getwd(), "analysis")

d <- read.csv(data_path, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
cat("N =", nrow(d), "\n")

# ---- Prepare variables ----
d$k6t1_13 <- as.integer(d$K6_T1 >= 13)

# Override llm_gen_any: use behavioral definition (any of nine T2 use-purpose
# items reported >= "Rarely") instead of the self-reported llm_start question.
d$llm_gen_any <- as.integer(d$llm_gen_max >= 2)

# Factor encoding with explicit reference levels
d$edu_3cat        <- factor(d$edu_3cat,        levels = c("high_school", "vocational", "university"))
d$income_5cat     <- factor(d$income_5cat,     levels = c("mid", "low", "mid_high", "high", "unknown"))
d$marital_3cat    <- factor(d$marital_3cat,    levels = c("married", "never", "separated"))
d$employment_4cat <- factor(d$employment_4cat, levels = c("regular", "non_regular", "self_employed", "not_working"))

individual_meds <- c(paste0("llm_use_", 1:8), "llm_mental")  # use 1..8 + emotional support (use 9)
aggregate_meds  <- c("llm_gen_sum", "llm_gen_max", "llm_gen_any")

vars_needed <- c("K6_T1", "K6_T3", "k6t1_13", "ace10_count", "ace10_4",
                 individual_meds, aggregate_meds,
                 "age", "female",
                 "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
                 "smoking", "alcohol", "physical_illness", "psychiatric_illness")
d_cc <- d[complete.cases(d[, vars_needed]), ]
cat("Complete cases:", nrow(d_cc), "\n")
cat(sprintf("  K6 >= 13 at T1: %d (%.1f%%)\n",
            sum(d_cc$k6t1_13), mean(d_cc$k6t1_13) * 100))
cat(sprintf("  ACE-10 >= 4:    %d (%.1f%%)\n",
            sum(d_cc$ace10_4),  mean(d_cc$ace10_4)  * 100))

# Pre-flight VIF check
vif_model <- lm(K6_T3 ~ k6t1_13 + llm_mental + ace10_count + age + female +
                  edu_3cat + income_5cat + marital_3cat + employment_4cat +
                  smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
vif_vals <- car::vif(vif_model)
vif_scalar <- if (is.matrix(vif_vals)) vif_vals[, "GVIF^(1/(2*Df))"]^2 else vif_vals
cat("VIF scalar:\n"); print(round(vif_scalar, 3))
if (any(vif_scalar > 5)) {
  stop("VIF > 5 detected. Offending variables: ",
       paste(names(vif_scalar)[vif_scalar > 5], collapse = ", "))
}
cat(">>> All VIFs <= 5. Proceeding.\n\n")

SIMS <- 5000

# Storage for compact table rows
rows <- list()

save_row <- function(label, exposure, mediator, is_binary, med_fit, out_fit, res) {
  ea <- summary(med_fit)$coefficients[exposure, ]
  eb <- summary(out_fit)$coefficients[mediator, ]
  rows[[length(rows) + 1]] <<- data.frame(
    label         = label,
    exposure      = exposure,
    mediator      = mediator,
    mediator_type = if (is_binary) "binary" else "continuous",
    N             = nrow(d_cc),
    path_a_beta   = ea[1], path_a_se = ea[2], path_a_p = ea[4],
    path_b_beta   = eb[1], path_b_se = eb[2], path_b_p = eb[4],
    ACME          = res$d0, ACME_lo = res$d0.ci[1], ACME_hi = res$d0.ci[2],
    ACME_p        = res$d0.p,
    ADE           = res$z0,
    total_effect  = res$tau.coef,
    prop_mediated = res$n0,
    prop_mediated_p = res$n0.p,
    stringsAsFactors = FALSE
  )
}

dump_section <- function(label, med_fit, out_fit, res) {
  cat("\n\n===== ", label, " =====\n", sep = "")
  cat("\nMediator:\n"); print(summary(med_fit))
  cat("\nOutcome:\n"); print(summary(out_fit))
  cat("\nMediation:\n"); print(summary(res))
}

# Generic continuous-mediator runner (LM mediator + LM outcome)
# Use bquote to inline the formula INTO the lm() call so that
# mediate()'s bootstrap update() can re-fit without depending on
# function-local variables like `med_formula`.
run_continuous <- function(exposure, mediator, label_prefix) {
  cov_str_h1 <- "ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat + smoking + alcohol + physical_illness + psychiatric_illness"
  cov_str_h2 <- "K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat + smoking + alcohol + physical_illness + psychiatric_illness"
  cov_str <- if (exposure == "k6t1_13") cov_str_h1 else cov_str_h2

  med_formula <- as.formula(sprintf("%s ~ %s + %s", mediator, exposure, cov_str))
  out_formula <- as.formula(sprintf("K6_T3 ~ %s + %s + %s", exposure, mediator, cov_str))
  environment(med_formula) <- globalenv()
  environment(out_formula) <- globalenv()

  med_call <- bquote(lm(.(med_formula), data = d_cc))
  out_call <- bquote(lm(.(out_formula), data = d_cc))
  med <- eval(med_call, envir = globalenv())
  out <- eval(out_call, envir = globalenv())

  set.seed(42)
  res <- mediate(med, out, treat = exposure, mediator = mediator,
                 boot = TRUE, sims = SIMS)
  full_label <- sprintf("%s  %s -> %s -> K6_T3", label_prefix, exposure, mediator)
  dump_section(full_label, med, out, res)
  save_row(label_prefix, exposure, mediator, FALSE, med, out, res)
}

run_binary_any <- function(exposure, label_prefix) {
  cov_str_h1 <- "ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat + smoking + alcohol + physical_illness + psychiatric_illness"
  cov_str_h2 <- "K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat + smoking + alcohol + physical_illness + psychiatric_illness"
  cov_str <- if (exposure == "k6t1_13") cov_str_h1 else cov_str_h2

  med_formula <- as.formula(sprintf("llm_gen_any ~ %s + %s", exposure, cov_str))
  out_formula <- as.formula(sprintf("K6_T3 ~ %s + llm_gen_any + %s", exposure, cov_str))
  environment(med_formula) <- globalenv()
  environment(out_formula) <- globalenv()

  med_call <- bquote(glm(.(med_formula), family = binomial(link = "logit"), data = d_cc))
  out_call <- bquote(lm(.(out_formula), data = d_cc))
  med <- eval(med_call, envir = globalenv())
  out <- eval(out_call, envir = globalenv())

  set.seed(42)
  res <- mediate(med, out, treat = exposure, mediator = "llm_gen_any",
                 boot = TRUE, sims = SIMS)
  full_label <- sprintf("%s  %s -> llm_gen_any -> K6_T3", label_prefix, exposure)
  dump_section(full_label, med, out, res)
  save_row(label_prefix, exposure, "llm_gen_any", TRUE, med, out, res)
}

# ---------- Driver ----------

dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
sink(file.path(output_path, "Suppl_Table_2_general_LLM_results.txt"))

cat("[TABLE 4] Mediation by individual LLM use cases (T2)\n")
cat("Mediator options:\n")
cat("  M01-M08: llm_use_1..8 (each Q37S3.1..8 individually, non-emotional)\n")
cat("  M09:     llm_mental (Q37S3.9, emotional support; matches main analysis)\n")
cat("  M10:     llm_gen_sum (sum of Q37S3.1..9, range 9-45)\n")
cat("  M11:     llm_gen_max (max of Q37S3.1..9, range 1-5)\n")
cat("  M12:     llm_gen_any (binary, llm_gen_max>=2)\n")
cat("Mediator source:  Q37S3.1-9 (Q37S3.10='other' excluded)\n")
cat("Outcome: K6_T3 (continuous)\n")
cat(sprintf("Full sample N = %d\n", nrow(d_cc)))

t_start <- Sys.time()

for (exposure_info in list(
        list(exp = "k6t1_13", prefix = "H1"),
        list(exp = "ace10_4", prefix = "H2"))) {

  exposure <- exposure_info$exp
  prefix   <- exposure_info$prefix
  cat(sprintf("\n\n############# %s (exposure = %s) #############\n", prefix, exposure))

  # 8 non-emotional-support use cases
  for (i in 1:8) {
    med_var <- paste0("llm_use_", i)
    label   <- sprintf("%s_use%d", prefix, i)
    run_continuous(exposure, med_var, label)
  }
  # use 9: emotional support (replicates the main-analysis full-sample continuous row
  # within bootstrap variability, included here so Table 4 can show the full gradient)
  run_continuous(exposure, "llm_mental", paste0(prefix, "_use9"))

  # Aggregate: sum, max
  run_continuous(exposure, "llm_gen_sum", paste0(prefix, "_sum"))
  run_continuous(exposure, "llm_gen_max", paste0(prefix, "_max"))

  # Aggregate: any (binary)
  run_binary_any(exposure, paste0(prefix, "_any"))
}

t_end <- Sys.time()
cat(sprintf("\n\nElapsed: %.1f min\n", as.numeric(t_end - t_start, units = "mins")))

sink()
cat("\nFull results saved to analysis/Suppl_Table_2_general_LLM_results.txt\n")

# ============================================================
# Compact CSV output (one row per exposure x mediator)
# ============================================================
tab <- do.call(rbind, rows)
rownames(tab) <- NULL
write.csv(tab, file.path(output_path, "Suppl_Table_2_general_LLM.csv"),
          row.names = FALSE)
cat("Summary CSV written:",
    file.path(output_path, "Suppl_Table_2_general_LLM.csv"), "\n")
cat("Total rows:", nrow(tab), "\n")
cat("Done!\n")

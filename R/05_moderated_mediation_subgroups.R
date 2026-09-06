# ============================================================
# Moderated mediation: exposure x subgroup tests (R3-9)
#
# Purpose: R3-9 requires a direct between-group test of the
# indirect effect before describing it as "concentrated" among
# younger adults or males. This script provides that test.
#
# PRE-SPECIFIED CONTRASTS (fixed before execution, 2026-08-12):
#   Sex moderator:  female (1) vs male (0)
#   Age moderator:  age_u50 = 1 if age 18-49, 0 if age >= 50
#     (matches the manuscript's claim pattern: indirect effect
#      observed in the 18-34/35-49 strata but not in 50-64/>=65)
#   Primary test:  ACME difference between moderator levels via
#                  test.modmed() (percentile bootstrap, 5000 sims)
#   Secondary:     Wald tests of the interaction coefficients
#                  (exposure x moderator in the mediator model;
#                   exposure x moderator and mediator x moderator
#                   in the outcome model)
#
# Both exposures are tested: K6>=13 (Model 1) and ACE-10>=4 (Model 2).
# Moderator main effects and interactions are included in both
# regressions; remaining covariates as in the main analyses.
# Output: analysis/modmed_subgroups_results.txt
# ============================================================

library(mediation)

data_path <- file.path(getwd(), "data", "3wave_analysis.csv")
output_path <- file.path(getwd(), "analysis")

d <- read.csv(data_path, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
d$k6t1_13 <- as.integer(d$K6_T1 >= 13)
d$edu_3cat        <- factor(d$edu_3cat,        levels = c("high_school", "vocational", "university"))
d$income_5cat     <- factor(d$income_5cat,     levels = c("mid", "low", "mid_high", "high", "unknown"))
d$marital_3cat    <- factor(d$marital_3cat,    levels = c("married", "never", "separated"))
d$employment_4cat <- factor(d$employment_4cat, levels = c("regular", "non_regular", "self_employed", "not_working"))

vars_needed <- c("K6_T1", "K6_T3", "ace10_4", "ace10_count",
                 "llm_mental", "llm_gen_max", "age", "female",
                 "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
                 "smoking", "alcohol", "physical_illness", "psychiatric_illness")
d_cc <- d[complete.cases(d[, vars_needed]), ]
d_cc$age_u50 <- as.integer(d_cc$age < 50)
cat("Complete cases:", nrow(d_cc), "\n")
cat("  age 18-49:", sum(d_cc$age_u50), " age >=50:", sum(1 - d_cc$age_u50), "\n\n")

SIMS <- 5000

sink(file.path(output_path, "modmed_subgroups_results.txt"), split = TRUE)
cat("============================================================\n")
cat("Moderated mediation: exposure x subgroup (R3-9)\n")
cat("Complete cases:", nrow(d_cc), " Bootstrap:", SIMS, "sims, seed 42\n")
cat("Pre-specified contrasts: female (1 vs 0); age_u50 (18-49 vs >=50)\n")
cat("============================================================\n")

# base covariate strings (moderator handled separately)
covs_base <- "edu_3cat + income_5cat + marital_3cat + employment_4cat + smoking + alcohol + physical_illness + psychiatric_illness"

run_modmed <- function(exposure, mutual_cov, moderator, other_covs, label) {
  cat("\n\n============================================================\n")
  cat(label, "\n")
  cat("============================================================\n")
  med_f <- as.formula(sprintf(
    "llm_mental ~ %s * %s + %s + %s", exposure, moderator, mutual_cov, other_covs))
  out_f <- as.formula(sprintf(
    "K6_T3 ~ %s * %s + llm_mental * %s + %s + %s", exposure, moderator, moderator, mutual_cov, other_covs))
  environment(med_f) <- globalenv()
  environment(out_f) <- globalenv()
  # bquote inlines the formula into the lm() call so that mediate()'s
  # bootstrap update() can re-fit without depending on function-local
  # variables (same workaround as in mediation_general_LLM.R)
  med_fit <- eval(bquote(lm(.(med_f), data = d_cc)))
  out_fit <- eval(bquote(lm(.(out_f), data = d_cc)))

  cat("\nMediator model:\n"); print(summary(med_fit))
  cat("\nOutcome model:\n"); print(summary(out_fit))

  int_a <- paste0(exposure, ":", moderator)
  cm <- coef(summary(med_fit))
  cat("\nSecondary Wald tests:\n")
  cat(sprintf("  a-path interaction %s: beta=%.4f p=%.4f\n",
              int_a, cm[int_a, "Estimate"], cm[int_a, "Pr(>|t|)"]))
  co <- coef(summary(out_fit))
  int_b <- intersect(c(paste0(moderator, ":llm_mental"), paste0("llm_mental:", moderator)), rownames(co))
  cat(sprintf("  outcome %s: beta=%.4f p=%.4f\n",
              int_a, co[int_a, "Estimate"], co[int_a, "Pr(>|t|)"]))
  cat(sprintf("  b-path interaction %s: beta=%.4f p=%.4f\n",
              int_b, co[int_b, "Estimate"], co[int_b, "Pr(>|t|)"]))

  set.seed(42)
  res <- mediate(med_fit, out_fit, treat = exposure, mediator = "llm_mental",
                 boot = TRUE, sims = SIMS)
  cov1 <- setNames(list(1), moderator)
  cov0 <- setNames(list(0), moderator)
  set.seed(42)
  res1 <- mediate(med_fit, out_fit, treat = exposure, mediator = "llm_mental",
                  covariates = cov1, boot = TRUE, sims = SIMS)
  set.seed(42)
  res0 <- mediate(med_fit, out_fit, treat = exposure, mediator = "llm_mental",
                  covariates = cov0, boot = TRUE, sims = SIMS)
  cat(sprintf("\nACME at %s = 1: %.4f [%.4f, %.4f] p=%.4f\n",
              moderator, res1$d.avg, res1$d.avg.ci[1], res1$d.avg.ci[2], res1$d.avg.p))
  cat(sprintf("ACME at %s = 0: %.4f [%.4f, %.4f] p=%.4f\n",
              moderator, res0$d.avg, res0$d.avg.ci[1], res0$d.avg.ci[2], res0$d.avg.p))

  set.seed(42)
  tm <- test.modmed(res, covariates.1 = cov1, covariates.2 = cov0, sims = SIMS)
  cat("\nPRIMARY TEST -- test.modmed (ACME difference, level 1 minus level 0):\n")
  print(tm)
  invisible(list(res1 = res1, res0 = res0, tm = tm))
}

r1 <- run_modmed("k6t1_13", "ace10_count", "female",
                 paste("age +", covs_base),
                 "M1-SEX: K6>=13 x female")
r2 <- run_modmed("ace10_4", "K6_T1", "female",
                 paste("age +", covs_base),
                 "M2-SEX: ACE>=4 x female")
r3 <- run_modmed("k6t1_13", "ace10_count", "age_u50",
                 paste("female +", covs_base),
                 "M1-AGE: K6>=13 x age_u50 (18-49 vs >=50)")
r4 <- run_modmed("ace10_4", "K6_T1", "age_u50",
                 paste("female +", covs_base),
                 "M2-AGE: ACE>=4 x age_u50 (18-49 vs >=50)")

cat("\n\n============================================================\n")
cat("SUMMARY (moderated mediation)\n")
cat("============================================================\n")
smy <- function(label, r) {
  # test.modmed returns a list of htest objects; [[1]] is the ACME test
  ht <- r$tm[[1]]
  cat(sprintf("%-44s ACME(1)=%.4f  ACME(0)=%.4f  diff=%.4f [%.4f, %.4f] p=%.4f\n",
              label, r$res1$d.avg, r$res0$d.avg,
              ht$estimate, ht$conf.int[1], ht$conf.int[2], ht$p.value))
}
smy("M1-SEX (K6>=13, female vs male)", r1)
smy("M2-SEX (ACE>=4, female vs male)", r2)
smy("M1-AGE (K6>=13, 18-49 vs >=50)", r3)
smy("M2-AGE (ACE>=4, 18-49 vs >=50)", r4)

sink()
cat("\nAll results saved to analysis/modmed_subgroups_results.txt\nDone!\n")

# ============================================================
# Additional analyses for PCN revision (R1-1, R1-2, R2-5c, R3-4)
#
# A1: K6_T1-excluded ACE model ("total-pathway estimand", R1-2
#     first-choice version, for Supplementary)
# A2: exposure x mediator interaction test (R1-1 iii) -- both
#     exposures (ACE >= 4 and K6 >= 13); pre-specified contrast:
#     Wald test of the product term in the outcome model, and
#     ACME(control) vs ACME(treated) from mediate()
# A3: continuous-exposure version (ACE-10 count, 0 vs 4)
# A3b: continuous-exposure version of Model 1 (K6_T1 score, 0 vs 13)
# A4: T2 K6-adjusted ACE model (R2-5c, reference values only --
#     conditioning on a post-treatment variable; complete cases
#     re-derived including K6_T2)
# A5: medsens sensitivity analysis for M-Y confounding (R3-4);
#     quasi-Bayesian mediate (boot = FALSE) as required by medsens
#
# Same data, covariates, seed (42) and bootstrap settings
# (5000 sims, percentile CI) as the main scripts.
# Output: analysis/additional_analyses_revision_results.txt
# ============================================================

library(mediation)
library(dplyr)

data_path <- file.path(getwd(), "data", "3wave_analysis.csv")
output_path <- file.path(getwd(), "analysis")

d <- read.csv(data_path, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
cat("N =", nrow(d), "\n")

d$llm_any <- as.integer(d$llm_mental >= 2)
d$k6t1_13 <- as.integer(d$K6_T1 >= 13)

d$edu_3cat        <- factor(d$edu_3cat,        levels = c("high_school", "vocational", "university"))
d$income_5cat     <- factor(d$income_5cat,     levels = c("mid", "low", "mid_high", "high", "unknown"))
d$marital_3cat    <- factor(d$marital_3cat,    levels = c("married", "never", "separated"))
d$employment_4cat <- factor(d$employment_4cat, levels = c("regular", "non_regular", "self_employed", "not_working"))

vars_needed <- c("K6_T1", "K6_T3", "ace10_4", "ace10_count",
                 "llm_any", "llm_mental", "llm_gen_max",
                 "age", "female",
                 "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
                 "smoking", "alcohol", "physical_illness", "psychiatric_illness")
d_cc <- d[complete.cases(d[, vars_needed]), ]
cat("Complete cases (main):", nrow(d_cc), "\n\n")

SIMS <- 5000

sink(file.path(output_path, "additional_analyses_revision_results.txt"), split = TRUE)

cat("============================================================\n")
cat("Additional analyses for PCN revision\n")
cat("Data: 3wave_analysis.csv  Complete cases:", nrow(d_cc), "\n")
cat("Bootstrap: 5000 sims, percentile CI, seed 42 (A5: quasi-Bayesian)\n")
cat("============================================================\n")

# ============================================================
# A1: K6_T1-excluded ACE model (total-pathway estimand)
# ============================================================
cat("\n\n============================================================\n")
cat("A1: ACE-10>=4 -> LLM mental (continuous) -> K6_T3, WITHOUT K6_T1\n")
cat("    (same complete-case sample as the main model)\n")
cat("============================================================\n")
med_a1 <- lm(llm_mental ~ ace10_4 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_a1 <- lm(K6_T3 ~ ace10_4 + llm_mental + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
cat("\nMediator model:\n"); print(summary(med_a1))
cat("\nOutcome model:\n"); print(summary(out_a1))
set.seed(42)
res_a1 <- mediate(med_a1, out_a1, treat = "ace10_4", mediator = "llm_mental",
                  boot = TRUE, sims = SIMS)
cat("\nMediation:\n"); print(summary(res_a1))

# ============================================================
# A2: exposure x mediator interaction
# ============================================================
cat("\n\n============================================================\n")
cat("A2-1: interaction ace10_4 x llm_mental (outcome model incl. K6_T1)\n")
cat("      Pre-specified test: Wald test of the product term\n")
cat("============================================================\n")
med_a2 <- lm(llm_mental ~ ace10_4 + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_a2 <- lm(K6_T3 ~ ace10_4 * llm_mental + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
cat("\nOutcome model with interaction:\n"); print(summary(out_a2))
cat("\nWald test of ace10_4:llm_mental:\n")
print(coef(summary(out_a2))["ace10_4:llm_mental", , drop = FALSE])
set.seed(42)
res_a2 <- mediate(med_a2, out_a2, treat = "ace10_4", mediator = "llm_mental",
                  boot = TRUE, sims = SIMS)
cat("\nMediation (ACME reported separately for control/treated):\n")
print(summary(res_a2))
cat("\ntest.modmed-style comparison: d1 - d0 =", res_a2$d1 - res_a2$d0, "\n")

cat("\n\n============================================================\n")
cat("A2-2: interaction k6t1_13 x llm_mental (covariates incl. ace10_count)\n")
cat("============================================================\n")
med_a2b <- lm(llm_mental ~ k6t1_13 + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_a2b <- lm(K6_T3 ~ k6t1_13 * llm_mental + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
cat("\nOutcome model with interaction:\n"); print(summary(out_a2b))
cat("\nWald test of k6t1_13:llm_mental:\n")
print(coef(summary(out_a2b))["k6t1_13:llm_mental", , drop = FALSE])
set.seed(42)
res_a2b <- mediate(med_a2b, out_a2b, treat = "k6t1_13", mediator = "llm_mental",
                   boot = TRUE, sims = SIMS)
cat("\nMediation (ACME reported separately for control/treated):\n")
print(summary(res_a2b))
cat("\ntest.modmed-style comparison: d1 - d0 =", res_a2b$d1 - res_a2b$d0, "\n")

# ============================================================
# A3: continuous exposure (ACE-10 count, 0 vs 4)
# ============================================================
cat("\n\n============================================================\n")
cat("A3: ACE-10 count (continuous, contrast 0 vs 4) -> LLM mental -> K6_T3\n")
cat("============================================================\n")
med_a3 <- lm(llm_mental ~ ace10_count + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_a3 <- lm(K6_T3 ~ ace10_count + llm_mental + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
cat("\nMediator model:\n"); print(summary(med_a3))
cat("\nOutcome model:\n"); print(summary(out_a3))
set.seed(42)
res_a3 <- mediate(med_a3, out_a3, treat = "ace10_count", mediator = "llm_mental",
                  control.value = 0, treat.value = 4,
                  boot = TRUE, sims = SIMS)
cat("\nMediation (ACE count 0 vs 4):\n"); print(summary(res_a3))

# ============================================================
# A3b: continuous exposure, Model 1 (K6_T1 score, 0 vs 13)
# ============================================================
cat("\n\n============================================================\n")
cat("A3b: K6_T1 score (continuous, contrast 0 vs 13) -> LLM mental -> K6_T3\n")
cat("============================================================\n")
med_a3b <- lm(llm_mental ~ K6_T1 + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_a3b <- lm(K6_T3 ~ K6_T1 + llm_mental + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
cat("\nMediator model:\n"); print(summary(med_a3b))
cat("\nOutcome model:\n"); print(summary(out_a3b))
set.seed(42)
res_a3b <- mediate(med_a3b, out_a3b, treat = "K6_T1", mediator = "llm_mental",
                   control.value = 0, treat.value = 13,
                   boot = TRUE, sims = SIMS)
cat("\nMediation (K6_T1 0 vs 13):\n"); print(summary(res_a3b))
z <- qnorm(0.975)
a <- coef(summary(med_a3b))["K6_T1", ]; b <- coef(summary(out_a3b))["llm_mental", ]
cat(sprintf("\npath a per point: %.4f [%.4f, %.4f]\n", a[1], a[1] - z * a[2], a[1] + z * a[2]))
cat(sprintf("path a x13      : %.4f [%.4f, %.4f]\n", 13 * a[1], 13 * (a[1] - z * a[2]), 13 * (a[1] + z * a[2])))
cat(sprintf("path b          : %.4f [%.4f, %.4f]\n", b[1], b[1] - z * b[2], b[1] + z * b[2]))

# ============================================================
# A4: T2 K6-adjusted ACE model (reference values only)
# ============================================================
cat("\n\n============================================================\n")
cat("A4: ACE model additionally adjusted for K6_T2 (post-treatment\n")
cat("    variable measured concurrently with the mediator; shown for\n")
cat("    transparency, not interpretable under sequential ignorability)\n")
cat("============================================================\n")
d_cc2 <- d[complete.cases(d[, c(vars_needed, "K6_T2")]), ]
cat("Complete cases incl. K6_T2:", nrow(d_cc2), "\n")
med_a4 <- lm(llm_mental ~ ace10_4 + K6_T1 + K6_T2 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc2)
out_a4 <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + K6_T2 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc2)
cat("\nMediator model:\n"); print(summary(med_a4))
cat("\nOutcome model:\n"); print(summary(out_a4))
set.seed(42)
res_a4 <- mediate(med_a4, out_a4, treat = "ace10_4", mediator = "llm_mental",
                  boot = TRUE, sims = SIMS)
cat("\nMediation:\n"); print(summary(res_a4))

# ============================================================
# A5: medsens (quasi-Bayesian, as required by medsens)
# ============================================================
cat("\n\n============================================================\n")
cat("A5-1: medsens for the ACE main model (rho.by = 0.05)\n")
cat("============================================================\n")
med_m <- lm(llm_mental ~ ace10_4 + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_m <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
set.seed(42)
res_qb <- mediate(med_m, out_m, treat = "ace10_4", mediator = "llm_mental",
                  boot = FALSE, sims = 1000)
cat("\nQuasi-Bayesian mediation (for reference):\n"); print(summary(res_qb))
set.seed(42)
sens_ace <- medsens(res_qb, rho.by = 0.05, sims = 1000)
cat("\nmedsens summary:\n"); print(summary(sens_ace))

cat("\n\n============================================================\n")
cat("A5-2: medsens for the K6>=13 main model (rho.by = 0.05)\n")
cat("============================================================\n")
med_m2 <- lm(llm_mental ~ k6t1_13 + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_m2 <- lm(K6_T3 ~ k6t1_13 + llm_mental + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
set.seed(42)
res_qb2 <- mediate(med_m2, out_m2, treat = "k6t1_13", mediator = "llm_mental",
                   boot = FALSE, sims = 1000)
cat("\nQuasi-Bayesian mediation (for reference):\n"); print(summary(res_qb2))
set.seed(42)
sens_k6 <- medsens(res_qb2, rho.by = 0.05, sims = 1000)
cat("\nmedsens summary:\n"); print(summary(sens_k6))

# ============================================================
# Compact summary block
# ============================================================
cat("\n\n============================================================\n")
cat("SUMMARY (additional analyses)\n")
cat("============================================================\n")
fmt <- function(label, res, n) {
  cat(sprintf("%-52s N=%-6d ACME=%.4f [%.4f, %.4f] p=%.4f\n",
              label, n, res$d.avg, res$d.avg.ci[1], res$d.avg.ci[2], res$d.avg.p))
}
fmt("A1: ACE, K6_T1 excluded (total-pathway)", res_a1, nrow(d_cc))
fmt("A2-1: ACE x mediator interaction (avg ACME)", res_a2, nrow(d_cc))
cat(sprintf("%-52s interaction p=%.4f\n", "      Wald test ace10_4:llm_mental",
            coef(summary(out_a2))["ace10_4:llm_mental", "Pr(>|t|)"]))
fmt("A2-2: K6>=13 x mediator interaction (avg ACME)", res_a2b, nrow(d_cc))
cat(sprintf("%-52s interaction p=%.4f\n", "      Wald test k6t1_13:llm_mental",
            coef(summary(out_a2b))["k6t1_13:llm_mental", "Pr(>|t|)"]))
fmt("A3: ACE count 0 vs 4 (continuous exposure)", res_a3, nrow(d_cc))
fmt("A3b: K6_T1 score 0 vs 13 (continuous exposure)", res_a3b, nrow(d_cc))
fmt("A4: ACE, additionally adjusted for K6_T2", res_a4, nrow(d_cc2))
fmt("A5-1 ref: ACE main, quasi-Bayesian", res_qb, nrow(d_cc))
fmt("A5-2 ref: K6>=13 main, quasi-Bayesian", res_qb2, nrow(d_cc))

sink()
cat("\nAll results saved to analysis/additional_analyses_revision_results.txt\nDone!\n")

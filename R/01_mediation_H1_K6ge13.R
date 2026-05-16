# ============================================================
# Causal Mediation Analysis: K6>=13(T1) as exposure (Model 1)
#
# Exposure:  K6 >= 13 at T1 (binary)
# Mediator:  LLM emotional support frequency (T2, continuous 1-5)
# Outcome:   K6 at T3 (continuous, 0-24)
# Covariates: ACE-10 count (cont), age, sex, BMI,
#             education, smoking, alcohol, physical illness
#
# Subgroup: Male / Female
# ============================================================

library(mediation)
library(dplyr)

data_path <- file.path(getwd(), "data", "3wave_analysis.csv")
output_path <- file.path(getwd(), "analysis")

d <- read.csv(data_path, fileEncoding = "UTF-8-BOM")
cat("N =", nrow(d), "\n")

# ---- Prepare variables ----
d$k6t1_13 <- as.integer(d$K6_T1 >= 13)

vars_needed <- c("K6_T1", "K6_T3", "k6t1_13",
                 "ace10_count", "llm_mental", "llm_gen_max",
                 "age", "female", "bmi", "edu_high",
                 "smoking", "alcohol", "physical_illness")
d_cc <- d[complete.cases(d[, vars_needed]), ]
cat("Complete cases:", nrow(d_cc), "\n")
cat(sprintf("  K6 >= 13 at T1: %d (%.1f%%)\n", sum(d_cc$k6t1_13), mean(d_cc$k6t1_13)*100))
cat(sprintf("  Male: %d  Female: %d\n\n", sum(d_cc$female==0), sum(d_cc$female==1)))

SIMS <- 5000

# ============================================================
# MAIN: K6>=13(T1) -> LLM mental (continuous 1-5) -> K6_T3
# ============================================================
cat(strrep("=", 60), "\n")
cat("MAIN: K6>=13(T1) -> LLM mental freq (continuous) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_cc)))
cat(strrep("=", 60), "\n\n")

med_main <- lm(llm_mental ~ k6t1_13 + ace10_count + age + female + bmi +
                 edu_high + smoking + alcohol + physical_illness, data = d_cc)
out_main <- lm(K6_T3 ~ k6t1_13 + llm_mental + ace10_count + age + female + bmi +
                 edu_high + smoking + alcohol + physical_illness, data = d_cc)

cat("-- Path a: K6>=13(T1) -> LLM mental --\n")
print(summary(med_main)$coefficients["k6t1_13", ])
cat("\n-- Path b & c': K6_T3 ~ K6>=13 + LLM mental --\n")
print(summary(out_main)$coefficients[c("k6t1_13", "llm_mental", "ace10_count"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_main <- mediate(med_main, out_main, treat = "k6t1_13", mediator = "llm_mental",
                    boot = TRUE, sims = SIMS)
print(summary(res_main))

# ============================================================
# SENSITIVITY 1: K6>=13 -> LLM any use (binary) -> K6_T3
# ============================================================
d_cc$llm_any <- as.integer(d_cc$llm_mental >= 2)

cat("\n", strrep("=", 60), "\n")
cat("SENSITIVITY 1: K6>=13(T1) -> LLM any use (binary) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_cc)))
cat(strrep("=", 60), "\n\n")

med_s1 <- glm(llm_any ~ k6t1_13 + ace10_count + age + female + bmi +
                edu_high + smoking + alcohol + physical_illness,
              family = binomial(link = "logit"), data = d_cc)
out_s1 <- lm(K6_T3 ~ k6t1_13 + llm_any + ace10_count + age + female + bmi +
               edu_high + smoking + alcohol + physical_illness, data = d_cc)

cat("-- Path a --\n")
a_s1 <- summary(med_s1)$coefficients["k6t1_13", ]
cat(sprintf("  b = %.4f, OR = %.2f, p = %.4f\n", a_s1[1], exp(a_s1[1]), a_s1[4]))
cat("\n-- Path b & c' --\n")
print(summary(out_s1)$coefficients[c("k6t1_13", "llm_any", "ace10_count"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_s1 <- mediate(med_s1, out_s1, treat = "k6t1_13", mediator = "llm_any",
                  boot = TRUE, sims = SIMS)
print(summary(res_s1))

# ============================================================
# SENSITIVITY 2: Current AI users only (llm_gen_max >= 2)
# Current user = any of 9 use-purpose items reported >= "Rarely" at T2
# ============================================================
d_users <- d_cc[d_cc$llm_gen_max >= 2, ]

cat("\n", strrep("=", 60), "\n")
cat("SENSITIVITY 2: K6>=13(T1) -> LLM mental (AI users only) -> K6_T3\n")
cat(sprintf("N = %d, K6>=13: %d (%.1f%%)\n",
            nrow(d_users), sum(d_users$k6t1_13), mean(d_users$k6t1_13)*100))
cat(strrep("=", 60), "\n\n")

med_s2 <- lm(llm_mental ~ k6t1_13 + ace10_count + age + female + bmi +
               edu_high + smoking + alcohol + physical_illness, data = d_users)
out_s2 <- lm(K6_T3 ~ k6t1_13 + llm_mental + ace10_count + age + female + bmi +
               edu_high + smoking + alcohol + physical_illness, data = d_users)

cat("-- Path a --\n")
print(summary(med_s2)$coefficients["k6t1_13", ])
cat("\n-- Path b & c' --\n")
print(summary(out_s2)$coefficients[c("k6t1_13", "llm_mental", "ace10_count"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_s2 <- mediate(med_s2, out_s2, treat = "k6t1_13", mediator = "llm_mental",
                  boot = TRUE, sims = SIMS)
print(summary(res_s2))

# ============================================================
# SENSITIVITY 2B: LLM users only (N), binary mediator (llm_any)
# ============================================================
d_users$llm_any <- as.integer(d_users$llm_mental >= 2)

cat("\n", strrep("=", 60), "\n")
cat("SENSITIVITY 2B: K6>=13(T1) -> LLM any use (binary, AI users only) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_users)))
cat(strrep("=", 60), "\n\n")

med_s2b <- glm(llm_any ~ k6t1_13 + ace10_count + age + female + bmi +
                 edu_high + smoking + alcohol + physical_illness,
               family = binomial(link = "logit"), data = d_users)
out_s2b <- lm(K6_T3 ~ k6t1_13 + llm_any + ace10_count + age + female + bmi +
                edu_high + smoking + alcohol + physical_illness, data = d_users)

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_s2b <- mediate(med_s2b, out_s2b, treat = "k6t1_13", mediator = "llm_any",
                   boot = TRUE, sims = SIMS)
print(summary(res_s2b))

# ============================================================
# SUBGROUP: Male
# ============================================================
d_male <- d_cc[d_cc$female == 0, ]

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP MALE: K6>=13(T1) -> LLM mental freq -> K6_T3\n")
cat(sprintf("N = %d, K6>=13: %d (%.1f%%)\n",
            nrow(d_male), sum(d_male$k6t1_13), mean(d_male$k6t1_13)*100))
cat(strrep("=", 60), "\n\n")

med_m <- lm(llm_mental ~ k6t1_13 + ace10_count + age + bmi + edu_high +
              smoking + alcohol + physical_illness, data = d_male)
out_m <- lm(K6_T3 ~ k6t1_13 + llm_mental + ace10_count + age + bmi + edu_high +
              smoking + alcohol + physical_illness, data = d_male)

cat("-- Path a --\n")
print(summary(med_m)$coefficients["k6t1_13", ])
cat("\n-- Path b & c' --\n")
print(summary(out_m)$coefficients[c("k6t1_13", "llm_mental", "ace10_count"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_m <- mediate(med_m, out_m, treat = "k6t1_13", mediator = "llm_mental",
                 boot = TRUE, sims = SIMS)
print(summary(res_m))

# ============================================================
# SUBGROUP MALE BINARY: binary mediator (llm_any)
# ============================================================
d_male$llm_any <- as.integer(d_male$llm_mental >= 2)

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP MALE BINARY: K6>=13(T1) -> LLM any use (binary) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_male)))
cat(strrep("=", 60), "\n\n")

med_mb <- glm(llm_any ~ k6t1_13 + ace10_count + age + bmi + edu_high +
                smoking + alcohol + physical_illness,
              family = binomial(link = "logit"), data = d_male)
out_mb <- lm(K6_T3 ~ k6t1_13 + llm_any + ace10_count + age + bmi + edu_high +
               smoking + alcohol + physical_illness, data = d_male)

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_mb <- mediate(med_mb, out_mb, treat = "k6t1_13", mediator = "llm_any",
                  boot = TRUE, sims = SIMS)
print(summary(res_mb))

# ============================================================
# SUBGROUP: Female
# ============================================================
d_female <- d_cc[d_cc$female == 1, ]

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP FEMALE: K6>=13(T1) -> LLM mental freq -> K6_T3\n")
cat(sprintf("N = %d, K6>=13: %d (%.1f%%)\n",
            nrow(d_female), sum(d_female$k6t1_13), mean(d_female$k6t1_13)*100))
cat(strrep("=", 60), "\n\n")

med_f <- lm(llm_mental ~ k6t1_13 + ace10_count + age + bmi + edu_high +
              smoking + alcohol + physical_illness, data = d_female)
out_f <- lm(K6_T3 ~ k6t1_13 + llm_mental + ace10_count + age + bmi + edu_high +
              smoking + alcohol + physical_illness, data = d_female)

cat("-- Path a --\n")
print(summary(med_f)$coefficients["k6t1_13", ])
cat("\n-- Path b & c' --\n")
print(summary(out_f)$coefficients[c("k6t1_13", "llm_mental", "ace10_count"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_f <- mediate(med_f, out_f, treat = "k6t1_13", mediator = "llm_mental",
                 boot = TRUE, sims = SIMS)
print(summary(res_f))

# ============================================================
# SUBGROUP FEMALE BINARY: binary mediator (llm_any)
# ============================================================
d_female$llm_any <- as.integer(d_female$llm_mental >= 2)

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP FEMALE BINARY: K6>=13(T1) -> LLM any use (binary) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_female)))
cat(strrep("=", 60), "\n\n")

med_fb <- glm(llm_any ~ k6t1_13 + ace10_count + age + bmi + edu_high +
                smoking + alcohol + physical_illness,
              family = binomial(link = "logit"), data = d_female)
out_fb <- lm(K6_T3 ~ k6t1_13 + llm_any + ace10_count + age + bmi + edu_high +
               smoking + alcohol + physical_illness, data = d_female)

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_fb <- mediate(med_fb, out_fb, treat = "k6t1_13", mediator = "llm_any",
                  boot = TRUE, sims = SIMS)
print(summary(res_fb))

# ============================================================
# Summary
# ============================================================
cat("\n", strrep("=", 60), "\n")
cat("SUMMARY\n")
cat(strrep("=", 60), "\n\n")

fmt <- function(res, n, label) {
  cat(sprintf("%-45s N=%-5d  ACME=%.4f [%.4f, %.4f] p=%.4f  Prop=%.3f%%\n",
              label, n,
              res$d0, res$d0.ci[1], res$d0.ci[2], res$d0.p,
              res$n0 * 100))
}

fmt(res_main, nrow(d_cc),     "Main: K6>=13 (continuous mediator)")
fmt(res_s1,   nrow(d_cc),     "Sens1: K6>=13 (binary mediator)")
fmt(res_s2,   nrow(d_users),  "Sens2: K6>=13 (LLM users only, continuous)")
fmt(res_s2b,  nrow(d_users),  "Sens2B: K6>=13 (LLM users only, binary)")
fmt(res_m,    nrow(d_male),   "Subgroup: Male (K6>=13, continuous)")
fmt(res_mb,   nrow(d_male),   "Subgroup: Male BIN (K6>=13, binary)")
fmt(res_f,    nrow(d_female), "Subgroup: Female (K6>=13, continuous)")
fmt(res_fb,   nrow(d_female), "Subgroup: Female BIN (K6>=13, binary)")

# ============================================================
# Save
# ============================================================
sink(file.path(output_path, "mediation_k6exposure_13_results.txt"))
cat("Causal Mediation Analysis: K6>=13(T1) as exposure (Model 1)\n")
cat("Outcome: K6_T3 (continuous)\n")
cat("Covariates: ACE-10 count (cont), age, sex, BMI, edu, smoking, alcohol, physical illness\n")
cat(sprintf("Full sample N = %d\n\n", nrow(d_cc)))

cat("===== MAIN: K6>=13(T1) -> LLM mental (continuous) -> K6_T3 =====\n")
cat("\nMediator:\n"); print(summary(med_main))
cat("\nOutcome:\n"); print(summary(out_main))
cat("\nMediation:\n"); print(summary(res_main))

cat("\n\n===== SENSITIVITY 1: LLM any use (binary) =====\n")
cat("\nMediator:\n"); print(summary(med_s1))
cat("\nOutcome:\n"); print(summary(out_s1))
cat("\nMediation:\n"); print(summary(res_s1))

cat(sprintf("\n\n===== SENSITIVITY 2: LLM users only (N=%d) =====\n", nrow(d_users)))
cat("\nMediator:\n"); print(summary(med_s2))
cat("\nOutcome:\n"); print(summary(out_s2))
cat("\nMediation:\n"); print(summary(res_s2))

cat(sprintf("\n\n===== SENSITIVITY 2B: LLM users only, binary mediator (N=%d) =====\n", nrow(d_users)))
cat("\nMediator:\n"); print(summary(med_s2b))
cat("\nOutcome:\n"); print(summary(out_s2b))
cat("\nMediation:\n"); print(summary(res_s2b))

cat(sprintf("\n\n===== SUBGROUP: MALE (N=%d) =====\n", nrow(d_male)))
cat("\nMediator:\n"); print(summary(med_m))
cat("\nOutcome:\n"); print(summary(out_m))
cat("\nMediation:\n"); print(summary(res_m))

cat(sprintf("\n\n===== SUBGROUP: MALE BINARY (N=%d) =====\n", nrow(d_male)))
cat("\nMediator:\n"); print(summary(med_mb))
cat("\nOutcome:\n"); print(summary(out_mb))
cat("\nMediation:\n"); print(summary(res_mb))

cat(sprintf("\n\n===== SUBGROUP: FEMALE (N=%d) =====\n", nrow(d_female)))
cat("\nMediator:\n"); print(summary(med_f))
cat("\nOutcome:\n"); print(summary(out_f))
cat("\nMediation:\n"); print(summary(res_f))

cat(sprintf("\n\n===== SUBGROUP: FEMALE BINARY (N=%d) =====\n", nrow(d_female)))
cat("\nMediator:\n"); print(summary(med_fb))
cat("\nOutcome:\n"); print(summary(out_fb))
cat("\nMediation:\n"); print(summary(res_fb))
sink()

cat("\nAll results saved to analysis/mediation_k6exposure_13_results.txt\n")
cat("Done!\n")

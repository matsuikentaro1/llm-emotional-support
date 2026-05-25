# ============================================================
# Causal Mediation Analysis (3-wave design) — H2 ACE
#
# Exposure:  ACE-10 >= 4 (T1, binary, CDC-Kaiser 10-item)
# Mediator:  LLM emotional support frequency (T2)
# Outcome:   K6 at T3 (continuous, 0-24)
# Covariates: K6_T1, age, sex, education (3-cat),
#             household income (5-cat incl. unknown),
#             marital status (3-cat), employment (4-cat),
#             smoking, alcohol, physical illness, psychiatric illness (lifetime)
# ============================================================

library(mediation)
library(dplyr)
library(car)  # for VIF

data_path <- file.path(getwd(), "data", "3wave_analysis.csv")
output_path <- file.path(getwd(), "analysis")

d <- read.csv(data_path, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
cat("N =", nrow(d), "\n")

d$llm_any <- as.integer(d$llm_mental >= 2)

# Factor encoding with explicit reference levels
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
cat("Complete cases:", nrow(d_cc), "\n")
cat(sprintf("  ACE-10 >= 4 (T1): %d (%.1f%%)\n", sum(d_cc$ace10_4), mean(d_cc$ace10_4)*100))
cat(sprintf("  LLM any use:      %d (%.1f%%)\n", sum(d_cc$llm_any), mean(d_cc$llm_any)*100))
cat(sprintf("  Male: %d  Female: %d\n\n", sum(d_cc$female==0), sum(d_cc$female==1)))

# ============================================================
# Pre-flight VIF check (gate) — abort if any VIF > 5
# ============================================================
cat(strrep("=", 60), "\n")
cat("Pre-flight VIF check (gate: stop if any VIF > 5)\n")
cat(strrep("=", 60), "\n")
vif_model <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + age + female +
                  edu_3cat + income_5cat + marital_3cat + employment_4cat +
                  smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
vif_vals <- car::vif(vif_model)
print(vif_vals)
# vif() returns matrix for factor terms; use GVIF^(1/(2*Df)) as the comparable scale
vif_scalar <- if (is.matrix(vif_vals)) vif_vals[, "GVIF^(1/(2*Df))"]^2 else vif_vals
cat("\nScalar VIF (squared adjusted GVIF for factors):\n")
print(round(vif_scalar, 3))
if (any(vif_scalar > 5)) {
  stop("VIF > 5 detected in one or more covariates. Investigation required before continuing. ",
       "Offending variables: ",
       paste(names(vif_scalar)[vif_scalar > 5], collapse = ", "))
}
cat("\n>>> All VIFs <= 5. Proceeding to mediation analyses.\n\n")

SIMS <- 5000

# ============================================================
# MAIN: ACE-10>=4 (T1) -> LLM mental (continuous) -> K6_T3
# ============================================================
cat(strrep("=", 60), "\n")
cat("MAIN: ACE-10>=4 (T1) -> LLM mental freq (continuous) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_cc)))
cat(strrep("=", 60), "\n\n")

med_main <- lm(llm_mental ~ ace10_4 + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                 smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_main <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                 smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)

cat("-- Path a --\n")
print(summary(med_main)$coefficients["ace10_4", ])
cat("\n-- Path b & c' --\n")
print(summary(out_main)$coefficients[c("ace10_4", "llm_mental", "K6_T1"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_main <- mediate(med_main, out_main, treat = "ace10_4", mediator = "llm_mental",
                    boot = TRUE, sims = SIMS)
print(summary(res_main))

# ============================================================
# SENSITIVITY 1: ACE-10>=4 -> LLM any use (binary) -> K6_T3
# ============================================================
cat("\n", strrep("=", 60), "\n")
cat("SENSITIVITY 1: ACE-10>=4 -> LLM any use (binary) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_cc)))
cat(strrep("=", 60), "\n\n")

med_s1 <- glm(llm_any ~ ace10_4 + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness,
              family = binomial(link = "logit"), data = d_cc)
out_s1 <- lm(K6_T3 ~ ace10_4 + llm_any + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)

cat("-- Path a --\n")
a_s1 <- summary(med_s1)$coefficients["ace10_4", ]
cat(sprintf("  b = %.4f, OR = %.2f, p = %.4f\n", a_s1[1], exp(a_s1[1]), a_s1[4]))
cat("\n-- Path b & c' --\n")
print(summary(out_s1)$coefficients[c("ace10_4", "llm_any", "K6_T1"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_s1 <- mediate(med_s1, out_s1, treat = "ace10_4", mediator = "llm_any",
                  boot = TRUE, sims = SIMS)
print(summary(res_s1))

# ============================================================
# SENSITIVITY 2: Current AI users only (llm_gen_max >= 2)
# Current user = any of 9 use-purpose items reported >= "Rarely" at T2
# ============================================================
d_users <- d_cc[d_cc$llm_gen_max >= 2, ]

cat("\n", strrep("=", 60), "\n")
cat("SENSITIVITY 2: Current AI users only (llm_gen_max >= 2)\n")
cat(sprintf("N = %d, ACE-10>=4: %d (%.1f%%)\n",
            nrow(d_users), sum(d_users$ace10_4), mean(d_users$ace10_4)*100))
cat(strrep("=", 60), "\n\n")

med_users <- lm(llm_mental ~ ace10_4 + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                  smoking + alcohol + physical_illness + psychiatric_illness, data = d_users)
out_users <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + age + female +
                  edu_3cat + income_5cat + marital_3cat + employment_4cat +
                  smoking + alcohol + physical_illness + psychiatric_illness, data = d_users)

cat("-- Path a --\n")
print(summary(med_users)$coefficients["ace10_4", ])
cat("\n-- Path b & c' --\n")
print(summary(out_users)$coefficients[c("ace10_4", "llm_mental", "K6_T1"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_users <- mediate(med_users, out_users, treat = "ace10_4", mediator = "llm_mental",
                     boot = TRUE, sims = SIMS)
print(summary(res_users))

# ============================================================
# SENSITIVITY 2B: binary mediator (llm_any)
# ============================================================
d_users$llm_any <- as.integer(d_users$llm_mental >= 2)

cat("\n", strrep("=", 60), "\n")
cat("SENSITIVITY 2B: ace10_4 -> LLM any use (binary) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_users)))
cat(strrep("=", 60), "\n\n")

med_s2b <- glm(llm_any ~ ace10_4 + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                 smoking + alcohol + physical_illness + psychiatric_illness,
              family = binomial(link = "logit"), data = d_users)
out_s2b <- lm(K6_T3 ~ ace10_4 + llm_any + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                 smoking + alcohol + physical_illness + psychiatric_illness, data = d_users)

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_s2b <- mediate(med_s2b, out_s2b, treat = "ace10_4", mediator = "llm_any",
                  boot = TRUE, sims = SIMS)
print(summary(res_s2b))

# ============================================================
# SUBGROUP: Male
# ============================================================
d_male <- d_cc[d_cc$female == 0, ]

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP MALE: ACE-10>=4 -> LLM mental freq -> K6_T3\n")
cat(sprintf("N = %d, ACE-10>=4: %d (%.1f%%)\n",
            nrow(d_male), sum(d_male$ace10_4), mean(d_male$ace10_4)*100))
cat(strrep("=", 60), "\n\n")

med_m <- lm(llm_mental ~ ace10_4 + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_male)
out_m <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_male)

cat("-- Path a --\n")
print(summary(med_m)$coefficients["ace10_4", ])
cat("\n-- Path b & c' --\n")
print(summary(out_m)$coefficients[c("ace10_4", "llm_mental", "K6_T1"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_m <- mediate(med_m, out_m, treat = "ace10_4", mediator = "llm_mental",
                 boot = TRUE, sims = SIMS)
print(summary(res_m))

# ============================================================
# SUBGROUP MALE BINARY: binary mediator (llm_any)
# ============================================================
d_male$llm_any <- as.integer(d_male$llm_mental >= 2)

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP MALE BINARY: ace10_4 -> LLM any use (binary) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_male)))
cat(strrep("=", 60), "\n\n")

med_mb <- glm(llm_any ~ ace10_4 + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness,
              family = binomial(link = "logit"), data = d_male)
out_mb <- lm(K6_T3 ~ ace10_4 + llm_any + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness, data = d_male)

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_mb <- mediate(med_mb, out_mb, treat = "ace10_4", mediator = "llm_any",
                  boot = TRUE, sims = SIMS)
print(summary(res_mb))

# ============================================================
# SUBGROUP: Female
# ============================================================
d_female <- d_cc[d_cc$female == 1, ]

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP FEMALE: ACE-10>=4 -> LLM mental freq -> K6_T3\n")
cat(sprintf("N = %d, ACE-10>=4: %d (%.1f%%)\n",
            nrow(d_female), sum(d_female$ace10_4), mean(d_female$ace10_4)*100))
cat(strrep("=", 60), "\n\n")

med_f <- lm(llm_mental ~ ace10_4 + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_female)
out_f <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_female)

cat("-- Path a --\n")
print(summary(med_f)$coefficients["ace10_4", ])
cat("\n-- Path b & c' --\n")
print(summary(out_f)$coefficients[c("ace10_4", "llm_mental", "K6_T1"), ])

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_f <- mediate(med_f, out_f, treat = "ace10_4", mediator = "llm_mental",
                 boot = TRUE, sims = SIMS)
print(summary(res_f))

# ============================================================
# SUBGROUP FEMALE BINARY: binary mediator (llm_any)
# ============================================================
d_female$llm_any <- as.integer(d_female$llm_mental >= 2)

cat("\n", strrep("=", 60), "\n")
cat("SUBGROUP FEMALE BINARY: ace10_4 -> LLM any use (binary) -> K6_T3\n")
cat(sprintf("N = %d\n", nrow(d_female)))
cat(strrep("=", 60), "\n\n")

med_fb <- glm(llm_any ~ ace10_4 + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness,
              family = binomial(link = "logit"), data = d_female)
out_fb <- lm(K6_T3 ~ ace10_4 + llm_any + K6_T1 + age + edu_3cat + income_5cat + marital_3cat + employment_4cat +
                smoking + alcohol + physical_illness + psychiatric_illness, data = d_female)

cat(sprintf("\n-- Mediation (%d bootstrap) --\n", SIMS))
set.seed(42)
res_fb <- mediate(med_fb, out_fb, treat = "ace10_4", mediator = "llm_any",
                  boot = TRUE, sims = SIMS)
print(summary(res_fb))

# ============================================================
# Summary
# ============================================================
cat("\n", strrep("=", 60), "\n")
cat("SUMMARY (H2: ACE-10)\n")
cat(strrep("=", 60), "\n\n")

fmt <- function(res, n, label) {
  cat(sprintf("%-45s N=%-5d  ACME=%.4f [%.4f, %.4f] p=%.4f  Prop=%.3f%%\n",
              label, n,
              res$d0, res$d0.ci[1], res$d0.ci[2], res$d0.p,
              res$n0 * 100))
}

fmt(res_main,  nrow(d_cc),     "Main: ACE-10>=4 (continuous mediator)")
fmt(res_s1,    nrow(d_cc),     "Sens 1: ACE-10>=4 (binary mediator)")
fmt(res_users, nrow(d_users),  "Sens 2: ACE-10>=4 (LLM users only)")
fmt(res_s2b, nrow(d_users), "Sens 2: ACE-10>=4 (LLM users only), BINARY")
fmt(res_m,     nrow(d_male),   "Subgroup: Male")
fmt(res_mb, nrow(d_male), "Subgroup: Male, BINARY")
fmt(res_f,     nrow(d_female), "Subgroup: Female")
fmt(res_fb, nrow(d_female), "Subgroup: Female, BINARY")

# ============================================================
# Save
# ============================================================
sink(file.path(output_path, "mediation_3wave_results.txt"))
cat("Causal Mediation Analysis (3-wave) — H2 ACE-10\n")
cat("Main exposure: ACE-10 >= 4 (T1) | Outcome: K6_T3 (continuous)\n")
cat("Covariates: K6_T1, age, sex, education (3-cat), income (5-cat incl. unknown),\n")
cat("            marital (3-cat), employment (4-cat), smoking, alcohol, physical illness\n")
cat(sprintf("Full sample N = %d\n\n", nrow(d_cc)))

cat("===== MAIN: ACE-10>=4 -> LLM mental freq (continuous) =====\n")
cat("\nMediator:\n"); print(summary(med_main))
cat("\nOutcome:\n"); print(summary(out_main))
cat("\nMediation:\n"); print(summary(res_main))

cat("\n\n===== SENSITIVITY 1: ACE-10>=4 -> LLM any use (binary) =====\n")
cat("\nMediator:\n"); print(summary(med_s1))
cat("\nOutcome:\n"); print(summary(out_s1))
cat("\nMediation:\n"); print(summary(res_s1))

cat(sprintf("\n\n===== SENSITIVITY 2: LLM users only (N=%d) =====\n", nrow(d_users)))
cat("\nMediator:\n"); print(summary(med_users))
cat("\nOutcome:\n"); print(summary(out_users))
cat("\nMediation:\n"); print(summary(res_users))

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

cat("\nAll results saved to analysis/mediation_3wave_results.txt\n")
cat("Done!\n")

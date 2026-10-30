# ============================================================
# Table 1: Characteristics and clinical variables of the
#          study participants (3-wave design)
#
# Main analysis uses K6 >= 13 (H1) and ACE-10 >= 4 (H2) at T1.
# H1 sensitivity: K6 >= 5 (lower threshold).
# ============================================================

library(dplyr)

data_path <- file.path(getwd(), "data", "3wave_analysis.csv")
output_path <- file.path(getwd(), "analysis", "Table_1.csv")

d_all <- read.csv(data_path, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))

# ---- Restrict to complete cases (same as mediation analysis) ----
# Complete-cases definition: matches what mediation scripts use.

analysis_vars <- c("K6_T1", "K6_T3",
                   "ace10_4", "ace10_count",
                   "llm_mental", "llm_gen_max", "age", "female",
                   "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
                   "smoking", "alcohol", "physical_illness", "psychiatric_illness")
d <- d_all[complete.cases(d_all[, analysis_vars]), ]
N <- nrow(d)
cat(sprintf("Complete cases: %d / %d (%.1f%%)\n", N, nrow(d_all), N / nrow(d_all) * 100))

# ---- Formatters ----
f_ms <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
f_pct <- function(n) sprintf("%s (%.1f%%)", format(n, big.mark = ",", trim = TRUE), n / N * 100)
f_n <- function(x) sum(x, na.rm = TRUE)

# ---- Build table ----
rows <- list()
add <- function(char, val) rows[[length(rows) + 1]] <<- c(char, val)

# Demographics (T1)
add("Age, mean (SD), years",                    f_ms(d$age))
add("Sex, No. (%)",                             "")
add("  Male",                                   f_pct(f_n(d$female == 0)))
add("  Female",                                 f_pct(f_n(d$female == 1)))
add("Education, No. (%)",                       "")
add("  High school or below",                   f_pct(f_n(d$edu_3cat == "high_school")))
add("  Vocational/junior college",              f_pct(f_n(d$edu_3cat == "vocational")))
add("  University or graduate",                 f_pct(f_n(d$edu_3cat == "university")))
add("Household income, No. (%)",                "")
add("  <4 million JPY (low)",                   f_pct(f_n(d$income_5cat == "low")))
add("  4-8 million JPY (mid)",                  f_pct(f_n(d$income_5cat == "mid")))
add("  8-12 million JPY (mid-high)",            f_pct(f_n(d$income_5cat == "mid_high")))
add("  >=12 million JPY (high)",                f_pct(f_n(d$income_5cat == "high")))
add("  Undisclosed",                            f_pct(f_n(d$income_5cat == "unknown")))
add("Marital status, No. (%)",                  "")
add("  Married",                                f_pct(f_n(d$marital_3cat == "married")))
add("  Never married",                          f_pct(f_n(d$marital_3cat == "never")))
add("  Divorced or widowed",                    f_pct(f_n(d$marital_3cat == "separated")))
add("Employment status, No. (%)",               "")
add("  Regular employment",                     f_pct(f_n(d$employment_4cat == "regular")))
add("  Non-regular employment",                 f_pct(f_n(d$employment_4cat == "non_regular")))
add("  Self-employed/business owner",           f_pct(f_n(d$employment_4cat == "self_employed")))
add("  Not working",                            f_pct(f_n(d$employment_4cat == "not_working")))
add("Current smoker, No. (%)",                  f_pct(f_n(d$smoking == 1)))
add("Habitual alcohol intake, No. (%)",         f_pct(f_n(d$alcohol == 1)))
add("Physical illness^a, No. (%)",              f_pct(f_n(d$physical_illness == 1)))
add("Psychiatric illness^b, No. (%)",           f_pct(f_n(d$psychiatric_illness == 1)))

# ACE-10 (T1, H2 exposure)
add("Number of ACEs (ACE-10, T1), mean (SD)",   f_ms(d$ace10_count))
add("  ACE-10 >= 4, No. (%)",                   f_pct(f_n(d$ace10_4 == 1)))

# K6 at T1
add("K6 score at T1, mean (SD), points",        f_ms(d$K6_T1))
add("  K6 >= 5 at T1, No. (%)",                 f_pct(f_n(d$K6_T1 >= 5)))
add("  K6 >= 13 at T1, No. (%)",                f_pct(f_n(d$K6_T1 >= 13)))

# K6 at T3
add("K6 score at T3, mean (SD), points",        f_ms(d$K6_T3))
add("  K6 >= 5 at T3, No. (%)",                 f_pct(f_n(d$K6_T3 >= 5)))
add("  K6 >= 13 at T3, No. (%)",                f_pct(f_n(d$K6_T3 >= 13)))

# Mediator: Generative AI and LLM emotional support (T2, combined)
# Current user = llm_gen_max >= 2 (any of 9 use-frequency items >= "Rarely")
add("Generative AI and LLM use for emotional support^c, No. (%)", "")
add("  Non-user",                               f_pct(f_n(d$llm_gen_max == 1)))
add("  Current user, not for emotional support", f_pct(f_n(d$llm_gen_max >= 2 & d$llm_mental == 1)))
add("  Rarely",                                 f_pct(f_n(d$llm_gen_max >= 2 & d$llm_mental == 2)))
add("  A few times a month",                    f_pct(f_n(d$llm_gen_max >= 2 & d$llm_mental == 3)))
add("  A few times a week",                     f_pct(f_n(d$llm_gen_max >= 2 & d$llm_mental == 4)))
add("  Almost daily",                           f_pct(f_n(d$llm_gen_max >= 2 & d$llm_mental == 5)))

# Footnotes
add("", "")
add("^a Includes diabetes, angina/MI, stroke, CKD, chronic hepatitis/cirrhosis,", "")
add("  immune abnormalities, cancer, asthma, COPD, and chronic pain.", "")
add("^b Lifetime history of physician-diagnosed depression or other psychiatric disorder", "")
add("  (alcohol use disorder excluded; adjusted separately via alcohol intake variable).", "")
add("^c Current generative AI users were defined as participants who reported any use", "")
add("  (at least 'rarely') of generative AI for one or more of nine purposes assessed at T2.", "")
add("  Current users who did not use LLM for emotional support are shown as", "")
add("  'Current user, not for emotional support'.", "")
add("ACEs, adverse childhood experiences; ACE-10, CDC-Kaiser 10-item (T1);", "")
add("T1, JASTIS2025 (Feb-Mar 2025); T2, JACSIS2025 (Dec 2025", "")
add("-Feb 2026); T3, JASTIS2026 (Apr 2026).", "")

# ---- Save ----
tbl <- do.call(rbind, rows) |> as.data.frame(stringsAsFactors = FALSE)
colnames(tbl) <- c("Characteristic", sprintf("Total (n = %d)", N))
write.csv(tbl, file = output_path, row.names = FALSE)
cat("Table 1 saved to", output_path, "\n")

# ---- Also save as Excel ----
xlsx_path <- file.path(getwd(), "analysis", "Table_1.xlsx")
if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(tbl, file = xlsx_path, rowNames = FALSE)
  cat("Table 1 saved to", xlsx_path, "\n\n")
} else {
  cat("openxlsx not installed; skipping xlsx output\n\n")
}

# ---- Console preview ----
cat(sprintf("Table 1  Characteristics and clinical variables of the study participants (n = %d)\n\n", N))
for (i in 1:nrow(tbl)) {
  if (tbl[i, 2] != "") {
    cat(sprintf("%-50s %12s\n", tbl[i, 1], tbl[i, 2]))
  } else {
    cat(sprintf("%s\n", tbl[i, 1]))
  }
}

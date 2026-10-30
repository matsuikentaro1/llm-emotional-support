# medsens refinement: rho.by = 0.01 (same models as A5)
library(mediation)
d <- read.csv(file.path(getwd(), "data", "3wave_analysis.csv"),
              fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))
d$llm_any <- as.integer(d$llm_mental >= 2)
d$k6t1_13 <- as.integer(d$K6_T1 >= 13)
d$edu_3cat        <- factor(d$edu_3cat,        levels = c("high_school", "vocational", "university"))
d$income_5cat     <- factor(d$income_5cat,     levels = c("mid", "low", "mid_high", "high", "unknown"))
d$marital_3cat    <- factor(d$marital_3cat,    levels = c("married", "never", "separated"))
d$employment_4cat <- factor(d$employment_4cat, levels = c("regular", "non_regular", "self_employed", "not_working"))
vars_needed <- c("K6_T1", "K6_T3", "ace10_4", "ace10_count",
                 "llm_any", "llm_mental", "llm_gen_max", "age", "female",
                 "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
                 "smoking", "alcohol", "physical_illness", "psychiatric_illness")
d_cc <- d[complete.cases(d[, vars_needed]), ]

sink(file.path(getwd(), "analysis", "medsens_fine_results.txt"), split = TRUE)
cat("medsens with rho.by = 0.01, sims = 1000, seed 42\n")

med_m <- lm(llm_mental ~ ace10_4 + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_m <- lm(K6_T3 ~ ace10_4 + llm_mental + K6_T1 + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
              smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
set.seed(42)
res_qb <- mediate(med_m, out_m, treat = "ace10_4", mediator = "llm_mental", boot = FALSE, sims = 1000)
set.seed(42)
s1 <- medsens(res_qb, rho.by = 0.01, sims = 1000)
cat("\n===== ACE main model =====\n"); print(summary(s1))

med_m2 <- lm(llm_mental ~ k6t1_13 + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
out_m2 <- lm(K6_T3 ~ k6t1_13 + llm_mental + ace10_count + age + female + edu_3cat + income_5cat + marital_3cat + employment_4cat +
               smoking + alcohol + physical_illness + psychiatric_illness, data = d_cc)
set.seed(42)
res_qb2 <- mediate(med_m2, out_m2, treat = "k6t1_13", mediator = "llm_mental", boot = FALSE, sims = 1000)
set.seed(42)
s2 <- medsens(res_qb2, rho.by = 0.01, sims = 1000)
cat("\n===== K6>=13 main model =====\n"); print(summary(s2))
sink()
cat("Done!\n")

# Generate a small simulated dataset for code demonstration.
# The output has the same column names and value domains as the
# real analytical dataset but contains entirely synthetic data.

set.seed(42)
N <- 1200

rcat <- function(n, levels, probs = NULL) {
  sample(levels, n, replace = TRUE, prob = probs)
}
rint <- function(n, min, max) sample(min:max, n, replace = TRUE)
rbin <- function(n, p = 0.5) rbinom(n, 1, p)

is_user <- rbinom(N, 1, 0.3) == 1

d <- data.frame(
  ID              = seq_len(N),
  K6_T1           = pmin(24L, pmax(0L, rnbinom(N, size = 1.5, mu = 5))),
  K6_T2           = pmin(24L, pmax(0L, rnbinom(N, size = 1.5, mu = 4))),
  K6_T3           = pmin(24L, pmax(0L, rnbinom(N, size = 1.5, mu = 5))),
  llm_start       = rint(N, 1, 6),
  llm_mental      = ifelse(is_user, rint(N, 1, 5), 1L),
  ai_months       = rcat(N, c(0L, 2L, 8L, 18L, 30L)),
  stringsAsFactors = FALSE
)

# LLM use items: non-users answer 1 (never) to all items
for (i in 1:8) {
  d[[paste0("llm_use_", i)]] <- ifelse(is_user, rint(N, 1, 5), 1L)
}

d$llm_gen_sum <- rowSums(d[, c(paste0("llm_use_", 1:8), "llm_mental")])
d$llm_gen_max <- do.call(pmax, d[, c(paste0("llm_use_", 1:8), "llm_mental")])
d$llm_gen_any <- as.integer(d$llm_gen_max >= 2)

d$ace10_count        <- pmin(10L, rnbinom(N, size = 0.5, mu = 0.6))
d$ace10_4            <- as.integer(d$ace10_count >= 4)
d$acej_count         <- pmin(14L, rnbinom(N, size = 0.5, mu = 1.1))
d$acej4              <- as.integer(d$acej_count >= 4)
d$ucla_sum           <- rint(N, 3, 12)
d$ucla_high          <- as.integer(d$ucla_sum >= 9)
d$age                <- rint(N, 15, 84)
d$female             <- rbin(N, 0.5)

d$edu_3cat <- rcat(N, c("high_school", "vocational", "university"),
                   c(0.3, 0.2, 0.5))
d$edu_3cat[sample(N, 2)] <- NA  # small number of missing values

d$income_5cat     <- rcat(N, c("mid", "low", "mid_high", "high", "unknown"),
                          c(0.3, 0.2, 0.2, 0.15, 0.15))
d$marital_3cat    <- rcat(N, c("married", "never", "separated"),
                          c(0.6, 0.25, 0.15))
d$employment_4cat <- rcat(N, c("regular", "non_regular", "self_employed", "not_working"),
                          c(0.4, 0.2, 0.1, 0.3))

d$smoking             <- rbin(N, 0.2)
d$alcohol             <- rbin(N, 0.5)
d$physical_illness    <- rbin(N, 0.3)
d$psychiatric_illness <- rbin(N, 0.1)
d$t2_to_t3_days       <- rint(N, 22, 89)

col_order <- c("ID", "K6_T1", "K6_T2", "K6_T3",
               "llm_start", "llm_mental", "ai_months",
               "llm_gen_sum", "llm_gen_max", "llm_gen_any",
               paste0("llm_use_", 1:8),
               "ace10_count", "ace10_4", "acej_count", "acej4",
               "ucla_sum", "ucla_high",
               "age", "female",
               "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
               "smoking", "alcohol", "physical_illness", "psychiatric_illness",
               "t2_to_t3_days")
d <- d[, col_order]

out_path <- file.path(dirname(getwd()), "demo", "demo_data.csv")
if (!interactive()) out_path <- file.path("demo", "demo_data.csv")

write.csv(d, out_path, row.names = FALSE)
cat("Wrote", out_path, "(", nrow(d), "rows )\n")

# ------------------------------------------------------------
# Demo file for the representativeness comparison (script 07):
# T1 characteristics of a simulated eligible T1 sample, with an
# indicator of inclusion in the analytic sample. Rows with
# in_analytic = 1 are the participants in demo_data.csv.
# ------------------------------------------------------------
N_extra <- 1800
t1_vars <- c("age", "female", "K6_T1", "ace10_count", "ace10_4",
             "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
             "smoking", "alcohol", "physical_illness", "psychiatric_illness")

a1 <- data.frame(ID = d$ID, in_analytic = 1L, d[, t1_vars], stringsAsFactors = FALSE)

a0 <- data.frame(
  ID                  = N + seq_len(N_extra),
  in_analytic         = 0L,
  age                 = rint(N_extra, 15, 84),
  female              = rbin(N_extra, 0.55),
  K6_T1               = pmin(24L, pmax(0L, rnbinom(N_extra, size = 1.5, mu = 5.5))),
  ace10_count         = pmin(10L, rnbinom(N_extra, size = 0.5, mu = 0.65)),
  stringsAsFactors = FALSE
)
a0$ace10_4            <- as.integer(a0$ace10_count >= 4)
a0$edu_3cat           <- rcat(N_extra, c("high_school", "vocational", "university"),
                              c(0.32, 0.2, 0.48))
a0$edu_3cat[sample(N_extra, 20)] <- NA
a0$income_5cat        <- rcat(N_extra, c("mid", "low", "mid_high", "high", "unknown"),
                              c(0.28, 0.2, 0.18, 0.12, 0.22))
a0$marital_3cat       <- rcat(N_extra, c("married", "never", "separated"),
                              c(0.55, 0.3, 0.15))
a0$employment_4cat    <- rcat(N_extra, c("regular", "non_regular", "self_employed", "not_working"),
                              c(0.4, 0.22, 0.1, 0.28))
a0$smoking            <- rbin(N_extra, 0.18)
a0$alcohol            <- rbin(N_extra, 0.45)
a0$physical_illness   <- rbin(N_extra, 0.3)
a0$psychiatric_illness <- rbin(N_extra, 0.1)

a <- rbind(a1, a0[, names(a1)])
out_path2 <- sub("demo_data.csv", "demo_attrition_t1.csv", out_path, fixed = TRUE)
write.csv(a, out_path2, row.names = FALSE)
cat("Wrote", out_path2, "(", nrow(a), "rows )\n")

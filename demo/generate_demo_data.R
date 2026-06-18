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

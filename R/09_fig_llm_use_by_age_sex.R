# ============================================================
# Fig. 3: LLM use by age (4 groups) and sex
# Top row    : (A) Generative AI use by age | (B) Emotional support by age (1x4)
# Bottom row : (C) Generative AI use by sex | (D) Emotional support by sex (1x2)
# Font: Arial (embedded via cairo_pdf)
# ============================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# Source file is UTF-8; ensure en-dash (–) and ≥ render correctly on Windows.
if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_CTYPE", "Japanese_Japan.utf8")
}

if (.Platform$OS.type == "windows") {
  windowsFonts(Arial = windowsFont("Arial"))
}
base_family <- "Arial"

data_path    <- file.path(getwd(), "data", "3wave_analysis.csv")
fig_path     <- file.path(getwd(), "figures", "fig_llm_emotional_by_age_sex.pdf")
fig_path_png <- file.path(getwd(), "figures", "fig_llm_emotional_by_age_sex.png")

dir.create(file.path(getwd(), "figures"), showWarnings = FALSE)

d_all <- read.csv(data_path, fileEncoding = "UTF-8-BOM", na.strings = c("", "NA"))

# ---- Restrict to complete cases (same definition as Table1.R / mediation) ----
analysis_vars <- c("K6_T1", "K6_T3",
                   "ace10_4", "ace10_count",
                   "llm_mental", "llm_gen_max", "age", "female",
                   "edu_3cat", "income_5cat", "marital_3cat", "employment_4cat",
                   "smoking", "alcohol", "physical_illness", "psychiatric_illness")
d <- d_all[complete.cases(d_all[, analysis_vars]), ]
cat(sprintf("Complete cases: %d / %d (%.1f%%)\n",
            nrow(d), nrow(d_all), nrow(d) / nrow(d_all) * 100))

# ---- Variables ----
d$age_4cat <- cut(d$age, breaks = c(-Inf, 34, 49, 64, Inf),
                  labels = c("18-34", "35-49", "50-64", "65+"))
d$age_4cat <- factor(d$age_4cat, levels = c("18-34", "35-49", "50-64", "65+"))

d$sex_label <- factor(ifelse(d$female == 0, "Male", "Female"),
                      levels = c("Male", "Female"))

d$ai_user <- ifelse(d$llm_gen_max >= 2, "Current user", "Non-user")
d$ai_user <- factor(d$ai_user, levels = c("Non-user", "Current user"))

# ---- Build axis/strip labels with sample sizes ----
# (A)(C): denominator is the parent population in each stratum (all respondents).
# (B)(D): denominator is current AI users only, because non-users are excluded
#         from the emotional-support frequency distribution. Use a separate n
#         label so the displayed n always matches the actual bar denominator.
fmt_n <- function(x) format(as.integer(x), big.mark = ",")

n_sex <- table(d$sex_label)
n_age <- table(d$age_4cat)

ai_users_tmp <- d[d$llm_gen_max >= 2, ]
n_sex_ai <- table(ai_users_tmp$sex_label)
n_age_ai <- table(ai_users_tmp$age_4cat)

# Full-sample labels (for A, C)
sex_label_lookup <- c(
  "Male"   = sprintf("Male\n(n = %s)",   fmt_n(n_sex["Male"])),
  "Female" = sprintf("Female\n(n = %s)", fmt_n(n_sex["Female"]))
)

age_label_lookup <- c(
  "18-34" = sprintf("18–34 years\n(n = %s)",   fmt_n(n_age["18-34"])),
  "35-49" = sprintf("35–49 years\n(n = %s)",   fmt_n(n_age["35-49"])),
  "50-64" = sprintf("50–64 years\n(n = %s)",   fmt_n(n_age["50-64"])),
  "65+"   = sprintf("≥ 65 years\n(n = %s)",    fmt_n(n_age["65+"]))
)

# AI-users-only labels (for B, D)
sex_label_lookup_ai <- c(
  "Male"   = sprintf("Male\n(n = %s)",   fmt_n(n_sex_ai["Male"])),
  "Female" = sprintf("Female\n(n = %s)", fmt_n(n_sex_ai["Female"]))
)

age_label_lookup_ai <- c(
  "18-34" = sprintf("18–34 years\n(n = %s)",   fmt_n(n_age_ai["18-34"])),
  "35-49" = sprintf("35–49 years\n(n = %s)",   fmt_n(n_age_ai["35-49"])),
  "50-64" = sprintf("50–64 years\n(n = %s)",   fmt_n(n_age_ai["50-64"])),
  "65+"   = sprintf("≥ 65 years\n(n = %s)",    fmt_n(n_age_ai["65+"]))
)

d$sex_label_n <- factor(d$sex_label,
                        levels = names(sex_label_lookup),
                        labels = unname(sex_label_lookup))
d$age_4cat_n  <- factor(d$age_4cat,
                        levels = names(age_label_lookup),
                        labels = unname(age_label_lookup))

# ---- Palettes ----
ai_palette <- c(
  "Non-user"     = "#DEEBF7",
  "Current user" = "#3182BD"
)

cat_levels <- c("Never", "Rarely", "A few times a month",
                "A few times a week", "Almost daily")

blue_palette <- c(
  "Never"               = "#DEEBF7",
  "Rarely"              = "#9ECAE1",
  "A few times a month" = "#6BAED6",
  "A few times a week"  = "#3182BD",
  "Almost daily"        = "#08519C"
)

# ---- AI users subset (for B and D) ----
# Strip labels for (B)(D) use AI-users-only n, matching the bar denominator.
ai_users <- d %>% filter(llm_gen_max >= 2)
ai_users$llm_cat <- factor(ai_users$llm_mental,
                           levels = 1:5,
                           labels = cat_levels)

ai_users$sex_label_n <- factor(ai_users$sex_label,
                               levels = names(sex_label_lookup_ai),
                               labels = unname(sex_label_lookup_ai))
ai_users$age_4cat_n  <- factor(ai_users$age_4cat,
                               levels = names(age_label_lookup_ai),
                               labels = unname(age_label_lookup_ai))

# ============================================================
# (A) Generative AI use by age (4 stacked bars)
# ============================================================
plot_A <- d %>%
  filter(!is.na(age_4cat_n)) %>%
  group_by(age_4cat_n, ai_user) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age_4cat_n) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(label = sprintf("%.1f%%", pct))

p_A <- ggplot(plot_A, aes(x = age_4cat_n, y = pct, fill = ai_user)) +
  geom_bar(stat = "identity", width = 0.65, color = "white", linewidth = 0.3) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3.8, color = "black", family = base_family) +
  scale_fill_manual(values = ai_palette, name = NULL,
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = NULL, title = "(A) Generative AI use by age") +
  theme_minimal(base_size = 13, base_family = base_family) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11, family = base_family),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold", family = base_family,
                               lineheight = 0.9, hjust = 0.5),
    axis.text.y = element_text(size = 11, family = base_family),
    plot.title = element_text(size = 14, face = "bold", family = base_family),
    plot.margin = margin(10, 5, 10, 10)
  )

# ============================================================
# (B) Emotional support by age (1x4 facets)
# ============================================================
plot_B <- ai_users %>%
  filter(!is.na(age_4cat_n)) %>%
  group_by(age_4cat_n, llm_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age_4cat_n) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p_B <- ggplot(plot_B, aes(x = llm_cat, y = pct, fill = llm_cat)) +
  geom_bar(stat = "identity", width = 0.75, color = "grey60", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 3.0, family = base_family) +
  facet_wrap(~ age_4cat_n, nrow = 1) +
  scale_fill_manual(values = blue_palette, guide = "none") +
  scale_y_continuous(breaks = c(0, 25, 50, 75),
                     limits = c(0, 92),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(labels = c("Never", "Rarely", "Monthly", "Weekly", "Daily")) +
  labs(x = NULL, y = NULL,
       title = "(B) Use for emotional support among generative AI users, by age") +
  theme_minimal(base_size = 12, base_family = base_family) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 9.5, family = base_family),
    axis.text.y = element_text(size = 10, family = base_family),
    strip.text = element_text(size = 11, face = "bold", family = base_family,
                              lineheight = 0.9),
    plot.title = element_text(size = 14, face = "bold", family = base_family),
    plot.margin = margin(10, 10, 10, 5)
  )

# ============================================================
# (C) Generative AI use by sex (2 stacked bars)
# ============================================================
plot_C <- d %>%
  group_by(sex_label_n, ai_user) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sex_label_n) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(label = sprintf("%.1f%%", pct))

p_C <- ggplot(plot_C, aes(x = sex_label_n, y = pct, fill = ai_user)) +
  geom_bar(stat = "identity", width = 0.5, color = "white", linewidth = 0.3) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4.2, color = "black", family = base_family) +
  scale_fill_manual(values = ai_palette, name = NULL,
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = NULL, title = "(C) Generative AI use by sex") +
  theme_minimal(base_size = 13, base_family = base_family) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11, family = base_family),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold", family = base_family,
                               lineheight = 0.9, hjust = 0.5),
    axis.text.y = element_text(size = 11, family = base_family),
    plot.title = element_text(size = 14, face = "bold", family = base_family),
    plot.margin = margin(10, 5, 10, 10)
  )

# ============================================================
# (D) Emotional support by sex (1x2 facets)
# ============================================================
plot_D <- ai_users %>%
  group_by(sex_label_n, llm_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sex_label_n) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p_D <- ggplot(plot_D, aes(x = llm_cat, y = pct, fill = llm_cat)) +
  geom_bar(stat = "identity", width = 0.7, color = "grey60", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, size = 3.6, family = base_family) +
  facet_wrap(~ sex_label_n, nrow = 1) +
  scale_fill_manual(values = blue_palette, guide = "none") +
  scale_y_continuous(breaks = c(0, 25, 50, 75),
                     limits = c(0, 92),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(labels = c("Never", "Rarely", "Monthly", "Weekly", "Daily")) +
  labs(x = NULL, y = NULL,
       title = "(D) Use for emotional support among generative AI users, by sex") +
  theme_minimal(base_size = 12, base_family = base_family) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10.5, family = base_family),
    axis.text.y = element_text(size = 10.5, family = base_family),
    strip.text = element_text(size = 12, face = "bold", family = base_family,
                              lineheight = 0.9),
    plot.title = element_text(size = 14, face = "bold", family = base_family),
    plot.margin = margin(10, 10, 10, 5)
  )

# ============================================================
# Combine: top (age) over bottom (sex)
# Width ratio matches between rows so columns visually align.
# ============================================================
p_top <- p_A + p_B + plot_layout(widths = c(1, 2.4))
p_bot <- p_C + p_D + plot_layout(widths = c(1, 1.6))
p_combined <- p_top / p_bot

ggsave(fig_path,     p_combined, width = 16, height = 11, device = cairo_pdf)
ggsave(fig_path_png, p_combined, width = 16, height = 11, dpi = 300)

cat("Saved to:\n")
cat(" ", fig_path, "\n")
cat(" ", fig_path_png, "\n")

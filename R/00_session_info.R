# ============================================================
# Capture R version + package versions used for this analysis.
# Run this once after installing the required packages (or after
# running any analysis script) to produce analysis/sessionInfo.txt,
# which reviewers can use to reproduce the exact environment.
# ============================================================

output_path <- file.path(getwd(), "analysis")
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

pkgs <- c("mediation", "dplyr")
# Load each package so sessionInfo() records attached/loaded versions.
for (p in pkgs) {
  if (requireNamespace(p, quietly = TRUE)) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

sink(file.path(output_path, "sessionInfo.txt"))
cat("Session info captured on ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
cat(strrep("=", 60), "\n\n")
print(sessionInfo())
cat("\n\n", strrep("=", 60), "\n", sep = "")
cat("Package versions for this project:\n\n")
for (p in pkgs) {
  if (requireNamespace(p, quietly = TRUE)) {
    cat(sprintf("  %s: %s\n", p, as.character(packageVersion(p))))
  } else {
    cat(sprintf("  %s: NOT INSTALLED\n", p))
  }
}
sink()

cat("Wrote analysis/sessionInfo.txt\n")

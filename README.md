# llm-emotional-support

R code for the analyses in:

Matsui K, Nakagomi A, Hazumi M, Stickley A, Kuriyama K, Tabuchi T.
*Seeking comfort, finding distress: emotional-support use of large
language models among vulnerable adults.* (under review)

Individual-level data are not publicly shared due to ethical
restrictions; requests can be directed to the corresponding author.

## 1. System requirements

- **R** >= 4.3
- Required packages: `mediation`, `dplyr`, `car`
- Tested on: Windows 11 (R 4.3.2)
- No non-standard hardware required.

## 2. Installation

```r
install.packages(c("mediation", "dplyr", "car"))
```

Typical install time: < 5 minutes on a standard desktop.

## 3. Demo

A small simulated dataset (`demo/demo_data.csv`, N = 1,200) is included
for code demonstration. It has the same column structure as the real
data but contains entirely synthetic values.

To run the demo:

```bash
# 1. Copy the demo data into the expected location
mkdir data
cp demo/demo_data.csv data/3wave_analysis.csv

# 2. Run any analysis script
Rscript R/01_mediation_H1_K6ge13.R
```

**Expected output:** Console output showing sample sizes, VIF checks,
and mediation analysis results (ACME, ADE, total effect, proportion
mediated) for the full sample and sex-stratified subgroups. Results are
also saved to `analysis/`.

**Expected run time:** ~15–20 minutes for scripts 01/02 and ~50 minutes
for script 03 on a standard desktop (5,000 bootstrap simulations each).

## 4. Instructions for use

To run the analyses on the real data, place `3wave_analysis.csv` in
`data/` and execute the scripts in `R/` in numerical order:

| Script | Description |
|--------|-------------|
| `00_session_info.R` | Record R version and package versions |
| `01_mediation_H1_K6ge13.R` | Tables 2–3 / Suppl. Table 1: Model 1 (K6 >= 13), full sample + sex- and age-stratified + sensitivity analyses |
| `02_mediation_H2_ACE10.R` | Tables 2–3 / Suppl. Table 1: Model 2 (ACE-10 >= 4), full sample + sex- and age-stratified + sensitivity analyses |
| `03_mediation_general_LLM.R` | Suppl. Table 2: purpose-specific and composite LLM use mediators |

## License

MIT (see `LICENSE`).

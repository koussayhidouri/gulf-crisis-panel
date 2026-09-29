# =====================
# R Code for the Project
# =====================
# This R code provides a step-by-step framework for students to import data, estimate the models,
# and interpret the results. It assumes the data has been collected and saved as a CSV file.

# ============================================================================
# Project: Geopolitics, Oil Shock, and Macroeconomics - Panel Data Analysis
# ============================================================================
# TEAM PANEL SELECTION:
#   Advanced Net-Importers : Germany, France, Italy, Japan
#   Advanced Net-Exporters : United States, Canada
#   Emerging Net-Importers : Turkey, Poland, India, Thailand
#   Emerging Net-Exporters : Saudi Arabia, Russia
# Time Window: January 2023 – June 2026 (Apr–Jun 2026 = ARIMA forecasted)
# ============================================================================

# ============================================================================
# 1. INSTALL AND LOAD LIBRARIES
# ============================================================================
# Install packages if you don't have them
# install.packages(c("tidyverse", "plm", "lmtest", "stargazer", "ggplot2", "car", "forecast"))

library(tidyverse)   # For data manipulation and plotting
library(plm)         # For panel data econometrics
library(lmtest)      # For robust standard errors and tests
library(stargazer)   # For creating nice regression tables
library(ggplot2)     # For advanced plotting
library(car)         # For hypothesis tests like linearHypothesis
library(forecast)    # For ARIMA forecasting of Apr-Jun 2026 values

# ============================================================================
# 2. LOAD AND PREPARE THE DATA
# ============================================================================

# Load the dataset (replace 'your_data.csv' with the actual file path)
# The dataset should be in 'long' format with columns:
# country, date, group, oil_balance, cpi, ipi, brent, neer, policy_rate, vix
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

data_raw <- read.csv("data/panel_data.csv", stringsAsFactors = FALSE)

# Inspect the data structure
str(data_raw)
head(data_raw)

# Transform within country and chronological order. The supplied panel already
# contains the common monthly oil_pct_change series; retain it rather than
# recalculating a lag across country boundaries.
data_processed <- data_raw %>%
  mutate(date = as.Date(date)) %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    ln_cpi = log(cpi),
    ln_ipi = log(ipi),
    ln_neer = log(neer),
    d_ln_cpi = ln_cpi - dplyr::lag(ln_cpi),
    d_ln_ipi = ln_ipi - dplyr::lag(ln_ipi),
    d_ln_neer = ln_neer - dplyr::lag(ln_neer),
    # Crisis-period indicator starts in March 2026; Apr-Jun are forecast rows.
    post_crisis = ifelse(date >= as.Date("2026-03-01"), 1, 0)
  ) %>%
  ungroup() %>%
  filter(!is.na(d_ln_cpi), !is.na(d_ln_ipi), !is.na(d_ln_neer),
         !is.na(oil_pct_change))

# ============================================================================
# 2B. ARIMA FORECASTING FOR APR–JUN 2026
# ============================================================================
# The project instructions require ARIMA forecasting to generate Apr–Jun 2026 values.
# These values are already embedded in our dataset (is_forecast == TRUE).
# Below we document the ARIMA procedure applied to Brent crude for transparency.

# Extract observed Brent series (up to March 2026) for one country (Brent is global)
brent_observed <- data_processed %>%
  filter(country == "Germany", date < as.Date("2026-04-01")) %>%
  arrange(date) %>%
  pull(brent)

brent_ts <- ts(brent_observed, start = c(2023, 1), frequency = 12)

# Fit ARIMA model automatically
arima_fit <- auto.arima(brent_ts)
cat("\n--- ARIMA Model for Brent Crude ---\n")
print(summary(arima_fit))

# Forecast 3 months ahead (Apr, May, Jun 2026)
arima_fcst <- forecast(arima_fit, h = 3)
cat("\nARIMA Forecast for Apr–Jun 2026 (Brent, USD/bbl):\n")
print(arima_fcst)

# ============================================================================
# 3. EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================================

# Plot the trend of oil prices over time
ggplot(data_processed, aes(x = date, y = brent)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_vline(xintercept = as.Date("2026-02-27"), linetype = "dashed", color = "blue") +
  annotate("text", x = as.Date("2026-02-27"), y = max(data_processed$brent), label = "Crisis Start", hjust = -0.1) +
  labs(title = "Brent Crude Oil Price: Pre and Post 2026 Gulf Crisis",
       x = "Date", y = "USD per Barrel") +
  theme_minimal()

ggsave("results/figures/brent-oil-price.png", width = 10, height = 5, dpi = 150)

# Plot the average CPI trend for different country groups
# (You need to create a 'group' column first, e.g., 'Advanced_Importer', 'Emerging_Importer', etc.)
# Assuming you have a 'group' column:
ggplot(data_processed, aes(x = date, y = ln_cpi, color = group)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  geom_vline(xintercept = as.Date("2026-02-27"), linetype = "dashed") +
  labs(title = "Log CPI Trends by Country Group",
       x = "Date", y = "Log(CPI)") +
  theme_minimal()

ggsave("results/figures/cpi-log-trends-by-group.png", width = 12, height = 5, dpi = 150)

# Additional plot: Log IPI trends by group
ggplot(data_processed, aes(x = date, y = ln_ipi, color = group)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  geom_vline(xintercept = as.Date("2026-02-27"), linetype = "dashed") +
  labs(title = "Log Industrial Production Trends by Country Group",
       x = "Date", y = "Log(IPI)") +
  theme_minimal()

ggsave("results/figures/ipi-log-trends-by-group.png", width = 12, height = 5, dpi = 150)

# Additional plot: Monthly Oil Shock over time
brent_unique <- data_processed %>% select(date, oil_pct_change) %>% distinct()
ggplot(brent_unique, aes(x = date, y = oil_pct_change, fill = oil_pct_change > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
  geom_vline(xintercept = as.Date("2026-02-27"), linetype = "dashed") +
  labs(title = "Monthly % Change in Brent Crude Oil Price (OilShock_t)",
       x = "Date", y = "% Change") +
  theme_minimal()

ggsave("results/figures/monthly-oil-price-change.png", width = 10, height = 4, dpi = 150)

# Descriptive statistics
cat("\n--- Descriptive Statistics ---\n")
desc_data <- data_processed %>% select(ln_cpi, ln_ipi, oil_pct_change, ln_neer, policy_rate, vix)
print(summary(desc_data))

# ============================================================================
# 4. PANEL DATA REGRESSION ANALYSIS
# ============================================================================

# First, declare the data as a panel data frame
# Use 'country' as the individual index and 'date' as the time index
pdata <- pdata.frame(data_processed, index = c("country", "date"))

# ----------------------------------------------------------------------------
# Model 1: Pooled OLS (POLS)
# This model ignores the panel structure.
# Equation: d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis
# ----------------------------------------------------------------------------
pols_model <- plm(d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                  data = pdata,
                  model = "pooling")

# ----------------------------------------------------------------------------
# Model 2: Fixed Effects (FE) Model
# This model allows each country to have its own intercept, controlling for
# time-invariant unobserved heterogeneity.
# Equation: d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis + country-specific intercepts
# ----------------------------------------------------------------------------
fe_model <- plm(d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                data = pdata,
                model = "within")  # 'within' is the argument for Fixed Effects

# ----------------------------------------------------------------------------
# Model 3: Random Effects (RE) Model
# This model assumes that country-specific effects are random and uncorrelated with the regressors.
# Equation: d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis
# ----------------------------------------------------------------------------
re_model <- plm(d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                data = pdata,
                model = "random")

# --- Repeat all three models for IPI (Industrial Production) ---
pols_model_ipi <- plm(d_ln_ipi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                      data = pdata,
                      model = "pooling")

fe_model_ipi <- plm(d_ln_ipi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                    data = pdata,
                    model = "within")

re_model_ipi <- plm(d_ln_ipi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                    data = pdata,
                    model = "random")

# Print summaries
cat("\n========== CPI MODELS ==========\n")
print(summary(pols_model))
print(summary(fe_model))
print(summary(re_model))

cat("\n========== IPI MODELS ==========\n")
print(summary(pols_model_ipi))
print(summary(fe_model_ipi))
print(summary(re_model_ipi))

# Regression tables (CPI)
stargazer(pols_model, fe_model, re_model,
          title         = "Table 1 — Panel Regression: Dependent Variable = Δ ln(CPI)",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"),
          dep.var.labels = "Δ ln(CPI)",
          covariate.labels = c("Oil Shock (%Δ Brent)", "Δ ln(NEER)",
                               "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          type = "text",
          out  = "results/tables/cpi-models.txt")

# Regression tables (IPI)
stargazer(pols_model_ipi, fe_model_ipi, re_model_ipi,
          title         = "Table 2 — Panel Regression: Dependent Variable = Δ ln(IPI)",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"),
          dep.var.labels = "Δ ln(IPI)",
          covariate.labels = c("Oil Shock (%Δ Brent)", "Δ ln(NEER)",
                               "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          type = "text",
          out  = "results/tables/ipi-models.txt")

# ============================================================================
# 5. DIAGNOSTIC TESTS AND ROBUST STANDARD ERRORS
# ============================================================================

# 1. Hausman Test: To choose between Fixed Effects and Random Effects
# Null Hypothesis (H0): RE is consistent and efficient. FE is consistent.
# Alternative (H1): RE is inconsistent. FE is consistent.
hausman_test <- phtest(fe_model, re_model)
print(hausman_test)
# If p-value < 0.05, reject H0. The FE model is preferred.

hausman_test_ipi <- phtest(fe_model_ipi, re_model_ipi)
print(hausman_test_ipi)

# 2. Breusch-Pagan Test for Random Effects: To test if Pooled OLS is sufficient vs. RE.
# Null Hypothesis: Pooled OLS is sufficient (no random effects).
bp_test <- plmtest(pols_model, effect = "individual", type = "bp")
print(bp_test)
# If p-value < 0.05, reject H0. RE model is better than POLS.

bp_test_ipi <- plmtest(pols_model_ipi, effect = "individual", type = "bp")
print(bp_test_ipi)

# 3. Robust Standard Errors
# We will use robust standard errors to correct for heteroskedasticity and autocorrelation.
# Use 'vcovHC' for heteroskedasticity-consistent (HC) and 'vcovSCC' for double-clustered
# (time and country) standard errors.

# For Fixed Effects:
coeftest(fe_model, vcov = vcovHC(fe_model, method = "arellano", type = "HC3"))  # Arellano (1987) robust SE
# For Random Effects:
coeftest(re_model, vcov = vcovHC(re_model, method = "arellano", type = "HC3"))
# For Pooled OLS:
coeftest(pols_model, vcov = vcovHC(pols_model, type = "HC3"))

# Same for IPI
cat("\n--- Robust SEs: FE IPI ---\n")
coeftest(fe_model_ipi, vcov = vcovHC(fe_model_ipi, method = "arellano", type = "HC3"))

# ============================================================================
# 6. HETEROGENEITY ANALYSIS: OIL-IMPORTERS VS. EXPORTERS
# ============================================================================

# Create a subset for oil importers (e.g., based on a column 'oil_balance' = 'importer')
importers_data <- pdata.frame(
  data_processed %>% filter(oil_balance == "importer"),
  index = c("country", "date")
)
exporters_data <- pdata.frame(
  data_processed %>% filter(oil_balance == "exporter"),
  index = c("country", "date")
)

# Run FE models for each subgroup
fe_importers <- plm(d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                    data = importers_data, model = "within")
fe_exporters <- plm(d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                    data = exporters_data, model = "within")

# Display the results side-by-side
stargazer(fe_importers, fe_exporters,
          title = "Fixed Effects Results: Oil Importers vs. Exporters",
          column.labels = c("Importers", "Exporters"),
          type = "text",
          keep.stat = c("n", "rsq"))

# Same heterogeneity analysis for IPI
fe_importers_ipi <- plm(d_ln_ipi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                        data = importers_data, model = "within")
fe_exporters_ipi <- plm(d_ln_ipi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                        data = exporters_data, model = "within")

stargazer(fe_importers_ipi, fe_exporters_ipi,
          title = "Fixed Effects IPI Results: Oil Importers vs. Exporters",
          column.labels = c("Importers", "Exporters"),
          type = "text",
          keep.stat = c("n", "rsq"))

# Advanced vs. Emerging split
advanced_data <- pdata.frame(
  data_processed %>% filter(grepl("Advanced", group)),
  index = c("country", "date")
)
emerging_data <- pdata.frame(
  data_processed %>% filter(grepl("Emerging", group)),
  index = c("country", "date")
)

fe_advanced <- plm(d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                   data = advanced_data, model = "within")
fe_emerging  <- plm(d_ln_cpi ~ oil_pct_change + d_ln_neer + policy_rate + vix + post_crisis,
                    data = emerging_data, model = "within")

stargazer(fe_advanced, fe_emerging,
          title = "Fixed Effects CPI: Advanced vs. Emerging Economies",
          column.labels = c("Advanced", "Emerging"),
          type = "text",
          keep.stat = c("n", "rsq"))

# ============================================================================
# BONUS: INTERACTION TERM – Importer × Oil Shock (advanced model for bonus points)
# ============================================================================
data_interact <- data_processed %>%
  mutate(
    importer_dummy = ifelse(oil_balance == "importer", 1, 0),
    oil_x_importer = oil_pct_change * importer_dummy   # interaction term
  )

pdata_int <- pdata.frame(data_interact, index = c("country", "date"))

fe_interact <- plm(
  d_ln_cpi ~ oil_pct_change + oil_x_importer + d_ln_neer + policy_rate + vix + post_crisis,
  data  = pdata_int,
  model = "within"
)

cat("\n--- BONUS: Interaction Model (Importer × OilShock) ---\n")
print(summary(fe_interact))
coeftest(fe_interact, vcov = vcovHC(fe_interact, method = "arellano", type = "HC3"))

# ============================================================================
# 7. INTERPRETATION AND CONCLUSION
# ============================================================================

# Students will summarize the findings:
# - Compare the magnitude and significance of the oil shock coefficient (oil_pct_change) across POLS, FE, and RE.
# - Based on Hausman and BP tests, justify which model is preferred.
# - Analyze the heterogeneity results: Are importers more adversely affected than exporters?
# - Discuss the economic significance of the results in the context of the 2026 Gulf War crisis report.
# - Acknowledge limitations (e.g., short time frame, potential endogeneity).

cat("\n\nAll outputs generated. Key files produced:\n")
cat("  data/panel_data.csv                          - the full panel dataset\n")
cat("  results/tables/cpi-models.txt                - CPI regression comparison\n")
cat("  results/tables/ipi-models.txt                - IPI regression comparison\n")
cat("  results/figures/brent-oil-price.png          - Brent oil price chart\n")
cat("  results/figures/cpi-log-trends-by-group.png  - CPI trends by group\n")
cat("  results/figures/ipi-log-trends-by-group.png  - IPI trends by group\n")
cat("  results/figures/monthly-oil-price-change.png - monthly oil price change\n")

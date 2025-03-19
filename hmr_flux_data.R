# Install and load the HMR package (run install only if needed)
install.packages("HMR")  # Uncomment this line if HMR is not installed
library(HMR)
installed.packages()["HMR", ]

# Load the dataset from a CSV file (update the filename as needed)
data <- read.csv("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/z_other/2023_CC4R_Exp.A gs_HMR Input.csv")

# Inspect the first few rows of the dataset to verify the structure
head(data)

# Ensure the column names match the expected format
colnames(data)  # Expected: "Series", "V", "A", "Time", "Concentration"

# --- Step 1: Estimate gas flux using HMR.fit() ---
# This function fits different models (Linear, Quadratic, Nonlinear HMR) to your data 
# and selects the best one based on statistical criteria.

flux_result <- HMR.fit(data$Time, data$Concentration)

# Print model details
summary(flux_result)

# ---- Interpretation of summary(flux_result) ----
# - Displays the estimated gas flux (e.g., µg/m²/h)
# - Shows which model was chosen: 
#   * LR (Linear Regression): Assumes constant gas flux
#   * QR (Quadratic Regression): Captures slight curvature in gas accumulation
#   * HMR (Nonlinear): Accounts for gas diffusion limitations
# - Confidence intervals indicate how precise the flux estimate is.

# --- Step 2: Visualize the gas flux estimation ---
# Generates a plot of the measured gas concentrations vs. time
# The best-fit model curve is overlaid to show how well it matches the data.

plot(flux_result)

# ---- Interpretation of plot(flux_result) ----
# - The black dots represent observed gas concentrations at different time points.
# - The curve represents the best-fitting model (Linear, Quadratic, or Nonlinear).
# - A good model should closely follow the pattern of the data.

# --- Step 3: Compare Different Flux Models ---
# This function runs different regression models (Linear, Quadratic, and HMR)
# and ranks them based on statistical performance.

model_comparison <- HMR.compare(data$Time, data$Concentration)

# Print model comparison results
print(model_comparison)

# ---- Interpretation of HMR.compare() ----
# - Displays the flux estimates from each model (LR, QR, HMR).
# - The best model is typically the one with the **lowest residual error** and best fit.
# - If LR and HMR give very different values, it suggests nonlinearity in flux behavior.

# --- Step 4: Process Multiple Chamber Measurements at Once ---
# If the dataset includes multiple chamber series (e.g., different locations/treatments),
# this function automatically processes them.

batch_results <- HMR.batch(data)

# Print batch results
print(batch_results)

# Save the batch results to a CSV file for further analysis
write.csv(batch_results, "flux_results.csv", row.names = FALSE)

# ---- Interpretation of HMR.batch() results ----
# - Returns a table with flux estimates for each chamber/series.
# - Useful for large datasets where many flux calculations are needed.
# - The output file ("flux_results.csv") contains results for easy analysis.

# --- Step 5: Manually Select the Best Model (if needed) ---
# If the automatic selection does not pick the preferred model, you can override it.

best_model <- HMR.select(data$Time, data$Concentration)

# Print manually selected model details
print(best_model)

# ---- Interpretation of HMR.select() ----
# - Allows manual selection of the preferred model (LR, QR, or HMR).
# - Useful when the automatic model selection does not align with expert judgment.

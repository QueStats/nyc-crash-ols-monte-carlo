# =============================================================================
# run_file.R
# Purpose: Load and clean data, build model matrix, fit OLS, run simulation.
# Usage:   Rscript run_file.r <seed>
#          e.g.  Rscript run_file.R 1
# Output:  results/run_results_<seed>.rds
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
seed <- if (length(args) >= 1) as.integer(args[1]) else 1
set.seed(seed)

# ---- 0. Output directory ----------------------------------------------------
dir.create("results", showWarnings = FALSE)

# =============================================================================
# SECTION 1 – Data import & cleaning
# =============================================================================

crash <- read.csv("~/Downloads/Motor_Vehicle_Collisions_-_Crashes.csv",
                  stringsAsFactors = FALSE)

# Retain only the six columns needed for analysis
crash_sub <- crash[, c(
  "CRASH.TIME",
  "BOROUGH",
  "NUMBER.OF.PERSONS.INJURED",
  "CONTRIBUTING.FACTOR.VEHICLE.1",
  "VEHICLE.TYPE.CODE.1",
  "VEHICLE.TYPE.CODE.2"
)]

# Remove rows with missing / blank values in key fields
crash_sub <- crash_sub[!is.na(crash_sub$NUMBER.OF.PERSONS.INJURED), ]
crash_sub <- crash_sub[crash_sub$BOROUGH != "" &
                         !is.na(crash_sub$BOROUGH), ]
crash_sub <- crash_sub[crash_sub$CRASH.TIME != "" &
                         !is.na(crash_sub$CRASH.TIME), ]
crash_sub <- crash_sub[crash_sub$CONTRIBUTING.FACTOR.VEHICLE.1 != "" &
                         !is.na(crash_sub$CONTRIBUTING.FACTOR.VEHICLE.1), ]
crash_sub <- crash_sub[crash_sub$VEHICLE.TYPE.CODE.1 != "" &
                         !is.na(crash_sub$VEHICLE.TYPE.CODE.1), ]

# Random subsample of 20,000 crashes (seed controls reproducibility)
n_keep     <- 20000
keep_index <- sample(seq_len(nrow(crash_sub)), n_keep)
crash_sub  <- crash_sub[keep_index, ]

# ---- Summary statistics for Table 1 ----------------------------------------
table1 <- data.frame(
  Item  = c("Original observations", "After cleaning", "Analysis sample",
            "Response variable", "Predictors (incl. intercept)"),
  Value = c(nrow(crash), nrow(crash[
    !is.na(crash$NUMBER.OF.PERSONS.INJURED) &
      crash$BOROUGH != "" & !is.na(crash$BOROUGH), ]),
    n_keep,
    "NUMBER.OF.PERSONS.INJURED",
    "15")
)

# =============================================================================
# SECTION 2 – Response and predictor construction
# =============================================================================

y    <- crash_sub$NUMBER.OF.PERSONS.INJURED
hour <- as.numeric(sub(":.*", "", crash_sub$CRASH.TIME))

# Binary predictors
night              <- ifelse(hour >= 21 | hour <= 5, 1, 0)
multi_vehicle      <- ifelse(crash_sub$VEHICLE.TYPE.CODE.2 != "", 1, 0)
borough_brooklyn   <- ifelse(crash_sub$BOROUGH == "BROOKLYN",  1, 0)
borough_manhattan  <- ifelse(crash_sub$BOROUGH == "MANHATTAN", 1, 0)
borough_queens     <- ifelse(crash_sub$BOROUGH == "QUEENS",    1, 0)
borough_bronx      <- ifelse(crash_sub$BOROUGH == "BRONX",     1, 0)

factor1             <- crash_sub$CONTRIBUTING.FACTOR.VEHICLE.1
factor_inattention  <- ifelse(factor1 == "Driver Inattention/Distraction",  1, 0)
factor_following    <- ifelse(factor1 == "Following Too Closely",            1, 0)
factor_yield        <- ifelse(factor1 == "Failure to Yield Right-of-Way",   1, 0)
factor_unspecified  <- ifelse(factor1 == "Unspecified",                      1, 0)

veh1              <- toupper(crash_sub$VEHICLE.TYPE.CODE.1)
vehicle_sedan     <- ifelse(grepl("SEDAN", veh1), 1, 0)
vehicle_suv       <- ifelse(grepl("SPORT|SUBN|UTILITY|SUV", veh1), 1, 0)
vehicle_pickup    <- ifelse(grepl("PICK|P/U", veh1), 1, 0)
vehicle_motorcycle <- ifelse(grepl("MOTORCYCLE|MOTORBIKE|MOPED", veh1), 1, 0)

# =============================================================================
# SECTION 3 – Design matrix
# =============================================================================

X <- cbind(
  Intercept          = 1,
  Night              = night,
  MultiVehicle       = multi_vehicle,
  Brooklyn           = borough_brooklyn,
  Manhattan          = borough_manhattan,
  Queens             = borough_queens,
  Bronx              = borough_bronx,
  Inattention        = factor_inattention,
  FollowingTooClosely = factor_following,
  FailureToYield     = factor_yield,
  UnspecifiedFactor  = factor_unspecified,
  Sedan              = vehicle_sedan,
  SUV                = vehicle_suv,
  Pickup             = vehicle_pickup,
  Motorcycle         = vehicle_motorcycle
)

# Remove incomplete rows and zero-variance columns
keep_rows <- complete.cases(X, y)
X <- X[keep_rows, , drop = FALSE]
y <- y[keep_rows]
keep_cols <- c(TRUE, apply(X[, -1, drop = FALSE], 2, var) > 0)
X <- X[, keep_cols, drop = FALSE]

# Borough labels vector (used in EDA plots in out_file.r)
borough_labels <- rep("STATEN ISLAND", nrow(X))
if ("Brooklyn"  %in% colnames(X)) borough_labels[X[, "Brooklyn"]  == 1] <- "BROOKLYN"
if ("Manhattan" %in% colnames(X)) borough_labels[X[, "Manhattan"] == 1] <- "MANHATTAN"
if ("Queens"    %in% colnames(X)) borough_labels[X[, "Queens"]    == 1] <- "QUEENS"
if ("Bronx"     %in% colnames(X)) borough_labels[X[, "Bronx"]     == 1] <- "BRONX"

# =============================================================================
# SECTION 4 – OLS estimation on real data (Table 2 / Figure 5)
# =============================================================================

beta_hat  <- qr.solve(X, y)
y_hat     <- as.vector(X %*% beta_hat)
residuals <- y - y_hat

# Residual standard deviation (used as sigma in simulation)
sigma_hat <- sqrt(mean(residuals^2))

# R-squared and adjusted R-squared
rss         <- sum(residuals^2)
tss         <- sum((y - mean(y))^2)
r_squared   <- 1 - rss / tss
n_obs       <- nrow(X)
p_cols      <- ncol(X)
adj_r_squared <- 1 - (rss / (n_obs - p_cols)) / (tss / (n_obs - 1))

# =============================================================================
# SECTION 5 – Inference on real data (Table 3)
# =============================================================================

# OLS standard errors from (X'X)^{-1} * sigma^2  (unbiased sigma^2)
sigma2_unbiased <- rss / (n_obs - p_cols)
XtX_inv         <- solve(t(X) %*% X)
se_beta         <- sqrt(diag(XtX_inv) * sigma2_unbiased)

t_stats  <- beta_hat / se_beta
p_values <- 2 * pt(abs(t_stats), df = n_obs - p_cols, lower.tail = FALSE)
ci_lower <- beta_hat - qt(0.975, df = n_obs - p_cols) * se_beta
ci_upper <- beta_hat + qt(0.975, df = n_obs - p_cols) * se_beta

# Table 3 data frame
table3 <- data.frame(
  Parameter  = names(beta_hat),
  Estimate   = as.vector(beta_hat),
  Std_Error  = se_beta,
  t_stat     = t_stats,
  p_value    = p_values,
  CI_Lower   = ci_lower,
  CI_Upper   = ci_upper
)

# =============================================================================
# SECTION 6 – Single synthetic data set (initial check)
# =============================================================================

epsilon_single <- rnorm(n_obs, mean = 0, sd = sigma_hat)
y_sim_single   <- as.vector(X %*% beta_hat) + epsilon_single
beta_sim_single <- qr.solve(X, y_sim_single)

comparison_single <- data.frame(
  Parameter  = colnames(X),
  True       = as.vector(beta_hat),
  Estimated  = as.vector(beta_sim_single),
  Difference = as.vector(beta_sim_single - beta_hat)
)

coef_error_single <- sqrt(sum((beta_sim_single - beta_hat)^2))
y_sim_single_hat  <- as.vector(X %*% beta_sim_single)
rmse_single       <- sqrt(mean((y_sim_single - y_sim_single_hat)^2))

# =============================================================================
# SECTION 7 – Simulation study  (N = 1000 replications)
# =============================================================================

N          <- 1000
beta_store <- matrix(0, N, p_cols)
rmse_store <- numeric(N)

for (i in seq_len(N)) {
  eps        <- rnorm(n_obs, 0, sigma_hat)
  y_s        <- as.vector(X %*% beta_hat) + eps
  b_s        <- qr.solve(X, y_s)
  beta_store[i, ] <- b_s
  yhat_s     <- as.vector(X %*% b_s)
  rmse_store[i]   <- sqrt(mean((y_s - yhat_s)^2))
}

colnames(beta_store) <- colnames(X)

# Table 4 simulation summary
table4 <- data.frame(
  Parameter     = colnames(X),
  True_Value    = as.vector(beta_hat),
  Mean_Estimate = colMeans(beta_store),
  Bias          = colMeans(beta_store) - as.vector(beta_hat),
  SD_Estimate   = apply(beta_store, 2, sd)
)

# =============================================================================
# SECTION 8 – Save all results
# =============================================================================

save(
  # Data objects
  y, X, borough_labels,
  # EDA summary stats
  table1,
  # Model results
  beta_hat, y_hat, residuals, sigma_hat,
  r_squared, adj_r_squared, rss, tss,
  # Inference
  table3,
  # Simulation
  beta_store, rmse_store, table4,
  comparison_single, coef_error_single, rmse_single,
  # Dimensions
  n_obs, p_cols, N,
  file = paste0("results/run_results_", seed, ".rds")
)

cat("run_file.r completed for seed =", seed, "\n")
cat("Results saved to results/run_results_", seed, ".rds\n", sep = "")

# =============================================================================
# bootstrap.r
# Purpose: Load and clean NYC crash data, fit OLS, run bootstrap study.
# Usage:   Rscript bootstrap.r <seed> <N>
#          e.g. Rscript bootstrap.r 1 1000
# Output:  results/bootstrap_results_<seed>.rds
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
seed <- if (length(args) >= 1) as.integer(args[1]) else 1
N <- if (length(args) >= 2) as.integer(args[2]) else 1000
set.seed(seed)

# ---- 0. Output directory ----------------------------------------------------
dir.create("Report_bootstrap/Results",
           recursive = TRUE, showWarnings = FALSE)
# =============================================================================
# SECTION 1 -- Data import and cleaning
# =============================================================================

crash <- read.csv("~/Downloads/Motor_Vehicle_Collisions_-_Crashes.csv",
                  stringsAsFactors = FALSE)

# Retain only the columns needed for the analysis.
crash_sub <- crash[, c(
  "CRASH.TIME",
  "BOROUGH",
  "NUMBER.OF.PERSONS.INJURED",
  "CONTRIBUTING.FACTOR.VEHICLE.1",
  "VEHICLE.TYPE.CODE.1",
  "VEHICLE.TYPE.CODE.2"
)]

# Remove rows with missing or blank values in key fields.
crash_sub <- crash_sub[!is.na(crash_sub$NUMBER.OF.PERSONS.INJURED), ]
crash_sub <- crash_sub[crash_sub$BOROUGH != "" &
                         !is.na(crash_sub$BOROUGH), ]
crash_sub <- crash_sub[crash_sub$CRASH.TIME != "" &
                         !is.na(crash_sub$CRASH.TIME), ]
crash_sub <- crash_sub[crash_sub$CONTRIBUTING.FACTOR.VEHICLE.1 != "" &
                         !is.na(crash_sub$CONTRIBUTING.FACTOR.VEHICLE.1), ]
crash_sub <- crash_sub[crash_sub$VEHICLE.TYPE.CODE.1 != "" &
                         !is.na(crash_sub$VEHICLE.TYPE.CODE.1), ]

# Draw the same size analysis sample used in the midterm project.
n_keep <- 20000
keep_index <- sample(seq_len(nrow(crash_sub)), n_keep)
crash_sub <- crash_sub[keep_index, ]

# Table 1 data for the report.
table1 <- data.frame(
  Item = c("Original observations", "After cleaning", "Analysis sample",
           "Response variable", "Predictors including intercept"),
  Value = c(nrow(crash), nrow(crash[
    !is.na(crash$NUMBER.OF.PERSONS.INJURED) &
      crash$BOROUGH != "" & !is.na(crash$BOROUGH), ]),
    n_keep,
    "NUMBER.OF.PERSONS.INJURED",
    "15")
)

# =============================================================================
# SECTION 2 -- Response and predictor construction
# =============================================================================

y <- crash_sub$NUMBER.OF.PERSONS.INJURED
hour <- as.numeric(sub(":.*", "", crash_sub$CRASH.TIME))

night <- ifelse(hour >= 21 | hour <= 5, 1, 0)
multi_vehicle <- ifelse(crash_sub$VEHICLE.TYPE.CODE.2 != "", 1, 0)
borough_brooklyn <- ifelse(crash_sub$BOROUGH == "BROOKLYN", 1, 0)
borough_manhattan <- ifelse(crash_sub$BOROUGH == "MANHATTAN", 1, 0)
borough_queens <- ifelse(crash_sub$BOROUGH == "QUEENS", 1, 0)
borough_bronx <- ifelse(crash_sub$BOROUGH == "BRONX", 1, 0)

factor1 <- crash_sub$CONTRIBUTING.FACTOR.VEHICLE.1
factor_inattention <- ifelse(factor1 == "Driver Inattention/Distraction", 1, 0)
factor_following <- ifelse(factor1 == "Following Too Closely", 1, 0)
factor_yield <- ifelse(factor1 == "Failure to Yield Right-of-Way", 1, 0)
factor_unspecified <- ifelse(factor1 == "Unspecified", 1, 0)

veh1 <- toupper(crash_sub$VEHICLE.TYPE.CODE.1)
vehicle_sedan <- ifelse(grepl("SEDAN", veh1), 1, 0)
vehicle_suv <- ifelse(grepl("SPORT|SUBN|UTILITY|SUV", veh1), 1, 0)
vehicle_pickup <- ifelse(grepl("PICK|P/U", veh1), 1, 0)
vehicle_motorcycle <- ifelse(grepl("MOTORCYCLE|MOTORBIKE|MOPED", veh1), 1, 0)

# =============================================================================
# SECTION 3 -- Design matrix
# =============================================================================

X <- cbind(
  Intercept = 1,
  Night = night,
  MultiVehicle = multi_vehicle,
  Brooklyn = borough_brooklyn,
  Manhattan = borough_manhattan,
  Queens = borough_queens,
  Bronx = borough_bronx,
  Inattention = factor_inattention,
  FollowingTooClosely = factor_following,
  FailureToYield = factor_yield,
  UnspecifiedFactor = factor_unspecified,
  Sedan = vehicle_sedan,
  SUV = vehicle_suv,
  Pickup = vehicle_pickup,
  Motorcycle = vehicle_motorcycle
)

# Remove incomplete rows and zero-variance columns.
keep_rows <- complete.cases(X, y)
X <- X[keep_rows, , drop = FALSE]
y <- y[keep_rows]
keep_cols <- c(TRUE, apply(X[, -1, drop = FALSE], 2, var) > 0)
X <- X[, keep_cols, drop = FALSE]

# Borough labels for EDA figures.
borough_labels <- rep("STATEN ISLAND", nrow(X))
if ("Brooklyn" %in% colnames(X)) {
  borough_labels[X[, "Brooklyn"] == 1] <- "BROOKLYN"
}
if ("Manhattan" %in% colnames(X)) {
  borough_labels[X[, "Manhattan"] == 1] <- "MANHATTAN"
}
if ("Queens" %in% colnames(X)) {
  borough_labels[X[, "Queens"] == 1] <- "QUEENS"
}
if ("Bronx" %in% colnames(X)) {
  borough_labels[X[, "Bronx"] == 1] <- "BRONX"
}

# =============================================================================
# SECTION 4 -- OLS fit on the real data
# =============================================================================

beta_hat <- qr.solve(X, y)
y_hat <- as.vector(X %*% beta_hat)
residuals <- y - y_hat
rss <- sum(residuals^2)
tss <- sum((y - mean(y))^2)
r_squared <- 1 - rss / tss
n_obs <- nrow(X)
p_cols <- ncol(X)
adj_r_squared <- 1 - (rss / (n_obs - p_cols)) / (tss / (n_obs - 1))
sigma_hat <- sqrt(mean(residuals^2))

sigma2_unbiased <- rss / (n_obs - p_cols)
XtX_inv <- solve(t(X) %*% X)
se_beta <- sqrt(diag(XtX_inv) * sigma2_unbiased)
t_stats <- beta_hat / se_beta
p_values <- 2 * pt(abs(t_stats), df = n_obs - p_cols, lower.tail = FALSE)
ci_lower <- beta_hat - qt(0.975, df = n_obs - p_cols) * se_beta
ci_upper <- beta_hat + qt(0.975, df = n_obs - p_cols) * se_beta

table3 <- data.frame(
  Parameter = names(beta_hat),
  Estimate = as.vector(beta_hat),
  Std_Error = se_beta,
  t_stat = t_stats,
  p_value = p_values,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper
)

# =============================================================================
# SECTION 5 -- Bootstrap study
# =============================================================================

# This is a residual bootstrap.  The design matrix X is held fixed, and the
# empirical residuals are resampled to create new response vectors.
#
# Standardized residuals are used to account for leverage:
#     r_i = u_hat_i / sqrt(1 - h_i)
# where h_i is the ith diagonal element of the projection matrix.

Px <- X %*% solve(t(X) %*% X) %*% t(X)
h <- diag(Px)
std_residuals <- residuals / sqrt(1 - h)
centered_std_residuals <- std_residuals - mean(std_residuals)

beta_boot <- matrix(NA, nrow = N, ncol = p_cols)
rmse_boot <- numeric(N)
sigma_boot <- numeric(N)

for (i in seq_len(N)) {
  u_star <- sample(centered_std_residuals, size = n_obs, replace = TRUE)
  y_star <- y_hat + u_star
  b_star <- qr.solve(X, y_star)
  y_star_hat <- as.vector(X %*% b_star)
  e_star <- y_star - y_star_hat

  beta_boot[i, ] <- b_star
  rmse_boot[i] <- sqrt(mean(e_star^2))
  sigma_boot[i] <- sqrt(sum(e_star^2) / (n_obs - p_cols))

  if (i %% 100 == 0) {
    cat("Completed bootstrap sample", i, "of", N, "\n")
  }
}

colnames(beta_boot) <- colnames(X)

# Bootstrap percentile confidence intervals and two-sided p-values.
alpha <- 0.05
boot_ci_lower <- apply(beta_boot, 2, quantile, probs = alpha / 2)
boot_ci_upper <- apply(beta_boot, 2, quantile, probs = 1 - alpha / 2)
boot_se <- apply(beta_boot, 2, sd)

boot_p_value <- numeric(p_cols)
for (j in seq_len(p_cols)) {
  prop_lower <- mean(beta_boot[, j] <= 0)
  prop_upper <- mean(beta_boot[, j] >= 0)
  boot_p_value[j] <- 2 * min(prop_lower, prop_upper)
  boot_p_value[j] <- min(boot_p_value[j], 1)
}

bootstrap_summary <- data.frame(
  Parameter = colnames(X),
  Estimate = as.vector(beta_hat),
  Parametric_SE = as.vector(se_beta),
  Bootstrap_SE = as.vector(boot_se),
  Parametric_CI_Lower = as.vector(ci_lower),
  Parametric_CI_Upper = as.vector(ci_upper),
  Bootstrap_CI_Lower = as.vector(boot_ci_lower),
  Bootstrap_CI_Upper = as.vector(boot_ci_upper),
  Parametric_p_value = as.vector(p_values),
  Bootstrap_p_value = as.vector(boot_p_value),
  Bootstrap_Mean = as.vector(colMeans(beta_boot)),
  Bootstrap_Bias = as.vector(colMeans(beta_boot) - beta_hat)
)

# Output values used in the report.
save(
  y, X, borough_labels, table1,
  beta_hat, y_hat, residuals, sigma_hat,
  r_squared, adj_r_squared, rss, tss,
  table3,
  beta_boot, rmse_boot, sigma_boot,
  bootstrap_summary,
  boot_ci_lower, boot_ci_upper, boot_se, boot_p_value,
  n_obs, p_cols, N, seed,
  file = paste0("Report_bootstrap/Results/bootstrap_results_", seed, ".rds")
)

cat("bootstrap.r completed for seed =", seed, "and N =", N, "\n")
cat("Results saved to Report_bootstrap/results/bootstrap_results_",
    seed, ".rds\n", sep = "")
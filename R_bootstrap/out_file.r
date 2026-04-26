# =============================================================================
# out_file.r
# ST 453 Final Project
# Purpose: Load bootstrap.r output and produce final figures and tables.
# Usage:   Rscript out_file.r <seed>
#          e.g. Rscript out_file.r 1
# Output:  Report_bootstrap/figures/ and Report_bootstrap/tables/
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
seed <- if (length(args) >= 1) as.integer(args[1]) else 1

# ---- Load bootstrap results -------------------------------------------------

results_file <- paste0("Report_bootstrap/results/bootstrap_results_",
                       seed, ".rds")

if (!file.exists(results_file)) {
  stop("Results file not found: ", results_file,
       "\nRun bootstrap.r first.")
}

load(results_file)

# ---- Output directories -----------------------------------------------------

dir.create("Report_bootstrap/Figures",
           recursive = TRUE, showWarnings = FALSE)

dir.create("Report_bootstrap/Tables",
           recursive = TRUE, showWarnings = FALSE)

# ---- Helper function for figures -------------------------------------------

fig <- function(name, width = 700, height = 500) {
  png(paste0("Report_bootstrap/Figures/", name, ".png"),
      width = width, height = height)
}

# =============================================================================
# TABLE 1 -- Data source and analysis sample
# =============================================================================

write.csv(table1,
          "Report_bootstrap/Tables/table1_data_description.csv",
          row.names = FALSE)

cat("Table 1 saved.\n")

# =============================================================================
# FIGURE 1 -- Histogram of persons injured
# =============================================================================

fig("fig1_hist_injuries", width = 600, height = 500)

h <- hist(y, breaks = 0:16 - 0.5, plot = FALSE)

barplot(h$counts + 1,
        log = "y",
        col = "gray",
        border = "white",
        names.arg = h$mids,
        main = "Figure 1: Distribution of Persons Injured",
        xlab = "Persons Injured",
        ylab = "Frequency (log scale)",
        las = 1)

box()
dev.off()

cat("Figure 1 saved.\n")

# =============================================================================
# FIGURE 2 -- Injuries by night indicator
# =============================================================================

fig("fig2_boxplot_night", width = 600, height = 500)

boxplot(y ~ X[, "Night"],
        main = "Figure 2: Persons Injured by Night vs Day",
        xlab = "Night Indicator (0 = Day, 1 = Night)",
        ylab = "Persons Injured",
        names = c("Day", "Night"),
        col = c("lightgray", "gray"))

dev.off()

cat("Figure 2 saved.\n")

# =============================================================================
# FIGURE 3 -- Injuries by borough
# =============================================================================

fig("fig3_boxplot_borough", width = 700, height = 500)

boxplot(y ~ borough_labels,
        main = "Figure 3: Persons Injured by Borough",
        xlab = "Borough",
        ylab = "Persons Injured",
        las = 2,
        col = "lightgray")

dev.off()

cat("Figure 3 saved.\n")

# =============================================================================
# FIGURE 4 -- Injuries by multi-vehicle indicator
# =============================================================================

fig("fig4_boxplot_multivehicle", width = 600, height = 500)

boxplot(y ~ X[, "MultiVehicle"],
        main = "Figure 4: Persons Injured by Multi-Vehicle Indicator",
        xlab = "Multiple Vehicles Involved (0 = No, 1 = Yes)",
        ylab = "Persons Injured",
        names = c("Single", "Multi"),
        col = c("lightgray", "gray"))

dev.off()

cat("Figure 4 saved.\n")

# =============================================================================
# FIGURE 5 -- Observed versus predicted values
# =============================================================================

fig("fig5_observed_vs_predicted", width = 650, height = 600)

plot(y_hat, y,
     main = "Figure 5: Observed vs Predicted Persons Injured",
     xlab = "Predicted Values",
     ylab = "Observed Persons Injured",
     pch = 16,
     cex = 0.4,
     col = rgb(0, 0, 0, 0.3))

abline(0, 1, lwd = 2, col = "red")

dev.off()

cat("Figure 5 saved.\n")

# =============================================================================
# TABLE 2 -- Real-data coefficient estimates
# =============================================================================

table2 <- data.frame(
  Parameter = names(beta_hat),
  Estimate = round(as.vector(beta_hat), 6)
)

write.csv(table2,
          "Report_bootstrap/Tables/table2_real_data_coefficients.csv",
          row.names = FALSE)

cat("Table 2 saved.\n")

# =============================================================================
# TABLE 3 -- Parametric inference from real-data OLS fit
# =============================================================================

table3_out <- table3
table3_out[, -1] <- round(table3_out[, -1], 6)

write.csv(table3_out,
          "Report_bootstrap/tables/table3_parametric_inference.csv",
          row.names = FALSE)

cat("Table 3 saved.\n")

# =============================================================================
# TABLE 4 -- Bootstrap inference and parametric comparison
# =============================================================================

table4_out <- bootstrap_summary
table4_out[, -1] <- round(table4_out[, -1], 6)

write.csv(table4_out,
          "Report_bootstrap/Tables/table4_bootstrap_inference.csv",
          row.names = FALSE)

cat("Table 4 saved.\n")

# =============================================================================
# FIGURE 6 -- Bootstrap distribution of Night coefficient
# =============================================================================

fig("fig6_bootstrap_night", width = 700, height = 500)

hist(beta_boot[, "Night"],
     main = "Figure 6: Bootstrap Distribution of Night Coefficient",
     xlab = "Bootstrap Estimate",
     col = "lightgray",
     border = "black",
     breaks = floor(sqrt(N)))

abline(v = beta_hat["Night"], col = "red", lwd = 2)
abline(v = boot_ci_lower["Night"], col = "blue", lwd = 2, lty = 2)
abline(v = boot_ci_upper["Night"], col = "blue", lwd = 2, lty = 2)

legend("topright",
       legend = c("Real-data estimate", "95% bootstrap CI"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1, 2))

dev.off()

cat("Figure 6 saved.\n")

# =============================================================================
# FIGURE 7 -- Bootstrap distribution of MultiVehicle coefficient
# =============================================================================

fig("fig7_bootstrap_multivehicle", width = 700, height = 500)

hist(beta_boot[, "MultiVehicle"],
     main = "Figure 7: Bootstrap Distribution of MultiVehicle Coefficient",
     xlab = "Bootstrap Estimate",
     col = "lightgray",
     border = "black",
     breaks = floor(sqrt(N)))

abline(v = beta_hat["MultiVehicle"], col = "red", lwd = 2)
abline(v = boot_ci_lower["MultiVehicle"], col = "blue", lwd = 2, lty = 2)
abline(v = boot_ci_upper["MultiVehicle"], col = "blue", lwd = 2, lty = 2)

legend("topright",
       legend = c("Real-data estimate", "95% bootstrap CI"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1, 2))

dev.off()

cat("Figure 7 saved.\n")

# =============================================================================
# FIGURE 8 -- Bootstrap distribution of FailureToYield coefficient
# =============================================================================

fig("fig8_bootstrap_failure_to_yield", width = 700, height = 500)

hist(beta_boot[, "FailureToYield"],
     main = "Figure 8: Bootstrap Distribution of FailureToYield",
     xlab = "Bootstrap Estimate",
     col = "lightgray",
     border = "black",
     breaks = floor(sqrt(N)))

abline(v = beta_hat["FailureToYield"], col = "red", lwd = 2)
abline(v = boot_ci_lower["FailureToYield"], col = "blue", lwd = 2, lty = 2)
abline(v = boot_ci_upper["FailureToYield"], col = "blue", lwd = 2, lty = 2)

legend("topright",
       legend = c("Real-data estimate", "95% bootstrap CI"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1, 2))

dev.off()

cat("Figure 8 saved.\n")

# =============================================================================
# FIGURE 9 -- Parametric SE versus bootstrap SE
# =============================================================================

fig("fig9_parametric_vs_bootstrap_se", width = 700, height = 650)

plot(bootstrap_summary$Parametric_SE,
     bootstrap_summary$Bootstrap_SE,
     main = "Figure 9: Parametric SE vs Bootstrap SE",
     xlab = "Parametric Standard Error",
     ylab = "Bootstrap Standard Error",
     pch = 19)

abline(0, 1, col = "red", lwd = 2)

text(bootstrap_summary$Parametric_SE,
     bootstrap_summary$Bootstrap_SE,
     labels = bootstrap_summary$Parameter,
     pos = 4,
     cex = 0.65)

dev.off()

cat("Figure 9 saved.\n")

# =============================================================================
# FIGURE 10 -- Bootstrap RMSE distribution
# =============================================================================

fig("fig10_bootstrap_rmse", width = 700, height = 500)

hist(rmse_boot,
     main = paste0("Figure 10: Bootstrap RMSE Across ", N, " Samples"),
     xlab = "Bootstrap RMSE",
     col = "lightgray",
     border = "black",
     breaks = floor(sqrt(N)))

abline(v = mean(rmse_boot), col = "red", lwd = 2)

dev.off()

cat("Figure 10 saved.\n")

# =============================================================================
# TABLE 5 -- Selected results for report
# =============================================================================

selected_parameters <- c("Night", "MultiVehicle",
                         "FailureToYield", "Motorcycle")

table5 <- bootstrap_summary[
  bootstrap_summary$Parameter %in% selected_parameters, ]

table5[, -1] <- round(table5[, -1], 6)

write.csv(table5,
          "Report_bootstrap/Tables/table5_selected_results.csv",
          row.names = FALSE)

cat("Table 5 saved.\n")

# =============================================================================
# Console report summary
# =============================================================================

night_row <- bootstrap_summary[bootstrap_summary$Parameter == "Night", ]
mv_row <- bootstrap_summary[bootstrap_summary$Parameter == "MultiVehicle", ]
yield_row <- bootstrap_summary[
  bootstrap_summary$Parameter == "FailureToYield", ]
mc_row <- bootstrap_summary[bootstrap_summary$Parameter == "Motorcycle", ]

cat("\n====================================================\n")
cat("KEY VALUES FOR FINAL REPORT\n")
cat("====================================================\n")
cat(sprintf("n_obs                 : %d\n", n_obs))
cat(sprintf("p_cols                : %d\n", p_cols))
cat(sprintf("Bootstrap N           : %d\n", N))
cat(sprintf("mean(y)               : %.6f\n", mean(y)))
cat(sprintf("sigma_hat             : %.7f\n", sigma_hat))
cat(sprintf("R-squared             : %.6f\n", r_squared))
cat(sprintf("Adjusted R-squared    : %.6f\n", adj_r_squared))

cat("\nNight coefficient:\n")
cat(sprintf("Estimate              : %.8f\n", night_row$Estimate))
cat(sprintf("Parametric SE         : %.8f\n", night_row$Parametric_SE))
cat(sprintf("Bootstrap SE          : %.8f\n", night_row$Bootstrap_SE))
cat(sprintf("Parametric CI         : (%.8f, %.8f)\n",
            night_row$Parametric_CI_Lower,
            night_row$Parametric_CI_Upper))
cat(sprintf("Bootstrap CI          : (%.8f, %.8f)\n",
            night_row$Bootstrap_CI_Lower,
            night_row$Bootstrap_CI_Upper))
cat(sprintf("Parametric p-value    : %.8f\n",
            night_row$Parametric_p_value))
cat(sprintf("Bootstrap p-value     : %.8f\n",
            night_row$Bootstrap_p_value))

cat("\nOther selected estimates:\n")
cat(sprintf("MultiVehicle estimate : %.8f\n", mv_row$Estimate))
cat(sprintf("FailureToYield est.   : %.8f\n", yield_row$Estimate))
cat(sprintf("Motorcycle estimate   : %.8f\n", mc_row$Estimate))

cat("====================================================\n\n")

cat("out_file.r completed. All figures and tables written.\n")
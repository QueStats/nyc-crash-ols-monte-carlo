# =============================================================================
# out_file.R
# Purpose: Load run_file.r output and produce ALL figures and tables
#          in the exact order they appear in the 10-page write-up.
# Usage:   Rscript out_file.r <seed>
#          e.g.  Rscript out_file.r 1
# Output:  figures/  (PNG files for each figure)
#          tables/   (CSV files for each table)
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
seed <- if (length(args) >= 1) as.integer(args[1]) else 1

# ---- Load results -----------------------------------------------------------
results_file <- paste0("results/run_results_", seed, ".rds")
if (!file.exists(results_file)) {
  stop("Results file not found: ", results_file,
       "\nRun run_file.r first.")
}
load(results_file)

# ---- Output directories -----------------------------------------------------
dir.create("figures", showWarnings = FALSE)
dir.create("tables",  showWarnings = FALSE)

# ---- Helper: open/close PNG -------------------------------------------------
fig <- function(name, width = 700, height = 500) {
  png(paste0("figures/", name, ".png"), width = width, height = height)
}

# =============================================================================
# TABLE 1 – Data source, original vs cleaned sample, variables retained
# (Paper: Section 1 – Data Description)
# =============================================================================

write.csv(table1, "tables/table1_data_description.csv", row.names = FALSE)
cat("Table 1 saved.\n")

# =============================================================================
# FIGURE 1 – Histogram of persons injured  (EDA)
# (Paper: Section 2 – Exploratory Data Analysis)
# =============================================================================

fig("fig1_hist_injuries", width = 600, height = 500)
h <- hist(y, breaks=0:16-0.5, plot=FALSE)
barplot(h$counts+1, log="y", col="#3F6BB5", border="white",
        names.arg=h$mids, main="(a) Full Range (log-scale y)",
        xlab="Persons Injured", ylab="Frequency (log scale)", las=1)
box()
dev.off()
cat("Figure 1 saved.\n")

# =============================================================================
# FIGURE 2 – Boxplot: persons injured by Night indicator  (EDA)
# (Paper: Section 2 – Exploratory Data Analysis)
# =============================================================================

fig("fig2_boxplot_night", width = 600, height = 500)
boxplot(y ~ X[, "Night"],
        main  = "Figure 2: Persons Injured by Night vs Day",
        xlab  = "Night Indicator  (0 = Day, 1 = Night)",
        ylab  = "Persons Injured",
        names = c("Day", "Night"),
        col   = c("lightblue", "steelblue"))
dev.off()
cat("Figure 2 saved.\n")

# =============================================================================
# FIGURE 3 – Boxplot: persons injured by Borough  (EDA)
# (Paper: Section 2 – Exploratory Data Analysis)
# =============================================================================

fig("fig3_boxplot_borough", width = 600, height = 500)
boxplot(y ~ borough_labels,
        main  = "Figure 3: Persons Injured by Borough",
        xlab  = "Borough",
        ylab  = "Persons Injured",
        las   = 2,
        col   = "orange")
dev.off()
cat("Figure 3 saved.\n")

# =============================================================================
# FIGURE 4 – Boxplot: persons injured by Multi-Vehicle indicator  (EDA)
# (Paper: Section 2 – Exploratory Data Analysis)
# =============================================================================

fig("fig4_boxplot_multivehicle", width = 600, height = 500)
boxplot(y ~ X[, "MultiVehicle"],
        main  = "Figure 4: Persons Injured by Multi-Vehicle Crash",
        xlab  = "Multiple Vehicles Involved  (0 = No, 1 = Yes)",
        ylab  = "Persons Injured",
        names = c("Single", "Multi"),
        col   = c("lightgreen", "darkgreen"))
dev.off()
cat("Figure 4 saved.\n")

# =============================================================================
# FIGURE 5 – Observed vs Predicted  (Model fit on real data)
# (Paper: Section 4 – Statistical Model / OLS Results)
# =============================================================================

fig("fig5_obs_vs_pred", width = 650, height = 600)
plot(y_hat, y,
     main = "Figure 5: Observed vs Predicted Persons Injured",
     xlab = "Predicted (Fitted Values)",
     ylab = "Observed (Persons Injured)",
     pch  = 16,
     cex  = 0.4,
     col  = rgb(0, 0, 0, 0.3))
abline(0, 1, col = "red", lwd = 2)
legend("topright",
       legend = c("Observations", "45-degree line"),
       pch    = c(16, NA),
       lty    = c(NA, 1),
       col    = c("black", "red"),
       lwd    = c(NA, 2))
dev.off()
cat("Figure 5 saved.\n")

# =============================================================================
# TABLE 2 – Real-data regression coefficients
# (Paper: Section 4 – Discussion of Fitted Model)
# =============================================================================

table2 <- data.frame(
  Parameter = names(beta_hat),
  Estimate  = round(as.vector(beta_hat), 6)
)
write.csv(table2, "tables/table2_coefficients.csv", row.names = FALSE)
cat("Table 2 saved.\n")

# =============================================================================
# TABLE 3 – Inference table: estimates, SEs, t-stats, p-values, 95% CIs
# (Paper: Section 5 – Hypothesis Tests and Confidence Intervals)
# =============================================================================

table3_out        <- table3
table3_out[, -1]  <- round(table3_out[, -1], 6)
write.csv(table3_out, "tables/table3_inference.csv", row.names = FALSE)

# Print Night CI to console for easy copy-paste into paper
cat("\n--- Night coefficient inference ---\n")
night_row <- table3[table3$Parameter == "Night", ]
cat(sprintf("  Estimate : %.8f\n",  night_row$Estimate))
cat(sprintf("  Std Error: %.8f\n",  night_row$Std_Error))
cat(sprintf("  t-stat   : %.4f\n",  night_row$t_stat))
cat(sprintf("  p-value  : %.2e\n",  night_row$p_value))
cat(sprintf("  95%% CI   : (%.8f, %.8f)\n",
            night_row$CI_Lower, night_row$CI_Upper))
cat("Table 3 saved.\n")

# =============================================================================
# TABLE 4 – Simulation summary: true values, mean estimates, bias, SD
# (Paper: Section 6 – Simulation Study Results)
# =============================================================================

table4_out       <- table4
table4_out[, -1] <- round(table4_out[, -1], 8)
write.csv(table4_out, "tables/table4_simulation_summary.csv", row.names = FALSE)
cat("Table 4 saved.\n")

# =============================================================================
# FIGURE 6 – Sampling distribution of Night coefficient
# (Paper: Section 6 – Simulation Study Plots)
# =============================================================================

fig("fig6_sampling_night", width = 700, height = 500)
hist(beta_store[, "Night"],
     main   = "Figure 6: Sampling Distribution of Night Coefficient",
     xlab   = "Estimated Coefficient",
     col    = "lightblue",
     border = "black")
abline(v   = beta_hat["Night"], col = "red", lwd = 2)
legend("topright",
       legend = "True parameter value",
       lty    = 1, col = "red", lwd = 2)
dev.off()
cat("Figure 6 saved.\n")

# =============================================================================
# FIGURE 7 – Sampling distribution of MultiVehicle coefficient
# (Paper: Section 6 – Simulation Study Plots)
# =============================================================================

fig("fig7_sampling_multivehicle", width = 700, height = 500)
hist(beta_store[, "MultiVehicle"],
     main   = "Figure 7: Sampling Distribution of MultiVehicle Coefficient",
     xlab   = "Estimated Coefficient",
     col    = "lightgreen",
     border = "black")
abline(v   = beta_hat["MultiVehicle"], col = "red", lwd = 2)
legend("topright",
       legend = "True parameter value",
       lty    = 1, col = "red", lwd = 2)
dev.off()
cat("Figure 7 saved.\n")

# =============================================================================
# FIGURE 8 – Distribution of RMSE across simulation replications
# (Paper: Section 6 – Simulation Study Plots)
# =============================================================================

fig("fig8_rmse_distribution", width = 700, height = 500)
hist(rmse_store,
     main   = paste0("Figure 8: Distribution of RMSE Across ",
                     N, " Simulations"),
     xlab   = "RMSE",
     col    = "lightgray",
     border = "black")
abline(v   = mean(rmse_store), col = "red",  lwd = 2)
abline(v   = sigma_hat,        col = "blue", lwd = 2, lty = 2)
legend("topright",
       legend = c("Mean simulation RMSE", "Real-data sigma_hat"),
       lty    = c(1, 2),
       col    = c("red", "blue"),
       lwd    = 2)
dev.off()
cat("Figure 8 saved.\n")

# =============================================================================
# FIGURE 9 – True vs mean estimated coefficients (45-degree check)
# (Paper: Section 6 – Simulation Study Plots)
# =============================================================================

fig("fig9_true_vs_mean_coefs", width = 700, height = 650)
plot(as.vector(beta_hat), colMeans(beta_store),
     main = "Figure 9: True vs Mean Estimated Coefficients",
     xlab = "True Coefficient",
     ylab = "Mean Estimated Coefficient",
     pch  = 19,
     cex  = 1.2)
abline(0, 1, col = "red", lwd = 2)
text(as.vector(beta_hat), colMeans(beta_store),
     labels = colnames(X),
     pos    = 4,
     cex    = 0.7)
legend("topleft",
       legend = "45-degree line (unbiased)",
       lty    = 1, col = "red", lwd = 2)
dev.off()
cat("Figure 9 saved.\n")

# =============================================================================
# BONUS FIGURE – 2x2 EDA panel (compact version for paper if space is tight)
# =============================================================================

fig("figA_eda_panel", width = 900, height = 750)
par(mfrow = c(2, 2))

hist(y,
     main  = "Distribution of Persons Injured",
     xlab  = "Persons Injured",
     col   = "lightgray", border = "black")

boxplot(y ~ X[, "Night"],
        main  = "Injuries by Night vs Day",
        xlab  = "Night (0=Day, 1=Night)",
        ylab  = "Persons Injured",
        names = c("Day", "Night"),
        col   = c("lightblue", "steelblue"))

boxplot(y ~ X[, "MultiVehicle"],
        main  = "Injuries by Multi-Vehicle",
        xlab  = "Multi-Vehicle (0=No, 1=Yes)",
        ylab  = "Persons Injured",
        names = c("Single", "Multi"),
        col   = c("lightgreen", "darkgreen"))

boxplot(y ~ borough_labels,
        main  = "Injuries by Borough",
        xlab  = "",
        ylab  = "Persons Injured",
        las   = 2,
        col   = "orange")

par(mfrow = c(1, 1))
dev.off()
cat("Bonus EDA panel saved.\n")

# =============================================================================
# Console summary of key values for paper write-up
# =============================================================================

cat("\n====================================================\n")
cat("KEY VALUES FOR PAPER WRITE-UP\n")
cat("====================================================\n")
cat(sprintf("n (obs in analysis)  : %d\n", n_obs))
cat(sprintf("p (model columns)    : %d\n", p_cols))
cat(sprintf("mean(y)              : %.4f\n", mean(y)))
cat(sprintf("sigma_hat            : %.7f\n", sigma_hat))
cat(sprintf("R-squared            : %.4f\n", r_squared))
cat(sprintf("Adjusted R-squared   : %.4f\n", adj_r_squared))
cat(sprintf("Simulation N         : %d\n",   N))
cat(sprintf("Mean RMSE (sim)      : %.7f\n", mean(rmse_store)))
cat(sprintf("SD   RMSE (sim)      : %.9f\n", sd(rmse_store)))
cat("====================================================\n\n")

cat("out_file.r completed. All figures and tables written.\n")

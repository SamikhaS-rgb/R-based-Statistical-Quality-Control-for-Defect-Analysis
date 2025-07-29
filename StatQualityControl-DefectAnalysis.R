# Load required packages
library(tidyverse)
library(nortest)
library(readr)
library(gridExtra)
library(e1071)

# 1. Import and Clean Data ------------------------------------------------
file_path <- "/Users/samikhasrinivasan/Downloads/Sheet 2 - Group 6 data set for # of defectives.csv"
weight_data <- read_csv(file_path, col_names = FALSE, show_col_types = FALSE) %>%
  select(1:6) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na() %>%
  set_names(paste0("Sample_", 1:6))
weight_matrix <- as.matrix(weight_data)

# 2. Calculate Statistics ------------------------------------------------
subgroup_means <- apply(weight_matrix, 1, mean)
subgroup_ranges <- apply(weight_matrix, 1, function(x) diff(range(x)))
subgroup_sd <- apply(weight_matrix, 1, sd)

# Constants for n=6
A2 <- 0.483
D3 <- 0
D4 <- 2.004
A3 <- 1.287
B3 <- 0.030
B4 <- 1.970
c4 <- 0.9515  

# Initial X-bar chart parameters (using R)
xbar_center_initial <- mean(subgroup_means)
R_bar_initial <- mean(subgroup_ranges)
xbar_ucl_initial <- xbar_center_initial + A2 * R_bar_initial
xbar_lcl_initial <- xbar_center_initial - A2 * R_bar_initial

# 3. Identify Out-of-Control Subgroups ------------------------------------
# Flag points outside control limits
out_of_control <- which(subgroup_means < xbar_lcl_initial | subgroup_means > xbar_ucl_initial)

cat("Out-of-control subgroups identified:", out_of_control, "\n")
cat("Number of subgroups removed:", length(out_of_control), "\n")

# 4. Create Revised Dataset -----------------------------------------------
# Remove out-of-control subgroups
revised_matrix <- weight_matrix[-out_of_control, ]
revised_means <- apply(revised_matrix, 1, mean)
revised_ranges <- apply(revised_matrix, 1, function(x) diff(range(x)))
revised_sd <- apply(revised_matrix, 1, sd)

# 5. Calculate Revised Control Limits -------------------------------------
# X-bar chart parameters (using R - revised)
xbar_center_rev <- mean(revised_means)
R_bar_rev <- mean(revised_ranges)
xbar_ucl_rev <- xbar_center_rev + A2 * R_bar_rev
xbar_lcl_rev <- xbar_center_rev - A2 * R_bar_rev

# R chart parameters (revised)
r_center_rev <- R_bar_rev
r_ucl_rev <- D4 * r_center_rev
r_lcl_rev <- D3 * r_center_rev

# X-bar chart parameters (using S - revised)
s_bar_rev <- mean(revised_sd)
xbar_ucl_s_rev <- xbar_center_rev + A3 * s_bar_rev
xbar_lcl_s_rev <- xbar_center_rev - A3 * s_bar_rev

# S chart parameters (revised)
s_center_rev <- s_bar_rev
s_ucl_rev <- B4 * s_center_rev
s_lcl_rev <- B3 * s_center_rev

# 6. Print Revised Control Limits -----------------------------------------
cat("\n===== REVISED CONTROL LIMITS =====\n")
cat("X-bar Chart Limits (using R):\n")
cat(sprintf("UCL = %.4f, CL = %.4f, LCL = %.4f\n\n", xbar_ucl_rev, xbar_center_rev, xbar_lcl_rev))

cat("R Chart Limits:\n")
cat(sprintf("UCL = %.4f, CL = %.4f, LCL = %.4f\n\n", r_ucl_rev, r_center_rev, r_lcl_rev))

cat("X-bar Chart Limits (using S):\n")
cat(sprintf("UCL = %.4f, CL = %.4f, LCL = %.4f\n\n", xbar_ucl_s_rev, xbar_center_rev, xbar_lcl_s_rev))

cat("S Chart Limits:\n")
cat(sprintf("UCL = %.4f, CL = %.4f, LCL = %.4f\n\n", s_ucl_rev, s_center_rev, s_lcl_rev))

# 7. Create Control Charts Function ---------------------------------------
create_control_chart <- function(data, statistic, center, ucl, lcl, 
                                 chart_name, y_label, breaks_y = NULL) {
  p <- ggplot(data, aes(x = Subgroup, y = {{statistic}})) +
    geom_line(color = "blue") +
    geom_point(size = 2) +
    geom_hline(yintercept = center, color = "green", linetype = "solid", linewidth = 0.8) +
    geom_hline(yintercept = ucl, color = "red", linetype = "dashed", linewidth = 0.8) +
    geom_hline(yintercept = lcl, color = "red", linetype = "dashed", linewidth = 0.8) +
    labs(title = chart_name, 
         x = "Subgroup", 
         y = y_label) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(1, nrow(data), by = 5))
  
  if(!is.null(breaks_y)) {
    p <- p + scale_y_continuous(breaks = breaks_y)
  }
  
  return(p)
}

# 8. Create Revised Control Charts ----------------------------------------
revised_plot_data <- data.frame(
  Subgroup = 1:nrow(revised_matrix),
  Mean = revised_means,
  Range = revised_ranges,
  SD = revised_sd
)

# Generate revised charts
revised_xbar_chart <- create_control_chart(revised_plot_data, Mean, 
                                           xbar_center_rev, xbar_ucl_rev, xbar_lcl_rev,
                                           "Revised X-bar Control Chart (using R)", 
                                           "Subgroup Mean")

revised_r_chart <- create_control_chart(revised_plot_data, Range, 
                                        r_center_rev, r_ucl_rev, r_lcl_rev,
                                        "Revised R Control Chart", 
                                        "Subgroup Range")

revised_xbar_s_chart <- create_control_chart(revised_plot_data, Mean, 
                                             xbar_center_rev, xbar_ucl_s_rev, xbar_lcl_s_rev,
                                             "Revised X-bar Control Chart (using S)", 
                                             "Subgroup Mean")

revised_s_chart <- create_control_chart(revised_plot_data, SD, 
                                        s_center_rev, s_ucl_rev, s_lcl_rev,
                                        "Revised S Control Chart", 
                                        "Subgroup Standard Deviation")

# Display revised charts
print(revised_xbar_chart)
print(revised_r_chart)
print(revised_xbar_s_chart)
print(revised_s_chart)

# 9. Normality Check for Revised Data -------------------------------------
# Anderson-Darling Normality Test
ad_test_rev <- ad.test(as.vector(revised_matrix))
cat("\n===== NORMALITY CHECK (REVISED DATA) =====\n")
cat("Anderson-Darling Normality Test Results:\n")
cat(sprintf("A = %.4f, p-value = %.4f\n", ad_test_rev$statistic, ad_test_rev$p.value))

# Interpret test result
if(ad_test_rev$p.value > 0.05) {
  cat("Conclusion: Fail to reject normality (p > 0.05)\n")
} else {
  cat("Conclusion: Data not normally distributed (p ≤ 0.05)\n")
}

# Create histogram with normal overlay
hist_plot_rev <- ggplot(data.frame(Weight = as.vector(revised_matrix)), aes(x = Weight)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 20, 
                 fill = "skyblue", 
                 color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(as.vector(revised_matrix)), 
                            sd = sd(as.vector(revised_matrix))),
                color = "red", 
                linewidth = 1) +
  labs(title = "Histogram with Normal Distribution Overlay (Revised Data)",
       x = "Weight Measurement",
       y = "Density") +
  theme_minimal()

# Create enhanced Q-Q plot
qq_data_rev <- data.frame(
  Theoretical = qqnorm(as.vector(revised_matrix), plot.it = FALSE)$x,
  Sample = qqnorm(as.vector(revised_matrix), plot.it = FALSE)$y
)

qq_plot_rev <- ggplot(qq_data_rev, aes(x = Theoretical, y = Sample)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = mean(qq_data_rev$Sample), 
              slope = sd(qq_data_rev$Sample),
              color = "red", 
              linewidth = 1) +
  labs(title = "Normal Q-Q Plot (Revised Data)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  annotate("text", x = min(qq_data_rev$Theoretical), y = max(qq_data_rev$Sample),
           label = sprintf("Anderson-Darling p-value: %.4f", ad_test_rev$p.value),
           hjust = 0, vjust = 1, color = "darkgreen")

# Combine plots
grid.arrange(hist_plot_rev, qq_plot_rev, ncol = 2)
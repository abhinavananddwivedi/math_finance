# ============================================================================
# RANDOM WALK SIMULATION: Demonstrating Key Properties
# Using tidyverse paradigm
# ============================================================================

library(tidyverse)
library(patchwork)

# Set theme for all plots
theme_set(theme_bw(base_size = 12))

# Set seed for reproducibility
set.seed(29)

# Simulation parameters
n_walks <- 1000  # number of random walks to simulate
n_values <- c(100, 500, 1000, 5000, 10000)  # different step counts

cat("============================================================\n")
cat("RANDOM WALK SIMULATION\n")
cat("============================================================\n\n")

# ============================================================================
# Helper Functions
# ============================================================================

# Simulate a single random walk
simulate_walk <- function(n_steps, walk_id = 1) {
  steps <- sample(c(-1, 1), size = n_steps, replace = TRUE)
  
  tibble(
    walk_id = walk_id,
    step = 0:n_steps,
    position = c(0, cumsum(steps))
  )
}

# Simulate final positions for many walks
simulate_final_positions <- function(n, n_walks) {
  map_dfr(1:n_walks, function(i) {
    steps <- sample(c(-1, 1), size = n, replace = TRUE)
    tibble(
      n = n,
      walk_id = i,
      final_position = sum(steps)
    )
  })
}

# ============================================================================
# FIGURE 1: Sample Random Walk Paths
# ============================================================================

cat("Generating sample random walks...\n")

sample_walks <- map_dfr(1:20, ~simulate_walk(n_steps = 10000, walk_id = .x))

p1 <- sample_walks %>%
  ggplot(aes(x = step, y = position, group = walk_id, color = as.factor(walk_id))) +
  geom_line(alpha = 0.6, linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", 
             linewidth = 1.5, alpha = 0.7) +
  labs(
    title = "20 Random Walk Realizations",
    subtitle = "Each walk has n = 10,000 steps",
    x = "Step (n)",
    y = "Position (Sₙ)"
  ) +
  scale_color_viridis_d() +
  theme(legend.position = "none") +
  annotate("text", x = 11000, y = 0, label = "E(Sn)=0", 
           vjust = -0.5, color = "black", size = 3.5)

# Save individual plot
# gsaveave("random_walk_paths.png", p1, width = 12, height = 6, dpi = 300)
#cat("✓ Saved: random_walk_paths.png\n\n")

# ============================================================================
# Simulate Many Walks for Each n
# ============================================================================

cat("Simulating", n_walks, "walks for each step count...\n")

final_positions <- map_dfr(n_values, function(n) {
  cat(sprintf("  n = %s...\n", format(n, big.mark = ",")))
  simulate_final_positions(n, n_walks)
})

# ============================================================================
# Calculate Empirical Statistics
# ============================================================================

empirical_stats <- final_positions %>%
  group_by(n) %>%
  summarise(
    mean_position = mean(final_position),
    var_position = var(final_position),
    sd_position = sd(final_position),
    theoretical_var = n,
    theoretical_sd = sqrt(n),
    var_ratio = var_position / n,
    sd_ratio = sd_position / sqrt(n),
    .groups = "drop"
  )

# ============================================================================
# FIGURE 2: Property 1 - E[Sₙ] = 0
# ============================================================================

p2 <- empirical_stats %>%
  ggplot(aes(x = n, y = mean_position)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", 
             linewidth = 1.2, alpha = 0.8) +
  labs(
    title = "Property 1: E[S_n] = 0",
    #subtitle = sprintf("Empirical mean across %s walks", format(n_walks, big.mark = ",")),
    x = "Number of steps (n)",
    y = "Mean position"
  ) +
  scale_x_continuous(labels = scales::comma) +
  # scale_y_continuous(limits = c(-1.50*min(unique(empirical_stats$mean_position)),
  #                               1.50*max(unique(empirical_stats$mean_position))
  #                               )
  #                    )+
  annotate("text", x = max(n_values) * 0.65, y = 0, 
           label = "Theoretical mean = 0", 
           vjust = -1, color = "black", size = 3.5)

# ============================================================================
# FIGURE 3: Property 2 - Var(Sₙ) = n
# ============================================================================

p3 <- empirical_stats %>%
  ggplot(aes(x = n)) +
  geom_line(aes(y = var_position, linetype = "Empirical"), 
            linewidth = 1.2, color = "black") +
  geom_point(aes(y = var_position), size = 4, color = "black") +
  geom_line(aes(y = theoretical_var, linetype = "Theoretical"), 
            linewidth = 1.2, color = "black") +
  labs(
    title = "Property 2: Var(S_n) = n",
    subtitle = sprintf("Empirical variance across %s walks", format(n_walks, big.mark = ",")),
    x = "Number of steps (n)",
    y = "Variance",
    linetype = NULL
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  scale_linetype_manual(values = c("Empirical" = "solid", "Theoretical" = "dashed")) +
  theme(legend.position = c(0.15, 0.85))

# ============================================================================
# FIGURE 4: Property 3 - SD(Sₙ) = √n
# ============================================================================

p4 <- empirical_stats %>%
  mutate(sqrt_n = sqrt(n)) %>%
  ggplot(aes(x = sqrt_n)) +
  geom_line(aes(y = sd_position, linetype = "Empirical"), 
            linewidth = 1.2, color = "black") +
  geom_point(aes(y = sd_position), size = 4, color = "black") +
  geom_line(aes(y = sqrt_n, linetype = "Theoretical"), 
            linewidth = 1.2, color = "black") +
  labs(
    title = "Property 3: SD(S_n) = √n",
    subtitle = "Square root relationship with number of steps",
    x = "√n",
    y = "Standard deviation",
    linetype = NULL
  ) +
  scale_linetype_manual(values = c("Empirical" = "solid", "Theoretical" = "dashed")) +
  theme(legend.position = c(0.15, 0.85))
# Combine three property plots
properties_layout <- p2 | p3 | p4
properties_combined <- properties_layout +
  plot_annotation(
    title = "Random Walk Simulation: Key Properties",
    subtitle = sprintf("Based on %s random walks", format(n_walks, big.mark = ",")),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

#ggsaveave("random_walk_properties.png", properties_combined,        width = 16, height = 5, dpi = 300)
#cat("✓ Saved: random_walk_properties.png\n")

# ============================================================================
# FIGURE 5: Central Limit Theorem - Distributions
# ============================================================================

cat("Creating CLT visualizations...\n")

standardized_positions <- final_positions %>%
  mutate(
    standardized = final_position / sqrt(n),
    n_label = paste0("n = ", format(n, big.mark = ","))
  ) %>%
  mutate(n_label = factor(n_label, levels = paste0("n = ", format(n_values, big.mark = ","))))

p5 <- standardized_positions %>%
  ggplot(aes(x = standardized)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 50, fill = "steelblue", alpha = 0.6, 
                 color = "white", linewidth = 0.3) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "red", linewidth = 1.5, linetype = "dashed") +
  facet_wrap(~n_label, ncol = 2, scales = "free_y") +
  labs(
    title = "Property 3: S_n/√n ~ N(0,1) by Central Limit Theorem",
    subtitle = "Distribution of standardized positions converges to standard normal",
    x = "Standardized position (S_n/√n)",
    y = "Density"
  ) +
  xlim(-4, 4) +
  annotate("text", x = 2.5, y = Inf, label = "N(0,1)", 
           color = "red", vjust = 1.5, size = 3.5)

#ggsaveave("random_walk_clt_distributions.png", p5, width = 12, height = 10, dpi = 300)
cat("✓ Saved: random_walk_clt_distributions.png\n")

# ============================================================================
# FIGURE 6: Q-Q Plots for Normality
# ============================================================================

# Create Q-Q plot data
qq_data <- standardized_positions %>%
  group_by(n, n_label) %>%
  arrange(standardized) %>%
  mutate(
    theoretical_quantile = qnorm(ppoints(n())),
    sample_quantile = standardized
  ) %>%
  ungroup()

p6 <- qq_data %>%
  ggplot(aes(x = theoretical_quantile, y = sample_quantile)) +
  geom_point(alpha = 0.4, color = "black", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 1.2) +
  facet_wrap(~n_label, ncol = 2, scales = "free") +
  labs(
    title = "Q-Q Plots: Testing Normality of Sₙ/√n",
    subtitle = "Points closer to red line indicate better fit to N(0,1)",
    x = "Theoretical N(0,1) quantiles",
    y = "Sample quantiles"
  ) 
# +
#   coord_equal()

#ggsaveave("random_walk_qq_plots.png", p6, width = 12, height = 10, dpi = 300)
#cat("✓ Saved: random_walk_qq_plots.png\n")

# ============================================================================
# FIGURE 7: Scaling Visualization
# ============================================================================

cat("Creating scaling visualization...\n")

# Simulate walks for visualization
scaling_walks <- map_dfr(n_values, function(n) {
  walk <- simulate_walk(n, walk_id = n)
  walk %>%
    mutate(
      n_max = n,
      n_label = format(n, big.mark = ","),
      sqrt_n_upper = sqrt(step),
      sqrt_n_lower = -sqrt(step)
    )
})

p7 <- scaling_walks %>%
  ggplot(aes(x = step, y = position)) +
  geom_ribbon(aes(ymin = sqrt_n_lower, ymax = sqrt_n_upper, 
                  group = n_max
                  #, fill = as.factor(n_max)
                  ), 
              alpha = 0.15) +
  geom_line(aes(color = as.factor(n_max), group = n_max), 
            linewidth = 1.2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", 
             linewidth = 1, alpha = 0.7) +
  labs(
    title = "Random Walk Scaling: Position typically within ±√n",
    subtitle = "Shaded regions show ±√n envelopes",
    x = "Step (n)",
    y = "Position (Sₙ)",
    color = "n value",
    fill = "n value"
  ) +
  scale_color_viridis_d(labels = function(x) format(as.numeric(x), big.mark = ",")) +
  scale_fill_viridis_d(labels = function(x) format(as.numeric(x), big.mark = ",")) +
  scale_x_continuous(labels = scales::comma) +
  theme(legend.position = "top")

#ggsaveave("random_walk_scaling.png", p7, width = 12, height = 7, dpi = 300)
#cat("✓ Saved: random_walk_scaling.png\n")

# ============================================================================
# COMPREHENSIVE SUMMARY TABLE
# ============================================================================

cat("\n============================================================\n")
cat("SUMMARY STATISTICS\n")
cat("============================================================\n")

summary_table <- empirical_stats %>%
  select(n, mean_position, var_position, theoretical_var, var_ratio,
         sd_position, theoretical_sd, sd_ratio) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

print(summary_table, n = Inf)

# ============================================================================
# KEY INSIGHTS
# ============================================================================

cat("\n============================================================\n")
cat("KEY INSIGHTS\n")
cat("============================================================\n")

cat(sprintf("1. Mean positions: range from %.3f to %.3f\n",
            min(empirical_stats$mean_position), 
            max(empirical_stats$mean_position)))
cat("   → Confirms E[Sₙ] ≈ 0\n\n")

cat(sprintf("2. Variance/n ratios: %.3f to %.3f\n",
            min(empirical_stats$var_ratio),
            max(empirical_stats$var_ratio)))
cat("   → Confirms Var(Sₙ) ≈ n\n\n")

cat(sprintf("3. SD/√n ratios: %.3f to %.3f\n",
            min(empirical_stats$sd_ratio),
            max(empirical_stats$sd_ratio)))
cat("   → Confirms SD(Sₙ) ≈ √n\n\n")

cat("4. Distributions of Sₙ/√n increasingly resemble N(0,1) as n grows\n")
cat("   → Demonstrates Central Limit Theorem\n")

# ============================================================================
# BONUS: Combined Summary Figure
# ============================================================================

cat("\nCreating combined summary figure...\n")

combined_layout <- (p1) / (p2 | p3 | p4) +
  plot_annotation(
    title = "Random Walk Simulation: Complete Analysis",
    subtitle = sprintf("Based on %s random walks | Properties: E[Sₙ]=0, Var(Sₙ)=n, Sₙ/√n~N(0,1)", 
                       format(n_walks, big.mark = ",")),
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

#ggsaveave("random_walk_complete_summary.png", combined_layout, width = 16, height = 12, dpi = 300)
#cat("✓ Saved: random_walk_complete_summary.png\n")

# ============================================================================
# ADDITIONAL ANALYSIS: Convergence rates
# ============================================================================

cat("\nCalculating convergence rates...\n")

convergence_data <- final_positions %>%
  group_by(n) %>%
  summarise(
    # Kolmogorov-Smirnov test against N(0,1)
    ks_statistic = ks.test((final_position / sqrt(n)), "pnorm", 0, 1)$statistic,
    # Mean absolute deviation from 0
    mad_mean = mean(abs(final_position)),
    # Root mean square error for variance
    rmse_var = sqrt(mean((final_position^2 - n)^2)),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("CONVERGENCE DIAGNOSTICS\n")
cat("============================================================\n")
print(convergence_data, n = Inf)

cat("\n============================================================\n")
cat("✓ SIMULATION COMPLETE!\n")
cat("============================================================\n")
cat("\nAll plots have been saved to the working directory.\n")
cat("\nFiles created:\n")
cat("  - random_walk_paths.png\n")
cat("  - random_walk_properties.png\n")
cat("  - random_walk_clt_distributions.png\n")
cat("  - random_walk_qq_plots.png\n")
cat("  - random_walk_scaling.png\n")
cat("  - random_walk_complete_summary.png\n")
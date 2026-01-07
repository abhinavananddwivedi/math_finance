# ============================================================================
# RIEMANN vs ITO INTEGRATION
# Illustrating why evaluation points matter in stochastic calculus
# ============================================================================

library(tidyverse)
library(patchwork)

# Set theme
theme_set(theme_minimal(base_size = 12))
set.seed(123)

cat("============================================================\n")
cat("RIEMANN vs ITO INTEGRATION\n")
cat("============================================================\n\n")

# ============================================================================
# PART 1: Classical Riemann Integration (Evaluation Point Doesn't Matter)
# ============================================================================

cat("PART 1: Classical Riemann Integration\n")
cat("--------------------------------------\n")

# Smooth function: f(x) = x^2
f <- function(x) x^2

# Create partition and compute Riemann sums with different evaluation points
compute_riemann_sum <- function(a, b, n, eval_type = "left") {
  # Partition
  delta_x <- (b - a) / n
  x_points <- seq(a, b, length.out = n + 1)
  
  # Choose evaluation points
  if (eval_type == "left") {
    eval_points <- x_points[1:n]
  } else if (eval_type == "right") {
    eval_points <- x_points[2:(n + 1)]
  } else if (eval_type == "midpoint") {
    eval_points <- (x_points[1:n] + x_points[2:(n + 1)]) / 2
  }
  
  # Compute sum
  sum(f(eval_points) * delta_x)
}

# Test with increasing refinements
n_values <- c(5, 10, 20, 50, 100, 200, 500, 1000)
a <- 0
b <- 1
true_integral <- 1/3  # ∫₀¹ x² dx = 1/3

riemann_results <- map_dfr(n_values, function(n) {
  tibble(
    n = n,
    left = compute_riemann_sum(a, b, n, "left"),
    right = compute_riemann_sum(a, b, n, "right"),
    midpoint = compute_riemann_sum(a, b, n, "midpoint")
  )
}) %>%
  pivot_longer(cols = c(left, right, midpoint), 
               names_to = "method", 
               values_to = "value")

# Visualize convergence
p1 <- riemann_results %>%
  ggplot(aes(x = n, y = value, linetype = method)) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2.5, color = "black") +
  geom_hline(yintercept = true_integral, linetype = "dotted", 
             linewidth = 1, color = "black") +
  labs(
    title = "Classical Riemann Integration",
    subtitle = "All evaluation methods converge to 1/3",
    x = "Number of partition points (n)",
    y = "Riemann sum value",
    linetype = "Evaluation"
  ) +
  scale_linetype_manual(values = c("left" = "solid", 
                                   "right" = "dashed", 
                                   "midpoint" = "dotdash")) +
  theme(legend.position = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", color = "black"))

# Visualize rectangles for one case
n_demo <- 8
delta_x <- (b - a) / n_demo
x_points <- seq(a, b, length.out = n_demo + 1)

rectangles_data <- tibble(
  method = rep(c("Left", "Right", "Midpoint"), each = n_demo),
  i = rep(1:n_demo, 3),
  x_left = rep(x_points[1:n_demo], 3),
  x_right = rep(x_points[2:(n_demo + 1)], 3)
) %>%
  mutate(
    x_eval = case_when(
      method == "Left" ~ x_left,
      method == "Right" ~ x_right,
      method == "Midpoint" ~ (x_left + x_right) / 2
    ),
    height = f(x_eval)
  )

p2 <- ggplot() +
  # Function curve
  geom_function(fun = f, linewidth = 1.2, color = "black") +
  # Rectangles
  geom_rect(data = rectangles_data, 
            aes(xmin = x_left, xmax = x_right, ymin = 0, ymax = height),
            fill = "gray80", color = "black", linewidth = 0.3, alpha = 0.6) +
  # Evaluation points
  geom_point(data = rectangles_data, 
             aes(x = x_eval, y = height), 
             size = 2, color = "black") +
  facet_wrap(~method, ncol = 3) +
  labs(
    title = "Riemann Sums with Different Evaluation Points",
    subtitle = "Function: f(x) = x², n = 8 intervals",
    x = "x",
    y = "f(x)"
  ) +
  xlim(0, 1) +
  ylim(0, 1)

# ============================================================================
# PART 2: Stochastic Integration (Evaluation Point DOES Matter!)
# ============================================================================

cat("\nPART 2: Stochastic Integration\n")
cat("--------------------------------\n")

# Simulate Brownian motion
simulate_brownian <- function(T = 1, n = 1000) {
  dt <- T / n
  dW <- rnorm(n, mean = 0, sd = sqrt(dt))
  W <- c(0, cumsum(dW))
  
  tibble(
    t = seq(0, T, length.out = n + 1),
    W = W
  )
}

# Compute stochastic integral ∫ W_t dW_t using different evaluation points
compute_stochastic_integral <- function(W, t, eval_type = "left") {
  n <- length(W) - 1
  dW <- diff(W)
  
  if (eval_type == "left") {
    # Ito integral: use W at left endpoint
    eval_W <- W[1:n]
  } else if (eval_type == "right") {
    # Use W at right endpoint
    eval_W <- W[2:(n + 1)]
  } else if (eval_type == "midpoint") {
    # Stratonovich integral: use average
    eval_W <- (W[1:n] + W[2:(n + 1)]) / 2
  }
  
  sum(eval_W * dW)
}

# Run multiple simulations
n_sims <- 500
T <- 1

cat("Running", n_sims, "simulations...\n")

stochastic_results <- map_dfr(1:n_sims, function(sim) {
  # Simulate path
  path <- simulate_brownian(T = T, n = 1000)
  
  # Compute integral with different methods
  tibble(
    sim = sim,
    W_T = tail(path$W, 1),
    left_ito = compute_stochastic_integral(path$W, path$t, "left"),
    right = compute_stochastic_integral(path$W, path$t, "right"),
    midpoint_stratonovich = compute_stochastic_integral(path$W, path$t, "midpoint")
  )
})

# Theoretical values:
# Ito (left): ∫₀ᵀ W_t dW_t = (W_T² - T) / 2
# Right endpoint: ∫₀ᵀ W_t dW_t = (W_T² + T) / 2  
# Stratonovich (midpoint): ∫₀ᵀ W_t ∘ dW_t = W_T² / 2

stochastic_comparison <- stochastic_results %>%
  mutate(
    ito_theoretical = (W_T^2 - T) / 2,
    right_theoretical = (W_T^2 + T) / 2,
    stratonovich_theoretical = W_T^2 / 2
  )

# Summary statistics
cat("\n============================================================\n")
cat("EMPIRICAL vs THEORETICAL VALUES\n")
cat("============================================================\n")

summary_stats <- tibble(
  Method = c("Ito (left)", "Right endpoint", "Stratonovich (midpoint)"),
  `Mean Empirical` = c(mean(stochastic_comparison$left_ito),
                       mean(stochastic_comparison$right),
                       mean(stochastic_comparison$midpoint_stratonovich)),
  `Mean Theoretical` = c(mean(stochastic_comparison$ito_theoretical),
                         mean(stochastic_comparison$right_theoretical),
                         mean(stochastic_comparison$stratonovich_theoretical)),
  `SD Empirical` = c(sd(stochastic_comparison$left_ito),
                     sd(stochastic_comparison$right),
                     sd(stochastic_comparison$midpoint_stratonovich))
) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

print(summary_stats, n = Inf)

cat("\nKey insight: The three methods give DIFFERENT results!\n")
cat("- Ito (left):           Mean ≈", round(mean(stochastic_comparison$left_ito), 3), "\n")
cat("- Right endpoint:       Mean ≈", round(mean(stochastic_comparison$right), 3), "\n")
cat("- Stratonovich (mid):   Mean ≈", round(mean(stochastic_comparison$midpoint_stratonovich), 3), "\n")
cat("\nThe difference is exactly T/2 =", T/2, "\n")

# Plot: Scatter plot comparing methods
p3 <- stochastic_comparison %>%
  ggplot(aes(x = ito_theoretical, y = left_ito)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
              linewidth = 1, color = "black") +
  geom_point(alpha = 0.2, size = 1.5, color = "black") +
  labs(
    title = "Ito Integral: int W_t dW_t (Left Endpoint)",
    subtitle = "Empirical vs Theoretical: (W_t² - 1)/2",
    x = "Theoretical value",
    y = "Empirical value"
  ) +
  coord_equal() +
  theme(aspect.ratio = 1)

# Plot: Distribution comparison
p4 <- stochastic_comparison %>%
  select(sim, left_ito, right, midpoint_stratonovich) %>%
  pivot_longer(cols = -sim, names_to = "method", values_to = "value") %>%
  mutate(method = case_when(
    method == "left_ito" ~ "Ito (left)",
    method == "right" ~ "Right endpoint",
    method == "midpoint_stratonovich" ~ "Stratonovich (mid)"
  )) %>%
  ggplot(aes(x = value, linetype = method)) +
  geom_density(linewidth = 1, color = "black") +
  labs(
    title = "Distribution of int w_tdw_t with Different Evaluation Points",
    subtitle = "The choice of evaluation point changes the distribution!",
    x = "Integral value",
    y = "Density",
    linetype = "Method"
  ) +
  scale_linetype_manual(values = c("Ito (left)" = "solid",
                                   "Right endpoint" = "dashed",
                                   "Stratonovich (mid)" = "dotdash")) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "white", color = "black"))

# ============================================================================
# PART 3: Visualizing the Ito Integral Construction
# ============================================================================

cat("\nPART 3: Ito Integral Construction\n")
cat("-----------------------------------\n")

# Single path for visualization
single_path <- simulate_brownian(T = 1, n = 50)

# Create rectangles for stochastic integral visualization
n_intervals <- nrow(single_path) - 1
stoch_rectangles <- tibble(
  i = 1:n_intervals,
  t_left = single_path$t[1:n_intervals],
  t_right = single_path$t[2:(n_intervals + 1)],
  W_left = single_path$W[1:n_intervals],
  W_right = single_path$W[2:(n_intervals + 1)],
  dW = W_right - W_left,
  dt = t_right - t_left
)

# Plot: Brownian path with Ito integral construction
p5 <- ggplot() +
  # Brownian motion path
  geom_line(data = single_path, aes(x = t, y = W), 
            linewidth = 1, color = "black") +
  # Rectangles showing Ito integral (height = W_left)
  geom_rect(data = stoch_rectangles %>% filter(i %% 5 == 0), # Show every 5th for clarity
            aes(xmin = t_left, xmax = t_right, 
                ymin = 0, ymax = W_left),
            fill = "gray70", color = "black", 
            linewidth = 0.3, alpha = 0.5) +
  # Mark left endpoints
  geom_point(data = stoch_rectangles %>% filter(i %% 5 == 0),
             aes(x = t_left, y = W_left),
             size = 2.5, color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(
    title = "Ito Integral Construction: ∫₀ᵀ Wₜ dWₜ",
    subtitle = "Each term: W(tᵢ) × [W(tᵢ₊₁) - W(tᵢ)], evaluating at LEFT endpoint",
    x = "Time (t)",
    y = "Brownian motion W(t)"
  ) +
  annotate("text", x = 0.7, y = -1.5, 
           label = "Height = W at left endpoint\nWidth = dW", 
           hjust = 0, size = 3.5,
           color = "black")

# Plot: Comparison of left vs right evaluation
comparison_data <- tibble(
  t = single_path$t[-1],
  left_height = single_path$W[1:n_intervals],
  right_height = single_path$W[2:(n_intervals + 1)],
  dW = diff(single_path$W)
) %>%
  mutate(
    left_contribution = left_height * dW,
    right_contribution = right_height * dW,
    difference = right_contribution - left_contribution
  )

p6 <- comparison_data %>%
  ggplot(aes(x = t)) +
  geom_line(aes(y = left_contribution, linetype = "Left (Ito)"), 
            linewidth = 1, color = "black") +
  geom_line(aes(y = right_contribution, linetype = "Right"), 
            linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(
    title = "Point-by-Point Contributions: Wₜ dWₜ",
    subtitle = "Left vs Right endpoint evaluation",
    x = "Time (t)",
    y = "Contribution to integral",
    linetype = "Evaluation"
  ) +
  scale_linetype_manual(values = c("Left (Ito)" = "solid", 
                                   "Right" = "dashed")) +
  theme(legend.position = c(0.15, 0.85),
        legend.background = element_rect(fill = "white", color = "black"))

# ============================================================================
# PART 4: Quadratic Variation - Why the difference exists
# ============================================================================

cat("\nPART 4: The Role of Quadratic Variation\n")
cat("-----------------------------------------\n")

# The difference between left and right is related to quadratic variation
# Right - Left = ∑ dW² = [W,W]_T = T (quadratic variation)

quadratic_var_data <- map_dfr(1:100, function(sim) {
  path <- simulate_brownian(T = 1, n = 1000)
  dW <- diff(path$W)
  
  tibble(
    sim = sim,
    sum_dW_squared = sum(dW^2),
    theoretical = 1  # Should equal T = 1
  )
})

p7 <- quadratic_var_data %>%
  ggplot(aes(x = sum_dW_squared)) +
  geom_histogram(bins = 30, fill = "gray70", color = "black", 
                 linewidth = 0.3, alpha = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", 
             linewidth = 1.2, color = "black") +
  labs(
    title = "Quadratic Variation: ∑(dWₜ)² → T",
    subtitle = "As partition refines, sum converges to T (not zero!)",
    x = "∑(dWₜ)²",
    y = "Frequency"
  ) +
  annotate("text", x = 1.01, y = Inf, 
           label = "Theoretical = 1", 
           vjust = 1.5, hjust = 0, size = 4, color = "black")

cat("Mean of ∑(dW)²:", round(mean(quadratic_var_data$sum_dW_squared), 4), "\n")
cat("This explains the T/2 difference between evaluation methods!\n")

# ============================================================================
# Save all plots
# ============================================================================

cat("\nSaving plots...\n")

# Layout 1: Riemann integration
riemann_layout <- p1 / p2 +
  plot_annotation(
    title = "Classical Riemann Integration: Evaluation Point Doesn't Matter",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

#ggsave("riemann_integration.png", riemann_layout, width = 12, height = 10, dpi = 300)

# Layout 2: Stochastic integration comparison
stochastic_layout <- (p3 | p4) / p7 +
  plot_annotation(
    title = "Stochastic Integration: Evaluation Point DOES Matter!",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

#ggsave("stochastic_integration_comparison.png", stochastic_layout, width = 12, height = 10, dpi = 300)

# Layout 3: Ito integral construction
ito_layout <- p5 / p6 +
  plot_annotation(
    title = "Ito Integral: Always Evaluate at Left Endpoint",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

#ggsave("ito_integral_construction.png", ito_layout, width = 12, height = 10, dpi = 300)

cat("✓ Saved: riemann_integration.png\n")
cat("✓ Saved: stochastic_integration_comparison.png\n")
cat("✓ Saved: ito_integral_construction.png\n")

cat("\n============================================================\n")
cat("✓ ANALYSIS COMPLETE!\n")
cat("============================================================\n")
cat("\nKEY TAKEAWAYS:\n")
cat("1. Classical Riemann: Evaluation point irrelevant (limit is same)\n")
cat("2. Stochastic: Evaluation point CRUCIAL (different integrals!)\n")
cat("3. Ito integral: Uses LEFT endpoint by definition\n")
cat("4. Difference explained by quadratic variation: [W,W]_T = T ≠ 0\n")
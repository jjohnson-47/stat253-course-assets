#!/usr/bin/env Rscript
# independence_visualization.R
# Section 3.6: Independence and Card Drawing Visualizations
# Compatible with R CLI on Linux terminal

# Check and install required packages
required_packages <- c("ggplot2", "dplyr", "scales", "gridExtra", "grid")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing package: %s\n", pkg))
    install.packages(pkg, repos = "https://cran.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# Clear workspace
rm(list = ls(all = TRUE))

cat("=== SECTION 3.6 INDEPENDENCE VISUALIZATION ===\n")
cat("Starting analysis...\n\n")

# ---- 1. Card Deck Setup ----
create_deck <- function() {
  suits <- c("Hearts", "Diamonds", "Clubs", "Spades")
  ranks <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
  
  deck <- expand.grid(Rank = ranks, Suit = suits, stringsAsFactors = FALSE)
  deck$Color <- ifelse(deck$Suit %in% c("Hearts", "Diamonds"), "Red", "Black")
  deck$IsFace <- deck$Rank %in% c("J", "Q", "K")
  deck$IsRed <- deck$Color == "Red"
  
  return(deck)
}

# ---- 2. Theoretical Probability Calculations ----
deck <- create_deck()

# Calculate theoretical probabilities
P_Red <- sum(deck$IsRed) / nrow(deck)           # P(A) = 26/52 = 0.5
P_Face <- sum(deck$IsFace) / nrow(deck)         # P(B) = 12/52 = 0.2308
P_RedAndFace <- sum(deck$IsRed & deck$IsFace) / nrow(deck)  # P(A∩B) = 6/52 = 0.1154

# Test for independence
P_Red_times_P_Face <- P_Red * P_Face
independence_test <- abs(P_RedAndFace - P_Red_times_P_Face) < 0.0001

cat("=== THEORETICAL CALCULATIONS ===\n")
cat(sprintf("P(Red) = %.4f (26/52)\n", P_Red))
cat(sprintf("P(Face) = %.4f (12/52)\n", P_Face))
cat(sprintf("P(Red ∩ Face) = %.4f (6/52)\n", P_RedAndFace))
cat(sprintf("P(Red) × P(Face) = %.4f\n", P_Red_times_P_Face))
cat(sprintf("Independence test: %s\n", ifelse(independence_test, "INDEPENDENT ✓", "DEPENDENT ✗")))
cat(sprintf("Difference: %.6f\n\n", abs(P_RedAndFace - P_Red_times_P_Face)))

# ---- 3. Simulation Function ----
simulate_card_independence <- function(n_simulations = 10000) {
  cat(sprintf("Running %d simulations...\n", n_simulations))
  
  results <- data.frame(
    simulation = 1:n_simulations,
    is_red = logical(n_simulations),
    is_face = logical(n_simulations),
    is_both = logical(n_simulations)
  )
  
  # Progress indicator for large simulations
  progress_points <- seq(1000, n_simulations, by = 1000)
  
  for (i in 1:n_simulations) {
    # Draw one random card
    card_index <- sample(1:52, 1)
    card <- deck[card_index, ]
    
    results$is_red[i] <- card$IsRed
    results$is_face[i] <- card$IsFace
    results$is_both[i] <- card$IsRed & card$IsFace
    
    # Show progress
    if (i %in% progress_points) {
      cat(sprintf("  Completed %d simulations\n", i))
    }
  }
  
  return(results)
}

# ---- 4. Run Simulation ----
set.seed(253)  # For reproducible results
sim_results <- simulate_card_independence(10000)

# Calculate simulated probabilities
sim_P_Red <- mean(sim_results$is_red)
sim_P_Face <- mean(sim_results$is_face)
sim_P_Both <- mean(sim_results$is_both)
sim_P_Red_times_P_Face <- sim_P_Red * sim_P_Face

cat("\n=== SIMULATION RESULTS (n=10,000) ===\n")
cat(sprintf("Simulated P(Red) = %.4f\n", sim_P_Red))
cat(sprintf("Simulated P(Face) = %.4f\n", sim_P_Face))
cat(sprintf("Simulated P(Red ∩ Face) = %.4f\n", sim_P_Both))
cat(sprintf("Simulated P(Red) × P(Face) = %.4f\n", sim_P_Red_times_P_Face))
cat(sprintf("Simulation difference: %.6f\n\n", abs(sim_P_Both - sim_P_Red_times_P_Face)))

# ---- 5. Create Convergence Plot ----
cat("Creating convergence plot...\n")

# Show how probabilities converge to theoretical values
convergence_data <- data.frame(
  n = 1:nrow(sim_results),
  cumulative_red = cumsum(sim_results$is_red) / (1:nrow(sim_results)),
  cumulative_face = cumsum(sim_results$is_face) / (1:nrow(sim_results)),
  cumulative_both = cumsum(sim_results$is_both) / (1:nrow(sim_results))
)

convergence_data$product_estimate <- convergence_data$cumulative_red * convergence_data$cumulative_face

# Plot convergence
p1 <- ggplot(convergence_data, aes(x = n)) +
  geom_line(aes(y = cumulative_both, color = "Observed P(Red ∩ Face)"), linewidth = 1.2) +
  geom_line(aes(y = product_estimate, color = "P(Red) × P(Face)"), linewidth = 1.2) +
  geom_hline(yintercept = P_RedAndFace, linetype = "dashed", color = "#E31A1C", alpha = 0.8) +
  geom_hline(yintercept = P_Red_times_P_Face, linetype = "dashed", color = "#1F78B4", alpha = 0.8) +
  scale_color_manual(values = c("Observed P(Red ∩ Face)" = "#E31A1C", 
                                "P(Red) × P(Face)" = "#1F78B4")) +
  labs(
    title = "Convergence to Independence: Card Color and Face Status",
    subtitle = "Simulation demonstrates P(A ∩ B) = P(A) × P(B) for independent events",
    x = "Number of Simulations",
    y = "Probability",
    color = "Estimate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# ---- 6. Create Contingency Table Visualization ----
cat("Creating contingency table visualization...\n")

# Visual representation of the 2x2 table
contingency_data <- deck %>%
  group_by(IsRed, IsFace) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(
    Color_Label = ifelse(IsRed, "Red", "Black"),
    Face_Label = ifelse(IsFace, "Face Card", "Number Card"),
    probability = count / 52,
    percentage = round(probability * 100, 1)
  )

p2 <- ggplot(contingency_data, aes(x = Color_Label, y = Face_Label)) +
  geom_tile(aes(fill = count), color = "white", linewidth = 3) +
  geom_text(aes(label = paste0(count, "\n(", percentage, "%)")), 
            color = "white", size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#3182bd", high = "#e6550d", name = "Count") +
  labs(
    title = "Card Distribution: Color vs Face Status",
    subtitle = "Equal proportions show independence",
    x = "Card Color",
    y = "Card Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12)
  )

# ---- 7. Create Independence vs Dependence Comparison ----
cat("Creating dependence comparison...\n")

# Simulate dependent case: drawing two cards without replacement
simulate_dependent <- function(n_simulations = 5000) {
  results <- data.frame(
    first_ace = logical(n_simulations),
    second_ace = logical(n_simulations)
  )
  
  for (i in 1:n_simulations) {
    # Draw two cards without replacement
    drawn_cards <- sample(1:52, 2, replace = FALSE)
    first_card <- deck[drawn_cards[1], ]
    second_card <- deck[drawn_cards[2], ]
    
    results$first_ace[i] <- first_card$Rank == "A"
    results$second_ace[i] <- second_card$Rank == "A"
  }
  
  return(results)
}

# Run dependent simulation
dependent_results <- simulate_dependent(5000)

# Calculate conditional probabilities
P_second_ace_given_first_ace <- mean(dependent_results$second_ace[dependent_results$first_ace])
P_second_ace_given_first_not_ace <- mean(dependent_results$second_ace[!dependent_results$first_ace])
P_second_ace_overall <- mean(dependent_results$second_ace)

# Create comparison data
comparison_data <- data.frame(
  scenario = c("First card is Ace", "First card is not Ace", "Overall (no info)"),
  probability = c(P_second_ace_given_first_ace, P_second_ace_given_first_not_ace, P_second_ace_overall),
  type = c("Conditional", "Conditional", "Unconditional")
)

p3 <- ggplot(comparison_data, aes(x = scenario, y = probability, fill = type)) +
  geom_col(alpha = 0.8, color = "black", width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", probability)), 
            vjust = -0.3, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Conditional" = "#d62728", "Unconditional" = "#2ca02c")) +
  labs(
    title = "Dependent Events: Drawing Aces Without Replacement",
    subtitle = "Different conditional probabilities indicate dependence",
    x = "Condition for Second Draw",
    y = "P(Second card is Ace)",
    fill = "Probability Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold")
  )

# ---- 8. Save All Plots ----
cat("Saving plots...\n")

# Save individual plots
ggsave("independence_convergence.png", p1, width = 12, height = 8, dpi = 300)
ggsave("independence_contingency_table.png", p2, width = 10, height = 8, dpi = 300)
ggsave("dependence_comparison.png", p3, width = 12, height = 8, dpi = 300)

# Create combined plot
combined_plot <- grid.arrange(p2, p3, ncol = 2, 
                             top = textGrob("Section 3.6: Independence vs Dependence in Card Drawing", 
                                          gp = gpar(fontsize = 18, fontface = "bold")))
ggsave("independence_combined_analysis.png", combined_plot, width = 20, height = 10, dpi = 300)

# ---- 9. Summary Statistics Output ----
cat("\n=== FINAL SUMMARY FOR SECTION 3.6 ===\n")
cat("INDEPENDENT EVENTS (Color vs Face Status):\n")
cat(sprintf("• Theoretical P(Red ∩ Face) = %.4f (6/52)\n", P_RedAndFace))
cat(sprintf("• Theoretical P(Red) × P(Face) = %.4f\n", P_Red_times_P_Face))
cat(sprintf("• Simulation difference = %.6f\n", abs(sim_P_Both - sim_P_Red_times_P_Face)))
cat("• Conclusion: Events are INDEPENDENT ✓\n\n")

cat("DEPENDENT EVENTS (Drawing without replacement):\n")
cat(sprintf("• P(2nd Ace | 1st Ace) = %.4f\n", P_second_ace_given_first_ace))
cat(sprintf("• P(2nd Ace | 1st not Ace) = %.4f\n", P_second_ace_given_first_not_ace))
cat(sprintf("• Difference = %.4f (shows dependence)\n", 
    abs(P_second_ace_given_first_ace - P_second_ace_given_first_not_ace)))
cat("• Conclusion: Events are DEPENDENT ✗\n\n")

cat("=== FILES CREATED ===\n")
cat("• independence_convergence.png\n")
cat("• independence_contingency_table.png\n")
cat("• dependence_comparison.png\n")
cat("• independence_combined_analysis.png\n")
cat("\n✓ Script completed successfully!\n")
cat("  Run from: blackboard-content/R-content/chap3/section-3-6-independence/\n")
# Load the libraries
library(ggplot2)
library(dplyr)
library(scales)

# 1. Define parameters for the binomial distribution
num_trials <- 10      # Number of wells drilled
prob_success <- 0.5   # Company's claimed success rate (50%)

# 2. Create a data frame with the number of successes and their probabilities
data_binomial <- data.frame(successes = 0:num_trials) %>%
  mutate(probability = dbinom(successes, size = num_trials, prob = prob_success))

# 3. Enhance the data frame for highlighting and labeling specific bars
data_binomial <- data_binomial %>%
  mutate(
    highlight_group = case_when(
      successes == 0 ~ "Extreme (0 Hits)",
      successes == 10 ~ "Extreme (10 Hits)",
      successes == 5 ~ "Most Likely (5 Hits)",
      TRUE ~ "Other Outcomes"
    ),
    value_label = case_when(
      successes %in% c(0, 5, 10) ~ paste0(round(probability * 100, 1), "%"),
      TRUE ~ ""
    )
  )

# 4. Define a color palette for the bars
color_palette <- c(
  "Extreme (0 Hits)" = "#E41A1C",
  "Extreme (10 Hits)" = "#E41A1C",
  "Most Likely (5 Hits)" = "#377EB8",
  "Other Outcomes" = "#B0BEC5"
)

# 5. Create the ggplot visualization
binomial_plot <- ggplot(data_binomial, aes(x = factor(successes), y = probability, fill = highlight_group)) +
  geom_col(width = 0.75, alpha = 0.9) +
  geom_text(aes(label = value_label),
            vjust = -0.5,
            size = 3.5,
            fontface = "bold",
            color = "black") +
  scale_fill_manual(values = color_palette,
                    name = "Outcome Type",
                    guide = guide_legend(nrow = 1)) +  # MODIFIED: Added guide argument
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(data_binomial$probability) * 1.18),
                     expand = expansion(mult = c(0, .05))) +
  labs(
    title = "Probability Distribution of Successful Oil Wells",
    subtitle = paste0(num_trials, " Wells Drilled, Assuming a ", prob_success * 100, "% Success Rate per Well"),
    x = "Number of Successful Wells (out of 10)",
    y = "Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 20)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top",
    legend.title = element_text(face="bold"),
    legend.justification = "center",        # ADDED: Ensure the legend block is centered
    legend.text = element_text(size = rel(0.85)), # ADDED: Reduce legend text size (e.g., 85% of base)
    legend.spacing.x = unit(0.2, "cm"),     # ADDED: Adjust horizontal spacing between legend items
    # legend.key.size = unit(0.8, "lines")  # Optional: uncomment and adjust to change key size
  )

# 6. Save the plot to a file in the current working directory
# The filename will be "binomial_oil_wells_plot.png"
# You can change the filename, width, height, and dpi as needed.
ggsave("binomial_oil_wells_plot.png", plot = binomial_plot, width = 10, height = 7, dpi = 300)

# Optional: If you want to explicitly print the plot to the console as well (though Rscript usually doesn't show plots)
# print(binomial_plot)

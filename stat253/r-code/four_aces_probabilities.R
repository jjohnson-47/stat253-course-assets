#!/usr/bin/env Rscript
# four_aces_probabilities.R  –– FIXED (no underscores)

library(ggplot2)
library(scales)        # install.packages("scales")  if needed

# ---- 1.  Probability calculations ----
total_hands           <- choose(52, 5)
prob_four_kind        <- 624 / total_hands              # any four‑of‑a‑kind
prob_four_aces_single <- 48  / total_hands              # exactly four aces
prob_four_aces_triple <- prob_four_aces_single^3        # three in a row

prob_lightning_year    <- 1 / 1222000                   # NOAA estimate
prob_powerball_jackpot <- 1 / 292000000                 # Powerball odds
prob_coin_30_heads     <- 0.5 ^ 30                      # 30 heads

events <- c(
  "Four of a Kind\n(1 hand)",
  "Four Aces\n(1 hand)",
  "Four Aces\n3 Hands in a Row",
  "Struck by Lightning\n(US, 1 yr)",
  "Win Powerball\nJackpot",
  "30 Heads\nin a Row"
)

probs <- c(
  prob_four_kind,
  prob_four_aces_single,
  prob_four_aces_triple,
  prob_lightning_year,
  prob_powerball_jackpot,
  prob_coin_30_heads
)

df <- data.frame(
  event       = factor(events, levels = events),
  probability = probs
)

# ---- 2.  Plot ----
p <- ggplot(df, aes(event, probability)) +
  geom_col(fill = "#FFA500") +
  scale_y_log10(
    labels  = scientific_format(digits = 2),
    expand  = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "How Unlikely Is Three Straight Hands of Four Aces?",
    y     = "Probability (log₁₀ scale)",
    x     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- 3.  Save & (optionally) display ----
ggsave("four_aces_probabilities.png", p, width = 8, height = 6, dpi = 300)
if (interactive()) print(p)


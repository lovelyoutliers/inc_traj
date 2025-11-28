library(tidyverse)
library(data.table)

# Data ####
forest_data <- data.frame(
  # Disorder identifiers
  disorder = rep(
    c(
      "Non-affective psychotic disorders",
      "Affective psychotic disorders",
      "Non-psychotic bipolar disorder"
    ),
    each = 6
  ),

  # Family income trajectory
  trajectory = rep(
    c(
      "Lowest (1)",
      "Low-to-increasing (2)",
      "Steady-increasing (3)",
      "Fluctuating (4)",
      "Moderate (5)",
      "Highest (6) - Ref"
    ),
    3
  ),

  # ----------------------------
  # Children of migrant parents
  # ----------------------------
  migrant_or = c(
    2.11,
    1.55,
    1.73,
    2.21,
    1.59,
    1.00, # Non-affective psychotic
    1.07,
    1.23,
    0.82,
    1.58,
    1.23,
    1.00, # Affective psychotic
    1.15,
    1.51,
    1.85,
    2.32,
    2.09,
    1.00 # Non-psychotic bipolar
  ),
  migrant_or_lower = c(
    1.37,
    1.00,
    1.10,
    1.42,
    0.99,
    1,
    0.38,
    0.43,
    0.26,
    0.55,
    0.39,
    1,
    0.77,
    1.00,
    1.22,
    1.54,
    1.36,
    1
  ),
  migrant_or_upper = c(
    3.24,
    2.41,
    2.73,
    3.45,
    2.55,
    1,
    3.01,
    3.52,
    2.58,
    4.58,
    3.86,
    1,
    1.72,
    2.27,
    2.80,
    3.49,
    3.19,
    1
  ),

  migrant_aor = c(
    1.89,
    1.56,
    1.74,
    1.97,
    1.51,
    1.00,
    0.85,
    1.15,
    0.78,
    1.26,
    1.12,
    1.00,
    1.17,
    1.68,
    1.95,
    2.16,
    2.05,
    1.00
  ),
  migrant_aor_lower = c(
    1.22,
    1.00,
    1.10,
    1.26,
    0.94,
    1,
    0.30,
    0.40,
    0.24,
    0.43,
    0.36,
    1,
    0.77,
    1.12,
    1.28,
    1.43,
    1.34,
    1
  ),
  migrant_aor_upper = c(
    2.92,
    2.44,
    2.74,
    3.08,
    2.43,
    1,
    2.44,
    3.33,
    2.45,
    3.70,
    3.53,
    1,
    1.76,
    2.54,
    2.96,
    3.26,
    3.13,
    1
  ),

  # ----------------------------
  # Children of Swedish-born parents
  # ----------------------------
  swedish_or = c(
    1.22,
    0.88,
    0.93,
    1.27,
    1.06,
    1.00,
    1.62,
    1.45,
    1.44,
    1.92,
    1.55,
    1.00,
    1.07,
    0.93,
    0.92,
    1.17,
    0.99,
    1.00
  ),
  swedish_or_lower = c(
    1.02,
    0.74,
    0.77,
    1.07,
    0.88,
    1,
    0.90,
    0.81,
    0.79,
    1.08,
    0.85,
    1,
    0.94,
    0.82,
    0.81,
    1.03,
    0.86,
    1
  ),
  swedish_or_upper = c(
    1.45,
    1.06,
    1.11,
    1.52,
    1.27,
    1,
    2.91,
    2.60,
    2.61,
    3.43,
    2.83,
    1,
    1.22,
    1.06,
    1.05,
    1.34,
    1.14,
    1
  ),

  swedish_aor = c(
    1.36,
    1.06,
    1.02,
    1.31,
    1.08,
    1.00,
    1.70,
    1.60,
    1.53,
    1.96,
    1.57,
    1.00,
    1.05,
    1.04,
    0.95,
    1.07,
    0.95,
    1.00
  ),
  swedish_aor_lower = c(
    1.14,
    0.89,
    0.85,
    1.09,
    0.90,
    1,
    0.93,
    0.89,
    0.84,
    1.09,
    0.85,
    1,
    0.92,
    0.91,
    0.83,
    0.94,
    0.83,
    1
  ),
  swedish_aor_upper = c(
    1.63,
    1.27,
    1.22,
    1.56,
    1.29,
    1,
    3.11,
    2.89,
    2.78,
    3.53,
    2.87,
    1,
    1.21,
    1.18,
    1.08,
    1.22,
    1.09,
    1
  )
)

# Processing ####
plot_data <- forest_data %>%
  pivot_longer(
    cols = -c(disorder, trajectory),
    names_to = c("parent", "measure"),
    names_pattern = "(migrant|swedish)_(.*)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  mutate(
    parent = factor(
      parent,
      levels = c("migrant", "swedish"),
      labels = c("Migrant parents", "Swedish parents")
    ),
    trajectory = factor(
      trajectory,
      levels = rev(c(
        "Lowest (1)",
        "Low-to-increasing (2)",
        "Steady-increasing (3)",
        "Fluctuating (4)",
        "Moderate (5)",
        "Highest (6) - Ref"
      ))
    ),
    disorder = factor(
      disorder,
      levels = c(
        "Non-affective psychotic disorders",
        "Affective psychotic disorders",
        "Non-psychotic bipolar disorder"
      )
    )
  )


# Plot ####
setDT(plot_data)
plot_data[parent == "Migrant parents", parent := "Children of migrant parents"]
plot_data[
  parent == "Swedish parents",
  parent := "Children of swedish-born parents"
]

ggplot(
  plot_data,
  aes(
    x = aor,
    xmin = aor_lower,
    xmax = aor_upper,
    y = trajectory,
    color = parent
  )
) +

  geom_point(position = position_dodge(width = 0.5), size = 3) +

  geom_errorbarh(position = position_dodge(width = 0.5), height = 0.2) +

  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +

  facet_wrap(~disorder, ncol = 1, axes = "all") +

  scale_color_manual(
    values = c(
      "Children of migrant parents" = "#CC5800FF",
      "Children of swedish-born parents" = "#1E8E99FF"
    )
  ) +

  scale_x_continuous(limits = c(0, 3.7)) +

  labs(
    x = "Adjusted odds ratio (95% CI)",
    y = "",
    color = ""
  ) +

  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10, hjust = 0),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.justification = "left",
    legend.background = element_rect(
      fill = NA,
      color = "black",
      linewidth = 0.1
    ),
    legend.box.spacing = unit(0.2, "cm"),
    strip.text = element_text(face = "bold", hjust = 0, size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(family = "Times New Roman"),
    panel.spacing = unit(1, "lines"),
    axis.title.x = element_text(margin = margin(t = 10), size = 10, hjust = 0)
  ) +
  guides(
    color = guide_legend(nrow = 1)
  )

ggsave(
  "x.png",
  width = 6,
  height = 10,
  dpi = "retina"
)

# Comparing Post-Pandemic Recovery Patterns across Regions

# Setup and Libraries
library(tidyverse)      # Core data manipulation
library(readxl)         # Excel Ingestion
library(janitor)        # Cleaning variable names
library(scales)         # Plot formatting
library(broom)          # tidy stastical outputs

# Data Ingestion and Reshaping
journeys_df <- read_excel("data/clean_regional_data.xlsx") %>%
  clean_names() %>%
  mutate(financial_year = as.numeric(year)) %>%
  arrange(financial_year)

regional_journeys_long <- journeys_df %>%
  select(
    financial_year,
    within_england,
    england_scotland,
    england_wales
  ) %>%
  pivot_longer(
    cols = c(within_england, england_scotland, england_wales),
    names_to = "journey_region",
    values_to = "passenger_journeys"
  ) %>%
  mutate(
    journey_region = recode(
      journey_region,
      within_england = "Domestic (England)",
      england_scotland = "England-Scotland",
      england_wales = "England-Wales"
    )
  )
  
# Construct recovery indices (2019 = 100) and calculate recovery gap
recovery_index_df <- regional_journeys_long %>%
  group_by(journey_region) %>%
  mutate(
    baseline_2019_journeys = passenger_journeys[financial_year == 2019],
    recovery_index_2019 = (passenger_journeys / baseline_2019_journeys) * 100,
    recovery_gap_pp = recovery_index_2019 - 100
  ) %>%
  ungroup()
  

# Figure 4 Post-Pandemic Recovery Trajectories
fig4_recovery_trajectories <- ggplot(
  recovery_index_df,
  aes(x = financial_year, y = recovery_index_2019, colour = journey_region)
) +
  geom_line(linewidth = 1.2) +
  geom_hline(
    yintercept = 100,
    linetype = "dotted",
    colour = "black",
    linewidth = 0.5
  ) +
  scale_y_continuous(labels =function(x) paste0(x,"%")) +
  scale_colour_manual(
    values = c(
      "Domestic (England)" = "#1f78b4",
      "England-Scotland" = "#e31a1c",
      "England-Wales" = "#33a02c"
    )
  ) +
  labs(
    title = "Post-Pandemic Recovery Trajectories by Journey Type",
    subtitle = "Passenger journeys indexed to pre-pandemic levels (2019 = 100)",
    x = "Financial Year",
    y = "Recovery Index (%)",
    colour = "Journey Type"
  ) +
  theme_minimal()

print(fig4_recovery_trajectories)


# Figure 5: Recovery Gap Relative to 2019 levels
fig5_recovery_gap <- recovery_index_df %>%
  filter(financial_year >= 2021) %>%
  ggplot(
    aes(
      x = factor(financial_year),
      y = recovery_gap_pp,
      fill = journey_region
    )
  ) +
  geom_col(position = "dodge", width =0.7) +
  geom_hline(
    yintercept = 0,
    colour = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Domestic (England)" = "#1f78b4",
      "England-Scotland" = "#e31a1c",
      "England-Wales" = "#33a02c"
    )
  ) +
  labs(
    title = "Recovery Gap Relative to Pre-Pandemic Levels",
    subtitle = "Negative values indicate incomplete recovery comapred to 2019",
    x = "Financial Year",
    y = "Recovery Gap",
    fill = "Journey Type"
  ) +
  theme_minimal()

print(fig5_recovery_gap)

# Statistical Inference: One-way ANOVA
# Focus on recovery period (2022 -2024)
post_covid_recovery_df <- recovery_index_df %>%
  filter(financial_year >= 2022)

# Global test for differences in mean recovery levels
recovery_anova_model <- aov(
  recovery_index_2019 ~ journey_region,
  data = post_covid_recovery_df
)

summary(recovery_anova_model)

# Post-hoc analysis: Tukey's HSD
recovery_tukey_hsd <- TukeyHSD(recovery_anova_model)
print(recovery_tukey_hsd)

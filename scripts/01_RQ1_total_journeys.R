# 0. Setup and Libraries
library(tidyverse)   # Core data manipulation
library(readxl)      # Excel ingestion
library(janitor)     # Cleaning variable names
library(scales)      # For axis formatting

# 1. Data Ingestion and Formatting
journey_data <- read_excel("data/clean_regional_data.xlsx") %>%
  clean_names() %>%
  mutate(year = as.numeric(year)) %>%
  arrange(year)

# Convert wide format into long format for plotting
journey_data_long <- journey_data %>%
  select(year, within_regions, between_regions) %>%
  pivot_longer(
    cols = c(within_regions, between_regions),
    names_to = "journey_type",
    values_to = "journeys"
  ) %>%
  mutate(
    journey_type = recode(
      journey_type,
      within_regions = "Domestic (Within_Regions)",
      between_regions = "Cross-Border (Between_Region)"
    )
  )

# 2. Figure 1: Total Passenger Journeys (1995-2024)

fig1_total_journeys <- ggplot(journey_data, aes(x = year, y = total_journeys)) +
  geom_area(fill = "#1f78b4", alpha=0.6) +
  geom_line(color = "#1f78b4", linewidth = 1) +
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "black") +
  
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  
  labs(
    title = "Total Rail Passeneger Journeys (1995-2024)",
    subtitle = "Long-term growth followed by a sharp pandemic induced collapse in 2020",
    x = "Financial Year",
    y = "Total Journeys"
  ) +
  theme_minimal(base_size = 12)

print(fig1_total_journeys)


# 3. Figure 2: Domestic vs Cross-Border Journeys 
fig2_market_scale_log <- ggplot(journey_data_long, aes(x = year, y = journeys, colour = journey_type)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "black", linewidth = 0.5) +
  scale_y_log10(labels = label_comma()) +
  
  labs(
    title = "Comparison of Market Scale",
    subtitle = "Parallel growth trends pre-2015, followed by divergence",
    x = "Financial Year",
    y = "Passenger Journeys (Log Scale)",
    colour = "Market Segment",
    ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(fig2_market_scale_log)
              

# 4. Figure 3: Indexed Growth Trajectories (1995 = 100)
journey_growth_index <- journey_data_long %>%
  group_by(journey_type) %>%
  mutate(index_1995 = journeys / first(journeys) * 100) %>%
  ungroup()

fig3_growth_index <- ggplot(journey_growth_index, aes(x = year, y = index_1995, colour = journey_type)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black", linewidth = 0.5) +
  
  labs(
    title = "Relative Growth Velocity (1995 = 100)",
    subtitle = "Cross-border travel grows faster pre-pandemic but contracts more sharply",
    x = "Financial Year",
    y = "Growth Index (1995 = 100)",
    colour = "Market Segment",
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


print(fig3_growth_index)

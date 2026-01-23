# setup and Libraries
library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(broom)   # for tidy model outputs

# Data Ingestion: Regional flows
passenger_journeys_df <- read_excel("data/clean_regional_data.xlsx") %>%
  clean_names() %>%
  mutate(financial_year = as.numeric(year)) %>%
  arrange(financial_year) %>%
  select(
    financial_year,
    within_regions,    # Domestic journeys
    between_regions    # Cross-border journeys
  )

# Data Ingestion: Real-Term Fares
rail_fares_df <- read_excel("data/rail_fares.xlsx") %>%
  clean_names() %>%
  mutate(
    financial_year = as.numeric(year),
    real_term_fare_index = as.numeric(fare_change)
  ) %>%
  select(financial_year, real_term_fare_index)

# Merge and Feature Engineering
# We need to create a single analytical dataset
rq3_analysis_df <- passenger_journeys_df %>%
  inner_join(rail_fares_df, by = "financial_year") %>%
  arrange(financial_year) %>%
  mutate(
    # Rebase to 2019 = 100 to allow fair comaprison pre?post COVID
    domestic_idx = (within_regions / within_regions[financial_year == 2019]) * 100,
    cross_border_idx = (between_regions / between_regions[financial_year == 2019]) * 100,
    
    # The Critical Metric: "Recovery Divergence"
    # Negative values mean Cross-Border is recovering slower than Domestic.
    recovery_divergence_pp = cross_border_idx - domestic_idx,
    
    # Dummy variable for the pandemic era (Structural Break)
    is_post_covid = if_else(financial_year >= 2020, 1, 0)
  ) %>%
  # Filter to 2010 onwards to  focus on relevant modern era
    filter(financial_year >= 2010)
  
  # Check the engineered features
  glimpse(rq3_analysis_df)
  
# 4. Visualisation: Are fares Rising?
  
  fig6_fare_trend <- ggplot(rq3_analysis_df, aes(x = financial_year, y = real_term_fare_index)) +
    geom_line(color = "2c3e50", linewidth = 1) +
    geom_point(size = 2) +
    # Highlight the pandemic start
    geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.6) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Trend in Real-Term Rail Fares (2010-2024)",
      subtitle = "Fares have stabilised in real terms post-2020 (Red Line)",
      x = "Financial Year",
      y = "Real-Term Fare Index",
    ) +
    theme_minimal()
  
  print(fig6_fare_trend)
  

# 5. Visualisation: The Price vs. Divergence Relationship
# We are looking for a negative correlation (Higher fares = Bigger Gap).
  
fig7_scatter <- ggplot(rq3_analysis_df, aes(x = real_term_fare_index, y = recovery_divergence_pp)) +
  geom_point(aes(color = as.factor(is_post_covid)), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dotted") +
  geom_hline(yintercept = 0, color = "grey50") +
  scale_color_manual(values = c("gray60", "red"), labels = c("Pre-COVID", "Post_COVID")) +
  labs(
    title = "Does Price Drive the Recovery Gap?",
    subtitle = "Scatter plot of Real Fares vs. The Gap between Domestic and Cross_border recovery",
    x = "Real-Term Fare Index",
    y = "Recovery Divergence (pp)",
    color = "Era"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(fig7_scatter)

  
# 6. Econometric Analysis (OLS Regression)
# Model: Divergence ~ fares + Covid_Control

model_rq3 <- lm(recovery_divergence_pp ~ real_term_fare_index + is_post_covid,
                data = rq3_analysis_df)

summary(model_rq3)

model_results <- tidy(model_rq3, conf.int = TRUE)
print(model_results)
  
  
  
  
  
  
  
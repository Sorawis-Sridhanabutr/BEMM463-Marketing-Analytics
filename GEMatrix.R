# Load required packages
install.packages(c("tidyverse", "ggplot2"))
library(tidyverse)
library(ggplot2)

# ---- DATA INPUT ----

# Segment Size
segment_size <- c(7.6, 20.2, 24.2, 19.9, 15.1, 13.0)  # Percentage of customers

# Income Levels
income_levels <- c(2.71, 3.71, 2.76, 3.06, 3.89, 3.66)  # Higher is better

# Demographics
age_levels <- c(38.74, 39.27, 39.21, 33.39, 31.58, 28.73)  # Younger is better
education_levels <- c(1.2, 1.42, 1.19, 1.27, 1.67, 1.25)  # Higher is better

# Feature Preferences & Digital Engagement (Mean Values from screenshot)
feature_preferences <- data.frame(
  Segment = 1:6,
  ConstCom = c(2.24, 5.4, 4.27, 4.92, 5.85, 4.09),
  TimelyInf = c(1.61, 5.68, 3.88, 4.37, 4.63, 3.95),
  TaskMgm = c(3.07, 4.86, 3.06, 5.06, 5.52, 3.04),
  DeviceSt = c(2.29, 5.06, 2.61, 3.33, 4.7, 4.92),
  Wellness = c(2.57, 3.47, 3.22, 5.28, 6.4, 5.18),
  Athletic = c(3.3, 2.41, 2.95, 4.31, 4.8, 6.01),
  Style = c(3.22, 3.42, 3.71, 4.7, 6.34, 4.39),
  AmznP = c(0.42, 0.5, 0.42, 0.47, 0.85, 0.83)
)

# ---- MARKET ATTRACTIVENESS ----
feature_weights <- rep(1 / 7, 7)  # Equal weights for all segments

# Compute Feature Importance Score for Each Segment
feature_importance_score <- rowSums(feature_preferences[, 2:8] * feature_weights)

# Normalize Market Attractiveness Scores (Scale 0-10)
market_attractiveness <- (
  (segment_size / max(segment_size)) * 0.15 +  
    (income_levels / max(income_levels)) * 0.15 +  
    (feature_importance_score / max(feature_importance_score)) * 0.40 +  
    (feature_preferences$AmznP / max(feature_preferences$AmznP)) * 0.20 +  
    ((max(age_levels) - age_levels) / max(age_levels)) * 0.05 +  
    (education_levels / max(education_levels)) * 0.05  
) * 10

# ---- COMPETITIVE STRENGTH CALCULATION ----

intel_strengths <- c(
  AI = 10,         
  Sensors = 8,   
  Chipset = 6,   
  IoT = 9,        
  Branding = 4,   
  Risk = 3,      
  Partnerships = 8 
)


# Competitive Strength Weights per Segment
competitive_weights <- list(
  c(0.05, 0.05, 0.05, 0.10, 0.05, 0.50, 0.20), 
  c(0.15, 0.20, 0.10, 0.15, 0.05, 0.25, 0.10),
  c(0.25, 0.25, 0.10, 0.10, 0.10, 0.15, 0.05),
  c(0.30, 0.15, 0.20, 0.20, 0.05, 0.05, 0.05),
  c(0.40, 0.10, 0.10, 0.20, 0.05, 0.10, 0.05),
  c(0.50, 0.10, 0.05, 0.10, 0.05, 0.05, 0.15) 
)

# Compute Competitive Strength per Segment
competitive_strength <- sapply(1:6, function(i) {
  sum(intel_strengths * competitive_weights[[i]]) 
})

# Normalize Competitive Strength (Scale 0-10)
competitive_strength <- (competitive_strength / max(competitive_strength)) * 8.5

# ---- GE MATRIX VISUALIZATION ----
ge_matrix <- data.frame(
  Segment = paste("Segment", 1:6),
  Market_Attractiveness = market_attractiveness,
  Competitive_Strength = competitive_strength
)

# ---- PLOT GE MATRIX ----
ggplot(ge_matrix, aes(x = Competitive_Strength, y = Market_Attractiveness, label = Segment)) +
  geom_point(aes(color = Segment), size = 5) +
  geom_text(vjust = -1.2, size = 5) +
  scale_x_continuous(name = "Competitive Strength", limits = c(0, 10), breaks = 1:10) +
  scale_y_continuous(name = "Market Attractiveness", limits = c(0, 10), breaks = 1:10) +
  theme_minimal() +
  ggtitle("GE Matrix: Intel Smartwatch Market Evaluation") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


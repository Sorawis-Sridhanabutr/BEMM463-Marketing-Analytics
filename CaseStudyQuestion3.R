# Install necessary packages (if not already installed)
install.packages(c('tidyverse', 'psych', 'ggplot2', 'openxlsx'))

# Load required libraries
library(readxl)
library(openxlsx)
library(tidyverse)
library(psych)
library(ggplot2)

# Load dataset
SmartWatch <- read_excel(file.choose())

# Convert 'cluster' to categorical factor
SmartWatch$cluster <- as.factor(SmartWatch$cluster)

# Disable scientific notation for better readability
options(scipen=999)

# Define feature variables
feature_vars <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style")

# Perform ANOVA for each feature & extract p-values
anova_results <- list()
p_values <- c()

for (var in feature_vars) {
  model <- aov(as.formula(paste(var, "~ cluster")), data = SmartWatch)
  anova_results[[var]] <- model
  p_values <- c(p_values, summary(model)[[1]][["Pr(>F)"]][1])  # Extract p-value
}

# Create a summary dataframe with p-values
anova_summary <- data.frame(
  Feature = feature_vars,
  P_Value = round(p_values, 6)
)

# Display ANOVA summary
View(anova_summary)
print(anova_summary)

# Run Tukey's HSD tests
tukey_results <- list()

for (var in feature_vars) {
  model <- aov(as.formula(paste(var, "~ cluster")), data = SmartWatch)
  tukey <- TukeyHSD(model)
  
  # Store ALL results (significant & non-significant)
  tukey_results[[var]] <- as.data.frame(tukey[[1]])
}

# Print Tukey HSD results
print(tukey_results)

# ==========================
# ANOVA & Tukey HSD for Purchasing Power (Income)
# ==========================
anova_income <- aov(Income ~ cluster, data = SmartWatch)
summary(anova_income)
tukey_income <- TukeyHSD(anova_income)
tukey_income_df <- as.data.frame(tukey_income$cluster)

# ==========================
# ANOVA & Tukey HSD for Digital Engagement (AmznP)
# ==========================
anova_amzn <- aov(AmznP ~ cluster, data = SmartWatch)
summary(anova_amzn)
tukey_amzn <- TukeyHSD(anova_amzn)
tukey_amzn_df <- as.data.frame(tukey_amzn$cluster)

# ==========================
# ANOVA & Tukey HSD for Demographic Variables (Age, Female, Degree)
# ==========================
# Age (Demographic Segmentation)
anova_age <- aov(Age ~ cluster, data = SmartWatch)
summary(anova_age)
tukey_age <- TukeyHSD(anova_age)
tukey_age_df <- as.data.frame(tukey_age$cluster)

# Female (Gender Distribution)
anova_female <- aov(Female ~ cluster, data = SmartWatch)
summary(anova_female)
tukey_female <- TukeyHSD(anova_female)
tukey_female_df <- as.data.frame(tukey_female$cluster)

# Degree (Education Level)
anova_degree <- aov(Degree ~ cluster, data = SmartWatch)
summary(anova_degree)
tukey_degree <- TukeyHSD(anova_degree)
tukey_degree_df <- as.data.frame(tukey_degree$cluster)

# ==========================
# Compute actual means for segment profiling
# ==========================
segment_means <- SmartWatch %>%
  group_by(cluster) %>%
  summarise(
    PurchasingPower = mean(Income, na.rm = TRUE),
    DigitalEngagement = mean(AmznP, na.rm = TRUE) * 100,
    Age = mean(Age, na.rm = TRUE),
    FemalePercentage = mean(Female, na.rm = TRUE) * 100,
    EducationLevel = mean(Degree, na.rm = TRUE),
    FeatureDemand = rowMeans(across(all_of(feature_vars)), na.rm = TRUE)
  )

# Normalize scores & rank segments based on attractiveness
segment_means$Score <- rowMeans(segment_means[,2:6])
segment_means <- segment_means[order(-segment_means$Score),]

# Print segment ranking
print(segment_means)

# ==========================
# Save results to Excel
# ==========================
write.xlsx(anova_summary, "ANOVA_Summary.xlsx")
write.xlsx(tukey_results, "Tukey_HSD_Results.xlsx")
write.xlsx(tukey_income_df, "Tukey_Income_Results.xlsx")
write.xlsx(tukey_amzn_df, "Tukey_Amzn_Results.xlsx")
write.xlsx(tukey_age_df, "Tukey_Age_Results.xlsx")
write.xlsx(tukey_female_df, "Tukey_Female_Results.xlsx")
write.xlsx(tukey_degree_df, "Tukey_Degree_Results.xlsx")

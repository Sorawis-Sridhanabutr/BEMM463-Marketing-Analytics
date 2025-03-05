install.packages(c("readxl", "tidyverse", "cluster", "openxlsx"))
library(readxl)
library(tidyverse)
library(cluster)
library(openxlsx)


# Load dataset (replace with actual file path)
SmartWatch <- read_excel(file.choose())

# View first few rows
head(SmartWatch)

# Check structure and summary
str(SmartWatch)
summary(SmartWatch)


# Select only numerical variables for clustering (excluding ID-type columns if needed)
SmartWatch_cluster <- SmartWatch %>% select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)

# Standardize data
SmartWatch_scaled <- scale(SmartWatch_cluster)

# View standardized data
head(SmartWatch_scaled)

# Compute Euclidean distance
distance <- dist(SmartWatch_scaled, method = 'euclidean')

# Perform hierarchical clustering using Ward's method
hc <- hclust(distance, method = 'ward.D')

# Plot dendrogram
plot(hc, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")


# Sort cluster heights and extract top 10 for elbow plot
x <- c(1:10)
sort_height <- sort(hc$height, decreasing = TRUE)
y <- sort_height[1:10]

# Elbow Plot
plot(x, y, type = "b", main = "Elbow Plot", xlab = "Clusters", ylab = "WSS")
lines(x, y, col = "blue")


# Choose the optimal number of clusters (adjust based on elbow plot)
k <- 6  # Example: If the elbow plot suggests 3 clusters

# Assign cluster labels
SmartWatch$cluster <- cutree(hc, k = 6)

# View cluster assignments
table(SmartWatch$cluster)  # Show different market segment sizes

# CALCULATE SEGMENT SIZES
proportions <- table(SmartWatch$cluster) / nrow(SmartWatch)     # Hint: What is your final dataset?
percentages <- proportions * 100
# Display segment sizes in percentages
print(percentages)

SmartWatch_final <- SmartWatch    # Hint: Combine the original data with clusters.
# Check the updated dataset
View(SmartWatch_final)

# Calculate mean values of attributes for each cluster
segments <- SmartWatch %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))

# Display cluster summary
print(segments)

write.xlsx(segments, "SmartWatch_Segments_Mean.xlsx")
write.xlsx(SmartWatch_final, "SmartWatch_Segments.xlsx")


# Install and load required package
install.packages("ggplot2")
library(ggplot2)

# Create the data
brands <- data.frame(
  Brand = c("Apple", "Garmin", "Samsung", "Intel"),
  Fashion_Lifestyle = c(8, 4, 6, 9),
  Athletic_Performance = c(6, 8, 5, 9)
)

# Plot the perceptual map
ggplot(brands, aes(x = Fashion_Lifestyle, y = Athletic_Performance, label = Brand)) +
  geom_point(size = 4, color = "blue") +
  geom_text(vjust = -1, size = 5) +
  xlim(0, 10) +
  ylim(0, 10) +
  labs(
    title = "Perceptual Map of Smartwatch Competitors",
    x = "Fashion & Lifestyle Appeal",
    y = "Athletic & Performance Features"
  ) +
  theme_minimal()

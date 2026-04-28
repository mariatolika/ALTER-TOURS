# Load necessary libraries
library(tidyverse)
library(cluster)
library(factoextra)

# 1. Data Simulation for ALTER TOURS Project
# Analyzing the Green and Digital readiness of 100 tourism agents 
# based on the project's core objectives (Sustainable & Digital Marketing)
set.seed(42)
alter_tours_data <- data.frame(
  agent_id = 1:100,
  digital_marketing_skills = runif(100, 1, 10), # Score in digital marketing competency
  green_marketing_skills = runif(100, 1, 10),    # Score in environmental stewardship/green marketing
  sustainable_tours_designed = rpois(100, lambda = 3) # Number of eco-friendly tours developed
)

# 2. Data Pre-processing for Machine Learning
# Scaling the data to ensure equal weight for all variables during clustering
scaled_data <- alter_tours_data %>%
  select(-agent_id) %>%
  scale()

# 3. Applying Machine Learning (K-Means Clustering)
# Implementing unsupervised learning to segment agents into 3 distinct groups
# to tailor skill certification and professional standards
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

# Adding cluster assignments back to the original dataset
alter_tours_data$cluster <- as.factor(kmeans_result$cluster)

# 4. Results Visualization
# Demonstrating the ability to present complex scientific data 
# to policy makers and stakeholders (Science-for-Policy)
fviz_cluster(kmeans_result, data = scaled_data,
             palette = "jco", 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal(),
             main = "ALTER TOURS: Agent Segmentation via Green & Digital Skills")

# 5. Descriptive Analytics per Cluster
# Synthesizing findings into actionable insights for project management
cluster_summary <- alter_tours_data %>%
  group_by(cluster) %>%
  summarise(
    avg_digital_score = mean(digital_marketing_skills),
    avg_green_score = mean(green_marketing_skills),
    total_sustainable_tours = sum(sustainable_tours_designed)
  )

print(cluster_summary)

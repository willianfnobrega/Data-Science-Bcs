library(dplyr)
library(ggplot2)
library(cluster)
library(knitr)

df_sale_encoded <- df_sale %>%
  select(Price, Size, Rooms, Suites, Parking, Condo, Elevator, Furnished, Swimming.Pool, New, District) %>%
  na.omit() %>%
  mutate(District = as.factor(District)) %>%
  mutate(across(District, ~ as.numeric(as.factor(.))))

scaled_features_sale <- scale(df_sale_encoded)

set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(scaled_features_sale, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

sil_scores <- sapply(2:10, function(k) {
  km <- kmeans(scaled_features_sale, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(scaled_features_sale))
  mean(ss[, 3])
})

plot(2:10, sil_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Score")

set.seed(123)
km <- kmeans(scaled_features_sale, centers = 5, nstart = 25)

df_sale$Cluster <- factor(km$cluster)

cluster_summary <- df_sale %>%
  group_by(Cluster) %>%
  summarise(across(c(Price, Size, Rooms, Suites, Parking, Condo, Elevator, Furnished, Swimming.Pool, New), 
                   \(x) mean(x, na.rm = TRUE)),
            Listings = n()) %>%
  arrange(Cluster)

kable(cluster_summary, caption = "Cluster Summary - Sale Apartments with District Encoded")

df_sale %>%
  group_by(Cluster, District) %>%
  summarise(Listings = n()) %>%
  slice_max(Listings, n = 1) %>%
  arrange(Cluster)


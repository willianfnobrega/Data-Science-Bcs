df_rent_encoded <- df_rent %>%
  select(Price, Size, Rooms, Suites, Parking, Condo, Elevator, Furnished, Swimming.Pool, New, District) %>%
  na.omit() %>%
  mutate(District = as.factor(District)) %>%
  mutate(across(District, ~ as.numeric(as.factor(.))))

scaled_features_rent <- scale(df_rent_encoded)

set.seed(123)
wss_rent <- sapply(1:10, function(k) {
  kmeans(scaled_features_rent, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss_rent, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

sil_scores_rent <- sapply(2:10, function(k) {
  km <- kmeans(scaled_features_rent, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(scaled_features_rent))
  mean(ss[, 3])
})

plot(2:10, sil_scores_rent, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Score")

set.seed(123)
km_rent <- kmeans(scaled_features_rent, centers = 5, nstart = 25)

df_rent$Cluster <- factor(km_rent$cluster)

cluster_summary_rent <- df_rent %>%
  group_by(Cluster) %>%
  summarise(across(c(Price, Size, Rooms, Suites, Parking, Condo, Elevator, Furnished, Swimming.Pool, New), 
                   \(x) mean(x, na.rm = TRUE)),
            Listings = n()) %>%
  arrange(Cluster)

kable(cluster_summary_rent, caption = "Cluster Summary - Rent Apartments with District Encoded")

df_rent %>%
  group_by(Cluster, District) %>%
  summarise(Listings = n()) %>%
  slice_max(Listings, n = 1) %>%
  arrange(Cluster)

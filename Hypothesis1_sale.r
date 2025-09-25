existing_pinheiros_sale <- df_sale %>%
  filter(Cluster == 5, District == "Pinheiros/São Paulo")

avg_price_cluster5 <- mean(existing_pinheiros_sale$Price, na.rm = TRUE)

new_pinheiros_sale <- data.frame(
  Price = rep(avg_price_cluster5, 100),
  Size = sample(50:80, 100, replace = TRUE),
  Rooms = rep(2, 100),
  Toilets = rep(1, 100),
  Suites = rep(0, 100),
  Parking = rep(1, 100),
  Condo = sample(150:300, 100, replace = TRUE),
  Elevator = rep(0, 100),
  Furnished = rep(0, 100),
  Swimming.Pool = rep(0, 100),
  New = rep(1, 100),
  District = rep("Pinheiros", 100),
  Negotiation.Type = rep("sale", 100),
  Property.Type = rep("apartment", 100),
  Latitude = mean(existing_pinheiros_sale$Latitude, na.rm = TRUE),
  Longitude = mean(existing_pinheiros_sale$Longitude, na.rm = TRUE)
)

df_sale_newSamples <- bind_rows(df_sale, new_pinheiros_sale)


df_sale_newSamples <- bind_rows(df_sale, new_pinheiros_sale)

before_sale <- log(existing_pinheiros_sale$Price)
after_sale <- log(new_pinheiros_sale$Price)

t_price_sale <- t.test(before_sale, after_sale, alternative = "greater", var.equal = FALSE)

t_price_sale


set.seed(123)

before_price_sale <- existing_pinheiros_sale$Price
after_price_sale <- new_pinheiros_sale$Price

combined_price_sale <- c(before_price_sale, after_price_sale)
n_before_sale <- length(before_price_sale)
n_after_sale <- length(after_price_sale)

obs_diff_price_sale <- mean(before_price_sale) - mean(after_price_sale)

n_perm <- 10000
perm_diffs_price_sale <- replicate(n_perm, {
  shuffled <- sample(combined_price_sale)
  mean(shuffled[1:n_before_sale]) - mean(shuffled[(n_before_sale+1):(n_before_sale+n_after_sale)])
})

p_value_price_sale <- mean(perm_diffs_price_sale >= obs_diff_price_sale)
obs_diff_price_sale
p_value_price_sale

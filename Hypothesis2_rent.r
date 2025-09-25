existing_pinheiros_rent <- df_rent %>%
  filter(Cluster == 5, District == "Pinheiros/São Paulo")

new_pinheiros_rent <- data.frame(
  Price = rep(1200, 100),
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
  District = rep("Pinheiros/São Paulo", 100),
  Negotiation.Type = rep("rent", 100),
  Property.Type = rep("apartment", 100),
  Latitude = mean(existing_pinheiros_rent$Latitude, na.rm = TRUE),
  Longitude = mean(existing_pinheiros_rent$Longitude, na.rm = TRUE)
)

t_price_rent <- t.test(existing_pinheiros_rent$Price, new_pinheiros_rent$Price, alternative = "greater", var.equal = FALSE)
t_size_rent <- t.test(existing_pinheiros_rent$Size, new_pinheiros_rent$Size, alternative = "less", var.equal = FALSE)
t_condo_rent <- t.test(existing_pinheiros_rent$Condo, new_pinheiros_rent$Condo, alternative = "greater", var.equal = FALSE)

t_price_rent
t_size_rent
t_condo_rent


set.seed(123)

before_price_rent <- existing_pinheiros_rent$Price
after_price_rent <- new_pinheiros_rent$Price

combined_price_rent <- c(before_price_rent, after_price_rent)
n_before_price <- length(before_price_rent)
n_after_price <- length(after_price_rent)

obs_diff_price <- mean(before_price_rent) - mean(after_price_rent)

n_perm <- 10000
perm_diffs_price <- replicate(n_perm, {
  shuffled <- sample(combined_price_rent)
  mean(shuffled[1:n_before_price]) - mean(shuffled[(n_before_price+1):(n_before_price+n_after_price)])
})

p_value_price <- mean(perm_diffs_price >= obs_diff_price)
obs_diff_price
p_value_price

before_size_rent <- existing_pinheiros_rent$Size
after_size_rent <- new_pinheiros_rent$Size

combined_size_rent <- c(before_size_rent, after_size_rent)
n_before_size <- length(before_size_rent)
n_after_size <- length(after_size_rent)

obs_diff_size <- mean(before_size_rent) - mean(after_size_rent)

perm_diffs_size <- replicate(n_perm, {
  shuffled <- sample(combined_size_rent)
  mean(shuffled[1:n_before_size]) - mean(shuffled[(n_before_size+1):(n_before_size+n_after_size)])
})

p_value_size <- mean(perm_diffs_size >= obs_diff_size)
obs_diff_size
p_value_size

before_condo_rent <- existing_pinheiros_rent$Condo
after_condo_rent <- new_pinheiros_rent$Condo

combined_condo_rent <- c(before_condo_rent, after_condo_rent)
n_before_condo <- length(before_condo_rent)
n_after_condo <- length(after_condo_rent)

obs_diff_condo <- mean(before_condo_rent) - mean(after_condo_rent)

perm_diffs_condo <- replicate(n_perm, {
  shuffled <- sample(combined_condo_rent)
  mean(shuffled[1:n_before_condo]) - mean(shuffled[(n_before_condo+1):(n_before_condo+n_after_condo)])
})

p_value_condo <- mean(perm_diffs_condo >= obs_diff_condo)
obs_diff_condo
p_value_condo

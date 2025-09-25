new_rent_apartments <- data.frame(
  Price = rep(910, 100),
  Size = sample(91:119, 100, replace = TRUE),
  Rooms = rep(2, 100),
  Toilets = rep(1, 100),
  Suites = rep(0, 100),
  Parking = rep(1, 100),
  Condo = sample(100:500, 100, replace = TRUE),
  Elevator = rep(0, 100),
  Furnished = rep(0, 100),
  Swimming.Pool = rep(0, 100),
  New = rep(1, 100),
  District = rep("Morumbi/São Paulo", 100),
  Negotiation.Type = rep("rent", 100),
  Property.Type = rep("apartment", 100),
  Latitude = mean(df_rent$Latitude[df_rent$District == "Morumbi/São Paulo"], na.rm = TRUE),
  Longitude = mean(df_rent$Longitude[df_rent$District == "Morumbi/São Paulo"], na.rm = TRUE)
)

df_rent <- bind_rows(df_rent, new_rent_apartments)

morumbi_rent <- df_rent %>%
  filter(Cluster == 2, District == "Morumbi/São Paulo") %>%
  pull(Price)

others_rent <- df_rent %>%
  filter(!(District == "Morumbi/São Paulo" & Cluster == 2)) %>%
  pull(Price)

before <- log(morumbi_rent)
after <- log(c(morumbi_rent, rep(910, 100)))

t_result_rent <- t.test(before, after[(length(before)+1):length(after)], alternative = "greater", var.equal = FALSE)

t_result_rent




set.seed(123)

before2 <- df_rent$Price[df_rent$Cluster == 2 & df_rent$District == "Morumbi/São Paulo"]
after2 <- new_rent_apartments$Price

before2 <- na.omit(before2)
after2 <- na.omit(after2)

combined2 <- c(before2, after2)
n_before2 <- length(before2)
n_after2 <- length(after2)

obs_diff2 <- mean(before2) - mean(after2)

n_perm2 <- 10000
perm_diffs2 <- replicate(n_perm2, {
  shuffled <- sample(combined2)
  mean(shuffled[1:n_before2]) - mean(shuffled[(n_before2+1):(n_before2+n_after2)])
})

p_value2 <- mean(perm_diffs2 >= obs_diff2)
obs_diff2
p_value2




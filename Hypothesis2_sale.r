before_size_sale <- existing_pinheiros_sale$Size
after_size_sale <- new_pinheiros_sale$Size

t_size_sale <- t.test(before_size_sale, after_size_sale, alternative = "less", var.equal = FALSE)
t_size_sale



set.seed(123)

before_price_sale <- existing_pinheiros_sale$Price
after_price_sale <- new_pinheiros_sale$Price
before_size_sale <- existing_pinheiros_sale$Size
after_size_sale <- new_pinheiros_sale$Size
before_condo_sale <- existing_pinheiros_sale$Condo
after_condo_sale <- new_pinheiros_sale$Condo

n_perm <- 10000

perm_test <- function(before, after, n_perm, alternative = "greater") {
  before <- na.omit(before)
  after <- na.omit(after)
  combined <- c(before, after)
  n_before <- length(before)
  n_after <- length(after)
  obs_diff <- mean(before) - mean(after)
  perm_diffs <- replicate(n_perm, {
    shuffled <- sample(combined)
    mean(shuffled[1:n_before]) - mean(shuffled[(n_before+1):(n_before+n_after)])
  })
  if(alternative == "less") {
    p_val <- mean(perm_diffs <= obs_diff)
  } else {
    p_val <- mean(perm_diffs >= obs_diff)
  }
  return(list(obs_diff = obs_diff, p_value = p_val))
}

price_result_sale <- perm_test(before_price_sale, after_price_sale, n_perm, "greater")
size_result_sale <- perm_test(before_size_sale, after_size_sale, n_perm, "less")
condo_result_sale <- perm_test(before_condo_sale, after_condo_sale, n_perm, "greater")

price_result_sale
size_result_sale
condo_result_sale


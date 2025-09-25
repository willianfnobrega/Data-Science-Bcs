library(dplyr)
library(ggplot2)

df_sale %>%
  group_by(District) %>%
  summarise(median_price = median(Price, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(median_price)) %>%
  top_n(15, median_price) %>%
  ggplot(aes(x = reorder(District, median_price), y = median_price, fill = count)) +
  geom_col() +
  geom_text(aes(label = count), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top Districts by Median Sale Price",
       x = "District", y = "Median Sale Price") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

df_rent %>%
  group_by(District) %>%
  summarise(median_price = median(Price, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(median_price)) %>%
  top_n(15, median_price) %>%
  ggplot(aes(x = reorder(District, median_price), y = median_price, fill = count)) +
  geom_col() +
  geom_text(aes(label = count), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Top Districts by Median Rent Price",
       x = "District", y = "Median Rent Price") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")


df_rent %>%
  group_by(District) %>%
  summarise(median_price = median(Price, na.rm = TRUE),
            count = n()) %>%
  arrange(median_price) %>%
  top_n(-15, median_price) %>%
  ggplot(aes(x = reorder(District, median_price), y = median_price, fill = count)) +
  geom_col() +
  geom_text(aes(label = count), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Least Expensive Districts by Median Rent Price",
       x = "District", y = "Median Rent Price") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")


df_sale %>%
  group_by(District) %>%
  summarise(median_price = median(Price, na.rm = TRUE),
            count = n()) %>%
  arrange(median_price) %>%
  top_n(-15, median_price) %>%
  ggplot(aes(x = reorder(District, median_price), y = median_price, fill = count)) +
  geom_col() +
  geom_text(aes(label = count), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Least Expensive Districts by Median Sale Price",
       x = "District", y = "Median Sale Price") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")


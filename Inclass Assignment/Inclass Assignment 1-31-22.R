library("ggplot2")
data("diamonds")
View(diamonds)
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + coord_cartesian(ylim = c(0,60))
library("dplyr")
unusual_y <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)
unusual_y

unusual_x <- diamonds %>%
  filter(x > 20 | x < 3) %>%
  arrange(x)
unusual_x

unusual_z <- diamonds %>%
  filter(z > 20 | z < 2) %>%
  arrange(z)
unusual_z


ggplot(diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 10) + coord_cartesian(ylim = c(0,1000), xlim = c(1450,1550))

unusual_price <- diamonds %>%
  filter(price > 1460 & price < 1540) %>%
  arrange(price)
unusual_price



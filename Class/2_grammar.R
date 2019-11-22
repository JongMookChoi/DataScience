library(tidyverse)

ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
  geom_point()

ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
  geom_line() +
  theme(legend.position = "none")

ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
  geom_bar(stat = "identity", position = "identity", fill = NA) +
  theme(legend.position = "none")

ggplot(mpg, aes(displ, hwy, color = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~year)

ggplot(mpg, aes(trans, cty)) +
  geom_point() +
  stat_summary(geom = "point", fun.y = "mean", color = "red", size = 4)
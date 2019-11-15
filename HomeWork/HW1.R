library(babynames)
library(mdsr)
library(Hmisc)
library(tidyverse)

baby_data <- make_babynames_dist()
baby_data

Josephs <- filter(baby_data, name == "Joseph" &  sex == "M")
arrange(baby_data, desc(year))

median <- wtd.quantile(Josephs$year, Josephs$est_alive_today, probs = 0.5)

ggplot(data = Josephs, aes(x = year)) +
  geom_bar(aes(y = alive_prob * count_thousands), fill = "skyblue", color = "white", stat = "identity", width = 0.7) +
  geom_line(aes(y = count_thousands), size = 2) +
  xlab(NULL) +
  ylab(NULL) +
  geom_bar(aes(y = ifelse(year == median, est_alive_today / 1000, 0)), fill = "#0099CC", stat = "identity", width = 0.7) +
  ggtitle(label = "Age Distribution of American Boys Named Joseph", subtitle = "By year of birth") +
  geom_text(x = 1915, y = 13, label = "Number of Josephs\nborn each year\nestimated to be alive\non Jan.1, 2014", color = "#0099CC", size = 5) +
  geom_text(x = 1935, y = 30, label = "Number of Josephs\nborn each year", size = 5) +
  geom_text(x = 2003, y = 40, label = "The median\nliving Joseph\nis 39 years old", color = "darkgray", size = 5) +
  geom_curve(x = 1997, xend = 1975, y = 42, yend = 24, arrow = arrow(length = unit(0.3,"cm")), curvature = 0.5) +
  scale_y_continuous(limits = c(0,42), labels = c("0","10","20","30","40k")) +
  scale_x_continuous(limits = c(1899,2011), breaks = seq(1900,2010,10), labels = c("1900","`10","`20","`30","`40","`50","60","`70","`80","`90","2000","`10")) +
  geom_abline(intercept = 0, slope = 0, size = 1) +
  theme(panel.background = element_rect(fill = "grey95"),
        plot.background = element_rect(fill = "grey95"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "lightgray"),
        panel.grid.major.x = element_line(color = "lightgray"),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 15))

com_m <- baby_data %>% 
  filter(sex == "M") %>% 
  group_by(name) %>% 
  dplyr :: summarize(N = n(), est_sum_alive = sum(est_alive_today),
                     q1_age = 2014 - tryCatch(wtd.quantile(year, est_alive_today, probs = 0.75),
                                              error = function(e) 0),
                     median_age = 2014 - tryCatch(wtd.quantile(year, est_alive_today, probs = 0.5),
                                              error = function(e) 0),
                     q3_age = 2014 - tryCatch(wtd.quantile(year, est_alive_today, probs = 0.25),
                                              error = function(e) 0)) %>% 
  arrange(desc(est_sum_alive)) %>%
  head(25) %>%
  arrange(median_age)

ggplot(data = com_m, aes(x = reorder(name, -median_age), y = median_age)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle(label = "Median Ages for Males With the 25 Most\nCommon Names",
          subtitle = "Among Americans estimated to be alive as of Jan.1, 2014") +
  geom_linerange(aes(ymin = q1_age, ymax = q3_age), color = "skyblue", size = 8, alpha = 0.8) +
  geom_point(fill = "red", color = "white", size = 4, shape = 21) +
  geom_point(aes(y = 59, x = 24), fill = "red", color = "white", size = 4, shape = 21) +
  geom_text(aes(y = 61, x = 24), label = "median") +
  geom_text(aes(y = 27, x = 16), label = "25th") +
  geom_text(aes(y = 52, x = 16), label = "75th percentile") +
  geom_point(aes(y = 25, x = 16), shape = 17, size = 3) +
  geom_point(aes(y = 56, x = 16), shape = 17, size = 3) +
  theme(panel.background = element_rect(fill = "grey95"),
        plot.background = element_rect(fill = "grey95"),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 15)) +
  coord_flip()

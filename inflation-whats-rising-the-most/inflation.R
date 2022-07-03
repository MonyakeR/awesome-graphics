library(tidyverse)
library(lubridate)
library(ggthemes)

# load the data
inflation <- read_csv('inflation-whats-rising-the-most/South African CPI headline year-on-year rates.csv')

# data wrangling
inflation <- inflation %>% 
  pivot_longer(
    cols = !Year,
    names_to = "Month",
    values_to = "inflation_rate"
  ) %>% 
  mutate(
    date = ym(paste0(Year,"-", Month)),
    date = ceiling_date(date, "month") - days(1)
  ) %>% 
  filter(!is.na(inflation_rate)) %>% 
  select(date, inflation_rate)

# plot
plt <- inflation %>% 
  ggplot(aes(x = date, y = inflation_rate)) + 
  geom_area(fill = "#85b4d3") +
  geom_line(color = "#0b6ba6", size = 1.5) +
  scale_x_date(
    limits = c(as.Date("2010-01-31"), as.Date("2022-06-30")),
    expand = c(0, 0)
  ) + 
  geom_hline(yintercept = 6, linetype="dashed", color = "#cf191e") +
  scale_y_continuous(
    limits = c(0, 8),
    labels = scales::percent_format(scale = 1)
  ) +
  geom_hline(yintercept = 0, size = 1.1) + 
  labs(
    title = "Annual consumer price inflation was 6,5% in May 2022",
    subtitle = "For the month of May 2022, the annual consumer price inflation broke through the upper limit of the \nSouth African Reserve Bank's monetary policy target range of 6%.",
    caption = "Source: Stats SA \n Graphic: @RetseMonyake"
  ) +
  annotate(
    geom = "curve", x = as.Date('2021-12-31'), y = 7, xend = as.Date('2022-04-30'), yend = 6.5, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate('point', x = as.Date('2022-05-31'), y = 6.5, color = "#0b6ba6", size=3) +
  annotate('label', x = as.Date('2021-12-31'), y = 7, label = "May 2022 \n6.5%", hjust = "right") + 
  annotate('label', x = as.Date('2019-09-30'), y = 5.7, label = "SARB target upper limit (6%)") +
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.minor = element_blank())

# save plot  
ggsave('inflation_plot.png')

# what's rising the most?

item_data <- read_csv("inflation-whats-rising-the-most/item inflation.csv")

# plot
item_data %>%
  mutate(
    inflation_perc = paste0(Inflation, "%"), 
    Items = factor(Items)
  ) %>% 
  ggplot(aes(x = Inflation, y = reorder(Items, Inflation))) + 
  geom_col(fill = "#0b6ba6") +
  geom_text(
    aes(label = inflation_perc), 
    hjust = 1, nudge_x = -0.2,
    color = "white",
    size = 4, fontface = "bold"
  ) +
  geom_vline(xintercept = 0) + 
  labs(
    title = "The biggest price changes between April and May",
    subtitle = "Percentage change in price indices, May 2022 compared with April 2022",
    caption = "Source: Stats SA \n Graphic: @RetseMonyake"
  ) + 
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        panel.border = element_blank())

# save plot
ggsave('item_inflation_plot.png')

# load the libraries
library(readr)
library(dplyr)
library(ggplot2)
library(bbplot)
library(scales)


# read in the data
unemployment <- read_csv("Data-and-Code/sa-unemployment/sa_unemployment.csv")

# prepare the data
unemployment <- unemployment %>% 
  mutate(unemployment_rate = unemployment_rate / 100)

# make plot
unemployment_plot <- ggplot(unemployment, aes(x = date, y = unemployment_rate)) + 
  geom_line(colour = "#1380A1", size = 1) + 
  geom_hline(yintercept = 0, size = 1, colour="#333333") + 
  geom_label(aes(x = as.Date("2021-10-01",  "%Y-%m-%d"), y = 0.349, label = "Q3:2021\n 34.9%"), 
             hjust = 0.3,
             vjust = 1.2,
             colour = "#990000", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 4.5) + 
  geom_point(aes(x = as.Date("2021-10-01",  "%Y-%m-%d"), y = 0.349), colour = "#990000", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.4)) + 
  scale_x_date(limits = c(as.Date("2008-01-01",  "%Y-%m-%d"), as.Date("2022-01-01",  "%Y-%m-%d"))) + 
  bbc_style() +
  labs(
    title = "South Africa Unemployment Rate",
    subtitle = "The official unemployment rate was 34,9% in Q3:2021"
  )

# save plot
finalise_plot(
  plot_name = unemployment_plot,
  source = "Source: Statistics South Africa",
  save_filepath = "Data-and-Code/sa-unemployment/sa_unemployment.png",
  width_pixels = 810,
  height_pixels = 550
)

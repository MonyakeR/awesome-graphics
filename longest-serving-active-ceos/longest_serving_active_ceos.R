# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(gt)

# load the data
ceo_tenure <- read_csv("longest-serving-active-ceos/longest_serving_active_ceos.csv")

# function for creating the bar chart in the table
bar_chart <- function(label, height = "15px",fill = "#3B82F6", background = "white") {
  
  bar <- glue::glue(
    "<div style='background:{fill};width:{label}%;height:{height};'></div>"
  )
  chart <- glue::glue(
    "<div style='flex-grow:1;margin-left:8px;background:{background};height:{height};'>{bar}</div>"
  )
  glue::glue(
    "<div style='display:flex;align-items:left';>{label}% {chart}</div>"
  ) %>%
    gt::html()
  
}





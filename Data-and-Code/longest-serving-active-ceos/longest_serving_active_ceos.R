# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(gt)
library(stringr)

# load the data
ceo_tenure <- read_csv("Data & Code/longest-serving-active-ceos/longest_serving_active_ceos.csv")

# function to create bar chart
bar_chart <- function(label, height = "15px",fill = "#00bfc4") {
  
  bar <- glue::glue(
    "<div style='background:{fill};width:{label}px;height:{height};'></div>"
  )
  glue::glue(
    "<div style='display:flex;align-items:right';>{bar}</div>"
  ) %>%
    gt::html()
  
}

# create a column for the bar chart
ceo_tenure <- ceo_tenure %>% 
  mutate(
    duration_var = round(Duration/max(Duration) * 100,0),
    duration_bar = purrr::map(
      duration_var,
      ~bar_chart(label = .x)
    )
  ) %>% 
  select(CEO, Company, Since, Duration, duration_bar)

# add hyphen in front of names 
ceo_tenure <- ceo_tenure %>% 
  mutate(CEO = paste(CEO, "-"))


# Combine text into div containers
# and then “stack” the text on top of each other with alternating color. 
stack_function <- function(x) {
  
  CEO <- sub(pattern = " -.*", replacement = "", x = x)
  Company <- sub(pattern = ".*- ", replacement = "", x = x)
  
  glue::glue(
    "<div style='line-height:12px'>
    <span style='font-weight:bold;font-variant:small-caps;font-size:14px'>
    {CEO}</div>
    <div style='line-height:12px'>
    <span style ='font-weight:bold;color:grey;font-size:11px'>
    {Company}</span></div>"
  )
}

# create table
ceo_tenure_tbl <- ceo_tenure %>% 
  gt() %>% 
  tab_header(
    title = md("**Longest Serving Active CEOs in the S&P 500**"),
    subtitle = md("Warren Buffett is the longest serving leader having maintained his position for over half a century")
  ) %>%
  cols_merge(
    columns = c(CEO, Company)
  ) %>% 
  text_transform(
    locations = cells_body(
      columns = c(CEO)
    ),
    fn = stack_function
  ) %>% 
  cols_label(
    Duration = "Duration in years",
    duration_bar = ""
  ) %>% 
  cols_width(
    Since ~ px(80)
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  opt_row_striping() %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        weight = px(3),
        color = "white" # remove subtitle bottom border.
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden"
  ) %>% 
  tab_source_note(
    source_note = md("Source: Visual Capitalist, as of 2021")
  )
  
# show table
ceo_tenure_tbl

# save table
gtsave(ceo_tenure_tbl, "longest-serving-active-ceos/longest_serving_active_ceos.png")


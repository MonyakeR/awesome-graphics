# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(gt)
library(stringr)

# load the data
ceo_tenure <- read_csv("Data-and-Code/longest-serving-active-ceos/longest_serving_active_ceos.csv")

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
  opt_table_font(
    font = list(
      google_font(name = "Archivo")
    ),
    weight = 400
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
  tab_style(
    style = list(
      cell_borders(
        sides = "top", color = "black", weight = px(0)
      ),
      cell_text(
        transform = "uppercase",
        v_align = "bottom",
        size = px(14),
        weight = 200
      )
    ),
    locations = cells_column_labels(columns = gt::everything())
  ) %>%
  tab_options(
    column_labels.background.color = "white",
    data_row.padding = px(3),
    heading.border.bottom.style = "none",
    table.border.top.width = px(3),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    column_labels.font.weight = "normal",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    stub.border.color = "white",
    stub.border.width = px(0),
    source_notes.font.size = 12,
    source_notes.border.lr.style = "none",
    table.font.size = 16,
    heading.align = "left"
  ) %>% 
  opt_css(
    "tbody tr:last-child {
        border-bottom: 2px solid #ffffff00;
    }
    ",
    add = TRUE
  ) %>%
  tab_source_note(
    source_note = md("*Source: Visual Capitalist, as of 2021*")
  ) %>% 
  tab_source_note(source_note = "Graphic: @RetseMonyake")
  
# show table
ceo_tenure_tbl

# save table
gtsave(ceo_tenure_tbl, "Data-and-Code/longest-serving-active-ceos/longest_serving_active_ceos.png")


library(readr)
library(dplyr)
library(stringr)
library(gt)

nba <- read_csv("https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/nba-arena-attendance/nba_arena_attendance.csv")

names(nba) <- c("team","fans_allowed", "maximum_capacity")

# function for creating the bar chart in the table, adapted from Thomas Mock
# https://themockup.blog/static/gt-cookbook-advanced.html
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

# Calculate the proportion of fans allowed 
nba <- nba %>% 
  mutate(proportion = round((fans_allowed / maximum_capacity), 2), # for sorting
         proportion_bar = purrr::map(
           round((fans_allowed / maximum_capacity), 2),
           ~bar_chart(label = .x * 100, fill = "#3B82F6", background = "#EBF5FB")
         )) %>% 
  arrange(desc(proportion)) %>% 
  select(-proportion)

# Add logo names
nba <- nba %>%
  mutate(team_logo = paste0((tolower(word(team, -1))), "-logo.svg")) %>% 
  select(team_logo, dplyr::everything())

images_url <- "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/nba-arena-attendance/images/"

nba_tbl <- gt(nba) %>% 
  fmt_number(
    columns = c(fans_allowed, maximum_capacity),
    sep_mark = ",",
    decimals = 0
  ) %>% 
  tab_header(
    title = "The number of fans allowed at each stadium in the 2021 NBA Playoffs",
    subtitle = md("*Last updated June 01, 2021*")
  ) %>% 
  # add logos
  text_transform(
    locations = cells_body(columns = c(team_logo)),
    fn = function(x) {
      web_image(
        url =  paste0(images_url, x)
      )
    }
  ) %>% 
  cols_label(
    team_logo = "Team",
    team = " ",
    fans_allowed = "Fans Allowed",
    maximum_capacity = "Maximum Capacity",
    proportion_bar = "Percentage Allowed"
  ) %>% 
  cols_align(
    align = "left",
    columns = c(fans_allowed, maximum_capacity)
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
    source_note = md("Source: PerThirtySix, NBA")
    )

nba_tbl

# safe the table
gtsave(nba_tbl, "nba_arena_attendance.html")
#


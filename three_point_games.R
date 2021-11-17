library(dplyr)
library(gt)


# player data
player <- c("Stephen Curry", "James Harden", "Damian Lillard", "Klay Thompson", "JR Smith", "Kobe Bryant")
games <- c(37, 9, 9, 7, 5, 4)

three_pointers <- tibble(player, games)


# function for creating the bar chart in the table, adapted from Thomas Mock
# https://themockup.blog/static/gt-cookbook-advanced.html
bar_chart <- function(label, height = "15px",fill = "#3B82F6", background = "white") {
  bar <- glue::glue(
    "<div style='background:{fill};width:{label}px;height:{height};'></div>"
  )
  glue::glue(
    "<div style='display:flex;align-items:right';>{bar}</div>"
  ) %>%
    gt::html()
  
}

three_pointers <- three_pointers %>% 
  mutate(
    #games_var = round(games/max(games) * 100,0),
    no_of_games = purrr::map(
      games,
      ~bar_chart(label = .x)
    )
  ) %>% 
  select(player, games, no_of_games)

three_pointers_tbl <- three_pointers %>% 
  gt() %>% 
  cols_label(
    player = "Name",
    games = "Number of Games",
    no_of_games = " "
  ) %>% 
  cols_width(
    no_of_games ~ px(100)
  ) %>% 
  cols_width(
    games ~ px(150)
  )
  

# show the table
three_pointers_tbl


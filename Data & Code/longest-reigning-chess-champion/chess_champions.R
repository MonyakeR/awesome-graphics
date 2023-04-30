library(tidyverse)
library(gt)
library(gtExtras)

# load the data
# source: https://worldchampionship.fide.com/chess-champions
chess_champions <- read_csv("Data & Code/longest-reigning-chess-champion/chess_champions - fide.csv")

gt_tbl <- chess_champions %>% 
  arrange(desc(`Reign Duration`)) %>% 
  mutate(reign_bar = `Reign Duration`) %>% 
  select(Champion, Federation, `Reign Duration`, reign_bar, Years) %>% 
  gt() %>% 
  tab_header(
    title = "The longest reigning chess champion",
    subtitle = md("**Emanuel Lasker** was the World Champion for **27** years 
                  consecutively from **1894** to **1921**, the longest reign
                  of a World Champion")
  ) %>% 
  gt_plt_bar(column = reign_bar, color = "#0b6ba6") %>% 
  cols_label(reign_bar = "") %>% 
  gt_merge_stack(col1 = Champion, col2 = Federation) %>%
  cols_width(
    `Reign Duration` ~ px(120)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Years),
      rows = Champion == "Magnus Carlsen"
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Champion, `Reign Duration`),
      rows = Champion == "Emmanuel Lasker"
    )
  ) %>% 
  tab_source_note(
    source_note = "Source: FIDE"
  ) %>% 
  tab_footnote(
    footnote = "On July 19, 2022, Magnus Carlsen announced that he would not 
                defend his title in the 2023 World Championship",
    locations = cells_body(
      columns = Years, 
      rows = Champion == "Magnus Carlsen"
    )
  ) %>% 
  tab_source_note(
    source_note = md("**Table**: @RetseMonyake")
  ) %>% 
  gt_theme_538()

gtsave(gt_tbl, "Data & Code/longest-reigning-chess-champion/chess_champions.png")


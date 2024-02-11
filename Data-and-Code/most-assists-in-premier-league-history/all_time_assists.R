# this script analyses and visualises the top 10 players with the most
# assists in the premier league

# load the libraries ----
library(tidyverse)
library(gt)
library(gtExtras)
library(glue)

# source files ----
source("utils/helper_functions.R")

# load the data ----
assists <- read_csv("Data-and-Code/most-assists-in-premier-league-history/data/Most assists in Premier League history.csv")

# data wrangling ----
assists_tbl <- assists %>% 
  mutate(
    assists_per_game = Assists/`Matches Played`,
    assists_plot = Assists
  ) %>% 
  select(Player, `Matches Played`, assists_plot, everything())

# graphic creation ----
tbl_assists <- gt(data = assists_tbl, id = "assists") %>% 
  tab_header(
    title = md("**Premiar league all time assists**")
  ) %>%
  cols_label(
    assists_per_game = "Assists Per Game",
    assists_plot = md("**Total Assists**"),
    Assists = ""
  ) %>% 
  gt_plt_bar(
    column = assists_plot, 
    color = "#40c1c9",
  ) %>% 
  fmt_number(
    columns = c("assists_per_game"),
    decimals = 2
  ) %>% 
  cols_width(
    assists_plot ~ px(150),
    `Matches Played` ~ px(180),
    assists_per_game ~ px(150)
  ) %>% 
  cols_align(
    columns = c(Assists),
    align = "left"
  ) %>% 
  gt_color_rows(columns = c("assists_per_game"), palette = "ggsci::teal_material") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Assists)
  ) %>% 
  tab_source_note(
    source_note = md("*Source: Premier League, As of **11 February 2024***")
  ) %>% 
  tab_source_note(source_note = md("*Graphic: @RetseMonyake*")) %>% 
  gt_theme_custom()

  # save the graphic ----
gtsave_extra(tbl_assists, "Data-and-Code/most-assists-in-premier-league-history/all_time_assists.png")  

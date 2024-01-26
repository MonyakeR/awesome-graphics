# this script explores and visualises the deloitte football money league data

library(tidyverse)
library(gt)
library(gtExtras)
library(glue)

# source files
source("utils/helper_functions.R")

# load the data
money_league <- read_csv("Data-and-Code/deloitte-football-money-league-2024/data/Deloitte Football Money League 2024.csv")

tbl_names <- c(
  "Club",
  "League",
  "Revenue '23",
  "Revenue '22",
  "% Change"
)

names(money_league) <- tbl_names

tbl_money <- money_league %>%
  mutate(
    rank = row_number(),
    revenue_23 = `Revenue '23`,
    `% Change` = `% Change`/ 100,
    badge = glue("Data-and-Code/deloitte-football-money-league-2024/team-badges/{Club}.svg")
  ) %>%
  select(rank, badge, Club, League, `Revenue '23`, revenue_23, everything()) %>% 
  head(n = 15) %>% 
  gt(id = "money_league") %>% 
  tab_header(
    title = "Deloitte Football Money League 2024",
    subtitle = "Real Madrid have eclipsed Manchester City to become the highest revenue generating football club in 2022/23"
  ) %>% 
  cols_label(
    rank = "",
    badge = "",
    `Revenue '23` = md("**Revenue '23**"),
    revenue_23 = ""
  ) %>% 
  fmt_currency(
    columns = c(`Revenue '23`, `Revenue '22`),
    currency = "EUR"
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = `% Change`,
      rows = `% Change` < 0
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "seagreen")
    ),
    locations = cells_body(
      columns = `% Change`,
      rows = `% Change` > 0
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = 600)
    ),
    locations = cells_body(
      columns = "Revenue '23"
    )
  ) %>% 
  fmt_percent(
    columns = `% Change`,
    decimals = 0
  ) %>% 
  gt_plt_bar(column = revenue_23, color = "#9fccd2",) %>% 
  gt_img_rows(columns = badge, img_source = "local") %>% 
  gt_merge_stack(col1 = Club, col2 = League) %>%  
  tab_source_note(source_note = md("*Source: Deloitte Football Money League*")) %>% 
  tab_source_note(source_note = "Graphic: @RetseMonyake")  %>% 
  gt_theme_custom()

gtsave_extra(tbl_money, "Data-and-Code/deloitte-football-money-league-2024/deloitte_football_money_league.png")  

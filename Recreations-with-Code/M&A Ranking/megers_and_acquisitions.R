library(tidyverse)
library(gt)
library(gtExtras)

# load the data

ma <- read_csv("Recreations-with-Code/M&A Ranking/data/Bloomberg M&A Ranking.csv")

# creaet the table 
tbl <- ma %>% 
  mutate(
    total_deal_plot = `Total Deal Value`,
    change_icon = case_when(
      Change == 0 ~ NA,
      Change > 0 ~ "caret-up",
      Change < 0 ~ "caret-down",
      .default = as.character(Change)
    )
  ) %>% 
  select(Rank, Bank, `Market Share`,total_deal_plot, everything()) %>% 
  gt() %>% 
  opt_row_striping() %>% 
  tab_header(
    title = md("**Goldman Sachs Loses Top M&A Ranking in First Half**"),
    subtitle = "Wall Street fortunes rise and fall amid difficult dealmaking environment"
  ) %>% 
  fmt_symbol_first(column = `Market Share`, suffix = "%") %>% 
  fmt_symbol_first(column = `Total Deal Value`, suffix = "B") %>% 
  fmt_currency(columns = `Total Deal Value`, rows = 1, currency = "USD") %>% 
  gt_plt_bar(
    column = total_deal_plot, 
    color = "#000000",
    #background = "#F0F0F0F0"
  ) %>% 
  cols_label(
    Rank = "",
    Bank = md("**Bank**"),
    `Market Share` = md("**Market Share**"),
    total_deal_plot = md("**Total Deal Value**"),
    `Total Deal Value` = "",
    Change = md("**Change**"),
    change_icon = ""
  ) %>%
  cols_width(
    total_deal_plot ~ px(150),
    Change ~ px(150)
  ) %>% 
  cols_align(
    columns = `Total Deal Value`,
    align = "left"
  ) %>% 
  gt_fa_column(
    column = change_icon,
    palette = c("caret-up" = "#00c88A", "caret-down" = "#FF415F")
  ) %>% 
  fmt_integer(
    columns = Change,
    rows = Change > 0,
    force_sign = TRUE
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "#00c88A")
    ),
    locations = cells_body(
      columns = Change,
      rows = Change > 0
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "#FF415F")
    ),
    locations = cells_body(
      columns = Change,
      rows = Change < 0
    )
  ) %>%
  tab_source_note(
    source_note = "Source: Bloomberg"
  ) %>% 
  tab_source_note(
    source_note = "Change compares 1H 2023 ranking to full year 2022 position."
  ) %>% 
  tab_options(
    table.font.names = c(
      "BWHaasDingbat",
      "BWHaasText",
      "Hiragino Kaku Gothic Pro",
      "Noto Sans Japanese",
      "Noto Sans Chinese",
      "Arial","sans-serif"
    ),
    heading.align = "left",
    heading.subtitle.font.size = 15.3,
    table.font.size = 14,
    table.font.weight = "450",
    table.font.color = "#111111",
    table.border.top.color = "#ffffff",
    table.border.bottom.color = "#ffffff",
    heading.border.bottom.color = "#ffffff",
    column_labels.border.bottom.color = "#000000",
    column_labels.border.bottom.width = 2,
    table_body.hlines.color = "#ffffff",
    row.striping.background_color = "#F0F0F0F0",
    table_body.border.bottom.color = "#ffffff",
    source_notes.font.size = 13
  ) 

gtsave(tbl, "Recreations-with-Code/M&A Ranking/bloomberg_ma_rankng.png")

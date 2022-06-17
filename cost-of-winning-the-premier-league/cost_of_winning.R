library(tidyverse)
library(gt)
library(gtExtras)

# load the data

prem_cost <- read_csv('cost-of-winning-the-premier-league/data/The cost of winning the Premier League (08_09 - 22_23).csv')

prem_cost %>% 
  mutate(expenditure_bar = Expenditure) %>% 
  gt() %>% 
  tab_header(
    title = "The cost of winning the Premier League",
    subtitle = "Besides Leicester, every club to have won the Premier League after the Abu Dhabi United Group bought Manchester City in 2008 has spent more than £1billion on transfers since that day"
  ) %>% 
  fmt_currency(
    columns = Expenditure,
    currency = "GBP",
    decimals = 2,
    suffixing = TRUE
  ) %>% 
  gt_plt_bar(column = expenditure_bar, color = "#1363DF") %>% 
  cols_label(
    expenditure_bar = ""
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = `Last title`,
      rows = club == "Manchester City"
    )
  ) %>% 
  tab_source_note(
    source_note = "Source: transfermarkt.com"
  ) %>% 
  tab_source_note(
    source_note = md("**Table:** @RetseMonyake")
  ) %>%
  gt_theme_538()

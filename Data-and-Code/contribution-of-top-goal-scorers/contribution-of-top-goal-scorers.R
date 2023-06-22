library(tidyverse)
library(gt)
library(gtExtras)


# load the data

contribution <- read_csv("Data-and-Code/contribution-of-top-goal-scorers/data/The Contribution of Top Goal Scorers.csv")


gt_tbl <- contribution %>%
  arrange(desc(Contribution)) %>% 
  mutate(
    Contribution_pct = Contribution * 100
  ) %>% 
  gt() %>% 
  tab_header(
    title = "The Proportional Contribution of Top Scorers on Team Goals in the Premier League (2022/23)",
    subtitle = md("**Harry Kane** leads the charge as **Tottenham Hotspur's** go-to goal scorer")
  ) %>% 
  gt_merge_stack(
    col1 = "Player",
    col2 = "Team"
  ) %>% 
  fmt_percent(
    columns = Contribution,
    decimals = 0,
  ) %>% 
  gt_plt_bar_pct(column = Contribution_pct, scaled = TRUE, fill = "#3fc1c9") %>% 
  cols_label(
    Contribution_pct = ""
  ) %>% 
  cols_width(
    Contribution_pct ~ px(150)
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = c("Player Goals Scored", "Team Goals Scored"),
      rows = `Player Goals Scored` == 36 & `Team Goals Scored` == 94 
    )
  ) %>% 
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = c("Contribution"),
      rows = Contribution > 0.40 
    )
  ) %>% 
  tab_source_note(
    source_note = md("*Source: premierleague.com*")
  ) %>% 
  tab_source_note(
    source_note = md("**Table**: @RetseMonyake")
  ) %>% 
  gt_theme_538()

gtsave(
  gt_tbl, 
  "Data-and-Code/contribution-of-top-goal-scorers/top_contributors.png", 
  expand = 15
)


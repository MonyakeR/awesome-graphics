library(tidyverse)
library(gt)
library(gtExtras)
library(stringr)

# load the data
top_scorers <- read_csv("premier-league-top-scorers-2022/data/Premier League Top Scorers (2021-2022) .csv")

# images
images_url <- "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/premier-league-top-scorers-2022/images/"

# creating the table
tbl <- top_scorers %>% 
  mutate(
    non_penalty_goals = Goals - PKs,
    last_name = str_extract(Player, '[^ ]+$'),
    last_name = if_else(last_name=="Bruyne", "DeBruyne", last_name),
    last_name = paste0(images_url, last_name, ".png"),
    PKs = ifelse(PKs==0, NA, PKs)
  ) %>% 
  filter(Goals > 12) %>% 
  group_by(last_name, Player, Club, Goals, Assists, Matches, Mins) %>% 
  summarise(all_goals = list(c(non_penalty_goals, PKs)), .groups = "drop") %>%
  select(last_name, Player, Club, Goals, all_goals, everything()) %>% 
  arrange(desc(Goals)) %>% 
  gt() %>% 
  tab_header(
    title = "Premier League Top Scorers (2021-2022)",
    subtitle = "Salah and Son share Golden Boot with 23 goals scored. While 5 of Salah's goals come from the penalty spot, none of Son’s 23 goals have come via the penalty spot."
  ) %>% 
  gt_merge_stack(col1 = Player, col2 = Club) %>% 
  gt_plt_bar_stack(
    position = "stack",
    column = all_goals,
    palette = c("#2596be", "#be4d25"),
    labels = c("Non-penalty goals", "Penalty goals"),
    width = 80
  ) %>% 
  fmt_number(
    columns = Mins,
    decimals = 0,
    use_seps = TRUE,
    sep_mark = ","
  ) %>% 
  cols_width(
    last_name ~ px(50),
    Matches ~px(100),
    Player ~px(150),
    Goals ~px(80),
    all_goals ~px(290),
    Assists ~px(80),
    Matches ~px(80),
    Mins ~ px(80)
  ) %>% 
  data_color(
    columns = c(Assists, Matches, Mins),
    colors = scales::col_numeric(palette = c("#e9f5f9", "#2596be"),
                                 domain = NULL)
  ) %>% 
  cols_align(
    align = c("left"),
    columns = c(Assists, Matches, Mins)
  ) %>% 
  gt_img_rows(columns = last_name) %>%
  cols_label(last_name = "") %>%
  tab_source_note(
    source_note = md("Source: *premierleague.com*")
  ) %>% 
  tab_source_note(
    source_note = md("**Table:** @RetseMonyake")
  ) %>% 
  gt_theme_538()
  
tbl

gtsave(tbl, "prem_top_scorers.png")

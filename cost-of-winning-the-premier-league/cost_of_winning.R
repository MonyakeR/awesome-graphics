library(tidyverse)
library(gt)
library(gtExtras)

# load the data

prem_cost <- read_csv('cost-of-winning-the-premier-league/data/The cost of winning the Premier League (08_09 - 22_23).csv')

image_urls <- c(
  "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/cost-of-winning-the-premier-league/images/man_city.png",
  "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/cost-of-winning-the-premier-league/images/chelsea.png",
  "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/cost-of-winning-the-premier-league/images/man_united.png",
  "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/cost-of-winning-the-premier-league/images/liverpool.png",
  "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/cost-of-winning-the-premier-league/images/leicester.png"
)

prem_cost$club_images <- image_urls

tbl <- prem_cost %>% 
  select(club_images, everything()) %>% 
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
  gt_img_rows(columns = club_images, height = 30) %>%
  cols_align(
    align = c("center"),
    columns = club_images
  ) %>% 
  cols_align(
    align = c("left"),
    columns = `Titles since 08/09`
  ) %>% 
  cols_label(
    expenditure_bar = "",
    club_images = ""
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

# save table
gtsave(tbl, "cost-of-winning-the-premier-league/prem_cost.png")

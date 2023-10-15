library(dplyr)
library(readr)
library(stringr)
library(gt)


# Load the data
three_pointers <- read_csv("Data-and-Code/games-with-9-or-more-three-pointers/games_with_9_or_more_three_pointers.csv")

# include the name of the images
three_pointers <- three_pointers %>% 
  mutate(player = paste0((tolower(word(names, -1))), ".png"))

images_url <- "https://raw.githubusercontent.com/MonyakeR/awesome-graphics/main/Data-and-Code/games-with-9-or-more-three-pointers/images/"


# function to create bar chart
bar_chart <- function(label, height = "15px",fill = "#00bfc4") {
  bar <- glue::glue(
    "<div style='background:{fill};width:{label}px;height:{height};'></div>"
  )
  glue::glue(
    "<div style='display:flex;align-items:right';>{bar}</div>"
  ) %>%
    gt::html()
}

# create a column for the bar chart
three_pointers <- three_pointers %>% 
  mutate(
    games_var = round(games/max(games) * 100,0),
    no_of_games = purrr::map(
      games_var,
      ~bar_chart(label = .x)
    )
  ) %>% 
  select(player, names, games, no_of_games)

three_pointers_tbl <- three_pointers %>% 
  filter(games >= 3) %>% 
  gt() %>% 
  # add player images
  text_transform(
    locations = cells_body(columns = c(player)),
    fn = function(x) {
      web_image(
        url =  paste0(images_url, x)
      )
    }
  ) %>% 
  cols_label(
    player = "Name",
    names = " ",
    games = "Number of Games",
    no_of_games = " "
  ) %>% 
  cols_width(
    no_of_games ~ px(100)
  ) %>% 
  cols_width(
    games ~ px(200)
  ) %>% 
  cols_width(
    player ~ px(60)
  ) %>% 
  tab_header(
    title = md("**Most regular-season games by a player with 9 or more 3 pointers made**"),
    subtitle = "Stephen Curry has the most with 38 games. The next five players on the list have 34 such games Combined"
  ) %>% 
  opt_align_table_header(align = "left") %>%
  opt_table_font(
    font = list(
      google_font(name = "Archivo")
    ),
    weight = 400
  ) %>% 
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
  tab_style(
    style = list(
      cell_borders(
        sides = "top", color = "black", weight = px(0)
      ),
      cell_text(
        transform = "uppercase",
        v_align = "bottom",
        size = px(14),
        weight = 200
      )
    ),
    locations = cells_column_labels(columns = gt::everything())
  ) %>%
  tab_options(
    # table.border.top.style = "hidden",
    # table.border.bottom.style = "hidden"
    column_labels.background.color = "white",
    data_row.padding = px(3),
    heading.border.bottom.style = "none",
    table.border.top.width = px(3),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    column_labels.font.weight = "normal",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    stub.border.color = "white",
    stub.border.width = px(0),
    source_notes.font.size = 12,
    source_notes.border.lr.style = "none",
    table.font.size = 16,
    heading.align = "left"
  ) %>% 
  tab_source_note(
    source_note = md("*Source: StatMuse, NBA*")
  ) %>% 
  tab_source_note(
    source_note = md("*Only players with 3 or more games are icluded*")
  ) %>% 
  tab_source_note(
    source_note = md("*Last updated **November 19, 2021***")
  ) %>% 
  tab_source_note(source_note = "Graphic: @RetseMonyake")
  
# show the table
three_pointers_tbl

# safe the table
gtsave(three_pointers_tbl, "Data-and-Code/games-with-9-or-more-three-pointers/three_pointers_games.png")


# this script visualises the premier league stadium capacities

library(tidyverse)
library(packcircles)
library(glue)
library(ggtext)
library(showtext)
library(gt)
library(gtExtras)

# configuration
# fonts
font_add_google(name = "Archivo", family = "archivo")
## Automatically use showtext to render text
showtext_auto()

# load the data
# data source: https://www.premierleague.com/clubs
prem_cap <- read_csv("premier-league-stadiums/Premier League stadiums.csv")

# data wrangling
prem_cap <- prem_cap %>% 
  mutate(
    Stadium = case_when(
      Stadium == "Tottenham Hotspur Stadium" ~ "Tottenham Hotspur\nStadium",
      Stadium == "Gtech Community Stadium" ~ "",
      Stadium == "Selhurst Park" ~ "Selhurst\nPark",
      Stadium == "AMEX Stadium" ~ "AMEX\nStadium",
      Stadium == "Craven Cottage" ~ "Craven\nCottage",
      Stadium == "Kenilworth Road" ~ "",
      Stadium == "Vitality Stadium" ~ "",
      .default = Stadium
    ),
    Capacity_fmt = format(Capacity, big.mark=",")
  )

# circleProgressiveLayout: Arranges a set of circles, which are denoted by their 
# sizes, by consecutively placing each circle externally tangent to two 
# previously placed circles while avoiding overlaps.
packing <- circleProgressiveLayout(prem_cap$Capacity, sizetype='radius')

# add the packing result back to the data
prem_cap <- bind_cols(prem_cap, packing)

# circleLayoutVertices: Given a matrix or data frame for a circle layout, 
# with columns for centre x-y coordinates and sizes, this function generates 
# a data set of vertices which can then be used with ggplot or base graphics functions.
prem_cap_circles <- circleLayoutVertices(packing, npoints=50)
# add the cities back, repeat each one 51 times
prem_cap_circles$city <- rep(prem_cap$City, each = 51)

# city colours
city_colours <- c(
  "Manchester" = "#006992",
  "London" = "#007749",
  "Liverpool" = "#C1272D",
  "Newcastle upon Tyne" = "#3C3C3C",
  "Birmingham" = "#483519",
  "Sheffield" = "#092327",
  "Brighton" = "#86BBD8",
  "Wolverhampton" = "#BA3F1D",
  "Nottingham" = "#841C26",
  "Burnley" = "#32021F",
  "Bournemouth" = "#DC136C",
  "Luton" = "#548C2F"
)

# the plot
p <- ggplot() +
  geom_polygon(data = prem_cap_circles, aes(x, y, group = id, fill = as.factor(city)), colour = "black",linewidth = 1.01 , alpha = 0.9) +
  geom_text(
    data = prem_cap, 
    aes(
      x, 
      y, 
      size = Capacity, 
      label = glue("{Stadium}\n{Capacity_fmt}")
    ),
    colour = "white"
  ) + 
  scale_size_continuous(range = c(1.5, 6)) +
  scale_fill_manual(values = city_colours) +
  annotate(
    geom = "curve", x = 155300, y = 211315, xend = 115100, yend = 187531.4,
    curvature = 0.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text", x = 121500, y = 215000, 
    label = "Gtech Community Stadium", 
    hjust = "left"
  ) +
  annotate(
    geom = "text", x =1.40e4 - 90000, y = -1.84e5 - 15000,
    label = "Kenilworth Road\n10,265", 
    hjust = "center"
  ) +
  annotate(
    geom = "curve", x = 1.40e4 - 55100, y = -1.84e5 - 10000, xend = 1.40e4 - 10000 , yend = -1.84e5 - 9000,
    curvature = 0.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "text", x =3.50e4 + 15000, y = -1.79e5 - 55000,
    label = "Vitality Stadium\n11,307",
    hjust = "center"
  ) +
  annotate(
    geom = "curve", x = 3.50e4 + 10000, y = -1.79e5 - 45000, xend = 3.50e4, yend = -1.79e5 - 10500,
    curvature = 0.3, arrow = arrow(length = unit(2, "mm"))
  ) +
  labs(
    title = "Premier League Stadium Capacities - 2023/24",
    subtitle = "Manchester United's Old Trafford has the highest capacity being able to accommodate up to 74,310 supporters.<br/>The smallest stadium belongs to newly promoted Luton Town, with a capacity of 10,265.",
    caption = "<i>Source: Premier League</i><br>Graphic: @RetseMonyake<br>"
  ) +
  theme_void()  + 
  theme(
    legend.position="none",
    plot.title=element_textbox(family = "archivo", size=32),
    plot.subtitle = element_textbox(family = "archivo", size = 16, lineheight = 1.1),
    plot.caption = element_textbox(family = "archivo", size = 11, lineheight = 1.2, hjust = 0)
  ) + 
  coord_equal()

p

ggsave("prem_capacity.pdf", p, width = 15, height = 10)

# table

prem_stadiums <- read_csv("premier-league-stadiums/Premier League stadiums.csv")

tbl <- prem_stadiums %>% 
  mutate(
    Capacity_bar = Capacity,
    badge = glue::glue("premier-league-stadiums/team-badges/{Team}.svg")
  ) %>% 
  select(badge, Team, City,Stadium, Capacity, Capacity_bar) %>% 
  gt() %>% 
  tab_header(
    title = "Premier League Stadium Capacities - 2023/24",
    subtitle = "Manchester United's Old Trafford has the highest capacity being 
    able to accommodate up to 74,310 supporters.The smallest stadium belongs to 
    newly promoted Luton Town, with a capacity of 10,265."
  ) %>% 
  opt_table_font(
    font = list(
      google_font(name = "Archivo")
    ),
    weight = 400
  ) %>% 
  tab_style(
    locations = cells_title("title"),
    style = cell_text(
      weight = 700
    )
  ) %>% 
  tab_style(
    locations = cells_title("subtitle"),
    style = cell_text(
      weight = 300
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
    heading.align = "left",
  ) %>% 
  opt_css(
    "tbody tr:last-child {
        border-bottom: 2px solid #ffffff00;
    }
    ",
    add = TRUE
  ) %>%
  gt_plt_bar(column = Capacity_bar, color = "#006992") %>% 
  gt_badge(column = City, palette = city_colours, alpha = 0.2) %>% 
  fmt_number(columns = Capacity, sep_mark = ",", decimals = 0) %>% 
  cols_label(
    Capacity_bar = "",
    badge = ""
  ) %>% 
  gt_img_rows(columns = badge, img_source = "local") %>% 
  cols_width(
    Team ~ px(310),
    City ~ px(210),
    Stadium ~ px(400),
    Capacity_bar ~ px(150)
  ) %>% 
  tab_source_note(source_note = md("*Source: Premier League*")) %>% 
  tab_source_note(source_note = "Graphic: @RetseMonyake") 


tbl

gtsave(tbl, "premier-league-stadiums/stadiums.png")


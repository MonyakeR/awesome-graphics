# data source : https://www.statista.com/statistics/1228016/largest-banks-in-africa-by-assets/

library(tidyverse)
library(packcircles)
library(glue)
library(ggtext)
library(showtext)

# fonts

font_add_google(name = "Chivo", family = "chivo")

## Automatically use showtext to render text
showtext_auto()

# data

banks <- read_csv("Data & Code/largest-african-banks-by-assets/Africa's largest banks by asset size.csv")

banks <- banks %>% 
  rename(Assets = `Assets ($m)`) %>% 
  mutate(
    Assets = Assets/1000,
    Bank = case_when(
      Bank == "Banque Centrale Populaire" ~ "Banque Centrale\n Populaire",
      Bank == "Bank of Africa - BMCE Group" ~ "Bank of Africa\n BMCE Group",
      .default = Bank
    )
  ) %>% 
  arrange(desc(Assets)) %>% 
  head(n = 10)

# Generate the layout. This function returns a data frame with one line per bubble.
# It gives its center (x and y) and its radius, proportional to the value

packing <- circleProgressiveLayout(banks$Assets, sizetype = "area")

packing$radius <- 0.98*packing$radius

# we can add these packing information to the initial data frame
banks <- cbind(banks, packing)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.

banks_circle <- circleLayoutVertices(packing, npoints = 50)

banks_circle$country <- rep(banks$Country, each = 51)

# Make the plot
p <- ggplot() +
  # bubble
  geom_polygon(
    data = banks_circle, aes(x, y, group = id, fill = as.factor(country)), 
    color = "black", alpha = 0.9
  ) +
  scale_fill_manual(values = c(
    "South Africa" = "#007749", 
    "Egypt" = "#C09300", 
    "Morocco" = "#C1272D"
  )) +
  geom_text(
    data = banks, 
    aes(x, y, size = Assets, label = glue(
      "{Bank}\n","$","{round(Assets, digits = 0)}", "B"
    )),
    color = "white",
    family = "chivo"
  ) +
  labs(
    title = "<b>South African Banks Dominate Africa\\'s Top 10</b>",
    subtitle = "As of 2021, <span style='color:#007749;'>South Africa</span> 
    concentrated five out of the top 10 banks with the largest assets <br>in Africa.
    North African nations, such as <span style='color:#C09300;'>Egypt</span>, 
    and <span style='color:#C1272D;'>Morocco</span> follow as main players <br>in the African banking sector.",
    caption = "<i>Source: Statista</i><br>Graphic: @RetseMonyake<br>"
  ) +
  scale_size_continuous(range = c(3, 6)) +
  theme_void()  + 
  theme(
    legend.position="none",
    plot.title=element_textbox(family = "chivo", size=24),
    plot.subtitle = element_textbox(family = "chivo", size = 14, lineheight = 1.1),
    plot.caption = element_textbox(family = "chivo", size = 11, lineheight = 1.2, hjust = 0)
  ) + 
  coord_equal()

ggsave("african_banks.pdf", p)

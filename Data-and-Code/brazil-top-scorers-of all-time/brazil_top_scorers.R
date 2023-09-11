# this script analyses brazil's top scorers of all time

library(tidyverse)
library(glue)
library(ggtext)
library(ggrepel)
library(showtext)

# set up font
font_add_google(name = "Chivo", family = "chivo")
## Automatically use showtext to render text
showtext_auto()

# read in the data
top_scorers_raw <- read_csv("brazil-top-scorers-of all-time/data/brazil-top-scorers-of-all-time.csv")

# exploratory data analysis
top_scorers <- top_scorers_raw %>% 
  mutate(
    average_goals_per_match = round(Goals/Caps, digits = 2),
    label = glue(
      "{Name}\n",
      "Goals/Caps: {average_goals_per_match} ({Goals}/{Caps})"
    )
  ) %>% 
  select(Name, Goals, Caps,average_goals_per_match, Status, label) %>% 
  arrange(desc(average_goals_per_match))

p <- ggplot(data = top_scorers, mapping = aes(x = Caps, y = Goals, label = label)) +
  geom_point(size = 1.5) +
  geom_label_repel(
    mapping = aes(label = label, colour = Status, fontface = "bold"), 
    nudge_x = 5, nudge_y = 2,
    label.padding = unit(0.5, "lines")
  ) +
  scale_colour_manual(values = c("active" = "#0086b3", "inactive" = "#A6A6A6")) +
  labs(
    title = "<strong>Neymar is now Brazil\\'s Leading Goal Scorer</strong>",
    subtitle = "It has taken Neymar 33 more games to overtake Pele as Brazil\\'s Leading Goal Scorer.<br/>As the only <span style = 'color:#0086b3;'>active</span> player in the top 10, he is well-positioned to retain the record for an extended period",
    caption = "<br><i>As per: 11 September 2023</i><br/>Graphic: @RetseMonyake<br>"
  ) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100), expand = c(0,0)) +
  scale_x_continuous(breaks = seq(20, 140, 20), limits = c(0, 150), expand = c(0,0)) +
  theme(
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title=element_textbox(family = "chivo", size=36, padding = unit(c(15, 0, 0, 0), "pt"),),
    plot.subtitle = element_textbox(family = "chivo", size = 18, lineheight = 1.1, colour = "#3C4048"),
    plot.caption = element_textbox(family = "chivo", size = 11, lineheight = 1.2, hjust = 0),
    plot.background = element_rect(colour = "#fefeff"),
    plot.margin = margin(0, 5, 0, 5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    axis.line.y = element_line(),
    axis.line.x = element_line()
  )

p

ggsave("Data-and-Code/brazil-top-scorers-of all-time/brazil_top_scorer.pdf", width = 15, height = 10)

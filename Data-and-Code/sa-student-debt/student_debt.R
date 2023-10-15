library(readr)
library(dplyr)
library(gt)
library(ggplot2)
library(ggtext)
library(scales)
library(ggthemes)
library(showtext)

# set up font
font_add_google(name = "Archivo", family = "archivo")
## Automatically use showtext to render text
showtext_auto()

# load the data
student_debt <- read_csv("Data-and-Code/sa-student-debt/student_debt.csv")

# ---------------------- ggplot ---------------------------

plt <- student_debt %>% 
  mutate(university = reorder(university, amount)) %>% 
  ggplot(aes(university, amount)) +
    geom_col(fill = "#00bfc4") +
    geom_text(
      family = "archivo", 
      aes(label = dollar(amount, accuracy = 0.01, prefix = "R", suffix = "m", big.mark = ",", decimal.mark = ".")),
      hjust = -0.1
    ) +
    coord_flip() + 
    scale_y_continuous(
      labels = label_number(prefix = "R", suffix = "m", big.mark = ",", decimal.mark = "."),
      expand = expansion(mult = c(0, .1))
    ) + 
    labs(title = "Money owed by graduates to universities",
         subtitle = expression(
           paste("A total of ",bold("116 837")," students had roughly",
                 bold(" R9.6 billion "), "in oustanding fees for the period from",
                 bold(" 2010 "), "to", bold(" 2020"))
         ),
         caption = "<i>Data source: mg.co.za</i><br/>Graphic: @RetseMonyake<br>",
         x = NULL,
         y = NULL) +
    theme_gdocs() +
    theme(
      legend.position = "none",
      plot.title=element_text(family = "archivo", color = "#00bfc4", size=36),
      plot.subtitle = element_text(family = "archivo", size = 18, lineheight = 1.1, colour = "#3C4048"),
      plot.caption = element_textbox(family = "archivo", size = 11, lineheight = 1.2, hjust = 0),
      axis.text = element_text(family = "archivo", size = 12),
      axis.title = element_text(family = "archivo", size = 13, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

ggsave("Data-and-Code/sa-student-debt/student_debt.pdf", width = 15, height = 10)

# ---------------------- gt ---------------------------

# function to create bar chart
bar_chart <- function(label, height = "15px",fill = "#00bfc4") {
  bar <- glue::glue(
    "<div style='background:{fill};width:{label + 5}px;height:{height};'></div>"
  )
  glue::glue(
    "<div style='display:flex;align-items:right';>{bar}</div>"
  ) %>%
    gt::html()
  
}

# create a column for the bar chart
student_debt <- student_debt %>% 
  mutate(
    amount_var = round(amount/max(amount) * 100,0),
    amount_bar = purrr::map(
      amount_var,
      ~bar_chart(label = .x)
    )
  ) %>% 
  select(university, amount, amount_bar, students)

# create table
student_debt_tbl <- student_debt %>%
  mutate(amount = amount * 1000000) %>% 
  gt() %>% 
  tab_header(
    title = md("**Money owed by graduates to universities**"),
    subtitle = md("A total of **116 837** students had roughly **R9.6 billion** in outstanding fees for the period **2010** to **2020**")
  ) %>% 
  opt_table_font(
    font = list(
      google_font(name = "Archivo")
    ),
    weight = 400
  ) %>% 
  fmt_number(
    columns = c(students),
    sep_mark = ",",
    decimals = 0
  ) %>% 
  fmt_currency(
    columns = amount,
    currency = "ZAR",
    suffixing = TRUE
  ) %>% 
  cols_label(
    university = "University",
    amount = "Amount owed",
    amount_bar = " ",
    students = "Number of students"
  ) %>% 
  data_color(
    columns = students,
    colors = scales::col_numeric(palette = c("white", "#00bfc4"),
    domain = NULL)
  ) %>% 
  cols_width(
    amount_bar ~ px(150)
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  opt_row_striping() %>% 
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
  opt_css(
    "tbody tr:last-child {
        border-bottom: 2px solid #ffffff00;
    }
    ",
    add = TRUE
  ) %>%
  tab_source_note(
    source_note = md("*Source: mg.co.za*")
  ) %>% 
  tab_source_note(
    source_note = md("*Amounts have been rounded off and reflect accumalated interest*")
  ) %>% 
  tab_source_note(source_note = "Graphic: @RetseMonyake") 

# show table
student_debt_tbl

# safe the table
gtsave(student_debt_tbl, "Data-and-Code/sa-student-debt/student_debt_tbl.png", expand = 10)

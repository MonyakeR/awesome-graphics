library(readr)
library(dplyr)
library(gt)
library(ggplot2)
library(scales)
library(ggthemes)

# load the data
student_debt <- read_csv("sa-student-debt/student_debt.csv")

# ---------------------- ggplot ---------------------------

student_debt %>% 
  mutate(university = reorder(university, amount)) %>% 
  ggplot(aes(university, amount)) +
    geom_col(fill = "#00bfc4") +
    geom_text(
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
         caption = "Data source: mg.co.za",
         x = NULL,
         y = NULL) +
    theme_gdocs() +
    theme(
      legend.position = "none",
      plot.title = element_text(color = "#00bfc4", size = 16, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )



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
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden"
  ) %>% 
  tab_source_note(
    source_note = md("Source: mg.co.za")
  ) %>% 
  tab_source_note(
    source_note = md("Amounts have been rounded off and reflect accumalated interest")
  )

# show table
student_debt_tbl

# safe the table
gtsave(student_debt_tbl, "student_debt_tbl.html")

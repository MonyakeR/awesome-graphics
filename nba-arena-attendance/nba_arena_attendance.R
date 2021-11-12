library(readr)
library(dplyr)
library(gt)

nba <- read_csv("nba-arena-attendance/nba_arena_attendance.csv")

nba <- nba %>% 
  mutate(proportion = `Fans Allowed`/`Maximum Capacity`)

nba_tbl <- gt(nba) %>% 
  fmt_number(
    columns = c(`Fans Allowed`, `Maximum Capacity`),
    sep_mark = ","
  ) %>% 
  fmt_percent(
    columns = proportion,
    decimals = 0
  )

nba_tbl

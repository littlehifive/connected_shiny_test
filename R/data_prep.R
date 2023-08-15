library(tidyverse)

dat <- read_csv(here::here("data/ConnectEd R5-8 dashboard mock-ups_data - Sheet1.csv"),
                show_col_types = FALSE)

# clean data
dat <- dat |> 
  mutate(stud_level_baseline = ifelse(stud_level_baseline %in% c(".r", ".u"),
                                      NA_character_,
                                      stud_level_baseline
                                      )
         )

if(!require("pacman"))install.packages("pacman")
pacman::p_load("here", "tidyverse", "digest")
#----load----
file_names <- list.files(here("data"), "\\.csv$")
df <- tibble(files = map(here("data", file_names), read_csv),
             event = str_split_fixed(file_names, "-", n = 2)[, 1],
             time = str_split_fixed(file_names, "-", n = 2)[, 2] %>%
               str_replace("\\.csv", ""))
df <- mutate(df, files = files %>% map(~cbind(.x, id = .x %>% rowwise() %>% do(id = digest(.)) %>% unlist()) %>% mutate(id = as.character(id))))

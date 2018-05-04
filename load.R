if(!require("pacman"))install.packages("pacman")
pacman::p_load("here", "tidyverse")

file_names <- list.files(here(), "\\.csv$")
df <- tibble(files = map(file_names, read_csv),
             event = str_split_fixed(file_names, "-", n = 2)[, 1],
             time = str_split_fixed(file_names, "-", n = 2)[, 2] %>%
               str_replace("\\.csv", ""))

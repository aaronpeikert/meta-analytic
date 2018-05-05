if(!require("pacman"))install.packages("pacman")
pacman::p_load("here", "tidyverse", "pwr")
source(here("scripts", "load.R"))
source(here("scripts", "retrodesign.R"))

#----calc-power-1----
calc_power <- function(df, d){
  df %>% pull("N") %>%
    map(~pwr.t.test(.x, d = d, type = "paired")) %>% 
    map_dbl("power")
}

df <- mutate(df, power = map(files, calc_power, d = .2))

#----unnest----
df_unnest <- unnest(df)

#----calc-errors----
df_unnest <- mutate(df_unnest,
                    closest_error = map_int(power, which_near, errors$power),
                    typeS = errors$typeS[closest_error],
                    typeM = errors$typeM[closest_error])

#----calc-power-2----
d_assumed <- seq(.1, .7, .01)
df_ds <- d_assumed %>%
  map(~mutate(df, power = map(files, calc_power, d = .x))) %>% 
  map(unnest) %>%
  map(~mutate(.x, closest_error = map_int(power, which_near, errors$power),
                         typeS = errors$typeS[closest_error],
                         typeM = errors$typeM[closest_error])) %>%
  map2(d_assumed, ~mutate(.x, d_assumed = .y)) %>% 
  bind_rows()

if(!require("pacman"))install.packages("pacman")
pacman::p_load("here", "tidyverse", "pwr")
source(here("scripts", "load.R"))
source(here("scripts", "retrodesign.R"))

#----calc-power----
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

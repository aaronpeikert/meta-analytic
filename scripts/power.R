if(!require("pacman"))install.packages("pacman")
pacman::p_load("here", "tidyverse", "pwr")
source(here("scripts", "load.R"))
source(here("scripts", "retrodesign.R"))

calc_power <- function(df, d){
  df %>% pull("N") %>%
    map(~pwr.t.test(.x, d = d, type = "paired")) %>% 
    map_dbl("power")
}

df <- mutate(df, power = map(files, calc_power, d = .2))

df_unnest <- unnest(df)
df_unnest %>% ggplot(aes(time, power)) +
  geom_violin() +
  facet_wrap(~event, ncol = 2) +
  theme_minimal()

df_unnest <- mutate(df_unnest,
                    closest_error = map_int(power, which_near, errors$power),
                    typeS = errors$typeS[closest_error],
                    typeM = errors$typeM[closest_error])

df_unnest %>% ggplot(aes(time, typeM)) +
  geom_violin() +
  facet_wrap(~event) +
  theme_minimal()

df_unnest %>% mutate(d = abs(d)) %>% 
  ggplot(aes(d, typeM, color = event)) + geom_point() + theme_minimal()

df_unnest %>% group_by(event, time) %>% summarise(typeM = mean(typeM)) %>% 
  pander::pander()


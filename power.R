library(tidyverse)
library(here)

file_names <- list.files(here(), "\\.csv$")

df <- tibble(files = map(file_names, read_csv),
       event = str_split_fixed(file_names, "-", n = 2)[, 1],
       time = str_split_fixed(file_names, "-", n = 2)[, 2] %>%
         str_replace("\\.csv", ""))


retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000){
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-A/s, df)
  p.lo <- pt(-z-A/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- A + s*rt(n.sims,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/A
  return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}

library(pwr)

mean_d_mariage <- mean(mariage_prospect$d)

calc_power <- function(df, d){
  df %>% pull("N") %>%
    map(~pwr.t.test(.x, d = d, type = "paired")) %>% 
    map_dbl("power")
}

df <- mutate(df, power = map(files, calc_power, d = .2))

df %>% unnest() %>% ggplot(aes(time, power)) + geom_violin() + facet_wrap(~event, ncol = 2) + theme_minimal()

D_range <- c(seq(0,1,.01),seq(1,10,.1),100)
n <- length(D_range)
power <- rep(NA, n)
typeS <- rep(NA, n)
exaggeration <- rep(NA, n)
for (i in 1:n){
  a <- retrodesign(D_range[i], 1)
  power[i] <- a$power
  typeS[i] <- a$typeS
  exaggeration[i] <- a$exaggeration
}

errors <- tibble(power, typeS, typeM = exaggeration)

plot(power, typeS, type="l", xlim=c(0,1.05), ylim=c(0,0.54), xaxs="i", yaxs="i",
     xlab="Power", ylab="Type S error rate", bty="l", cex.axis=.9, cex.lab=.9) 

plot(power, exaggeration, type="l", xlim=c(0,1.05), ylim=c(0,12), xaxs="i", yaxs="i",
     xlab="Power", ylab="Exaggeration ratio", bty="l", yaxt="n", cex.axis=.9, cex.lab=.9)

which_near <- function(what, where){
  which.min(abs(where - what))[1]
}

df %>%
  unnest() %>%
  mutate(closest_error = map_int(power, which_near, errors$power),
         typeS = errors$typeS[closest_error],
         typeM = errors$typeM[closest_error]) %>% 
  ggplot(aes(time, typeM)) + geom_violin() + facet_wrap(~event) + theme_minimal()

df %>%
  unnest() %>%
  mutate(closest_error = map_int(power, which_near, errors$power),
         typeS = errors$typeS[closest_error],
         typeM = errors$typeM[closest_error]) %>% 
  mutate(d = abs(d)) %>% 
  ggplot(aes(d, typeM, color = event)) + geom_point() + theme_minimal()

df %>%
  unnest() %>%
  mutate(closest_error = map_int(power, which_near, errors$power),
         typeS = errors$typeS[closest_error],
         typeM = errors$typeM[closest_error]) %>% 
  group_by(event, time) %>% 
  summarise(typeM = mean(typeM)) %>% 
  pander::pander()


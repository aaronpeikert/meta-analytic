if(!require("pacman"))install.packages("pacman")
pacman::p_load("here", "tidyverse")
# Code from Gelman, A., & Carlin, J. (2014). Beyond Power Calculations: Assessing Type S (Sign) and Type M (Magnitude) Errors. Perspectives on Psychological Science, 9(6), 641–651. https://doi.org/10.1177/1745691614551642

#----beyond-power-calculations----
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

#plot(power, typeS, type="l", xlim=c(0,1.05), ylim=c(0,0.54), xaxs="i", yaxs="i",
#     xlab="Power", ylab="Type S error rate", bty="l", cex.axis=.9, cex.lab=.9) 

#plot(power, exaggeration, type="l", xlim=c(0,1.05), ylim=c(0,12), xaxs="i", yaxs="i",
#     xlab="Power", ylab="Exaggeration ratio", bty="l", yaxt="n", cex.axis=.9, cex.lab=.9)

## own code, not from paper

#----save-errors----
errors <- tibble(power, typeS, typeM = exaggeration)

which_near <- function(what, where){
  which.min(abs(where - what))[1]
}

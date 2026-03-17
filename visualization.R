library(readr)
library(tidyverse)
d <- read.csv("Concussion Injuries 2012-2014.csv")
x <- d$Position
y <- d$Week.of.Injury

# Opening the graphical device
pdf("visualization.pdf")
boxplot(y ~ x
        , main = "Position vs Injury" # main chart title
        , xlab = "Type of Position" # x-axis label
        , ylab = "Weeks of injury " # y-axis label
        , pch = 19
        , frame = T
        , boxwex=0.3
        , las=2
        , options(scipen = 100))
# Points
stripchart(y~x,              # Data
           method = "jitter", # Random noise
           pch = 20,          # Pch symbols
           col = 2,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE,
           xlim = c(40,100)
           #col="green"
)
h <- hist(y # variable to count
          , 6 # number of 'bins'(or bars)
          , main = "Week of Injury Frequency" # main chart title
          , xlab = "Week of Injury" # x-axis label
          , ylab = "Frequency" # y-axis label
          , col="azure" # fill color for the bars
          , breaks =10
)
# Overlay a normal curve

mn <- mean(y) # mean of the data (runs)
stdD <- sd(y) # standard deviation of the data
x <- seq(min(y), max(y), length = 40) # x-axis points: 100 to 700
y1 <- dnorm(x, mean=mn, sd=stdD) # an idealized normal curve,
# (frequencies are fraction of 1.0)
y1 <- y1 * diff(h$mids[1:2]) * length(y); # calibrate the curve against actual

lines(x, y1, col="darkblue",lwd=2) # plot calibrated line in darkblue

# Closing the graphical device
dev.off() 


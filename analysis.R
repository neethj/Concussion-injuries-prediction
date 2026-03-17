library(readr)
library(tidyverse)
library(dplyr)
d <- read.csv("Concussion Injuries 2012-2014.csv")
 

d1 <- d %>%
mutate(Positiontype = case_when(Position == 'Center' | Position == 'Full Back' | Position == 'Guard' | Position == 'Long Snapper' | Position == 'Offensive Tackle' | Position == 'Quarterback' | Position == 'Running Back' | Position == 'Tight End' | Position == 'Wide Receiver' ~ 'offense',
Position=='Comerback' | Position =='Defensive End' | Position =='Defensive Tackle' | Position =='Linebacker' | Position =='Safety' ~ 'defense'))


Defense <- d1 %>%
  filter(Positiontype =="defense")
Offensive <- d1%>%
  filter(Positiontype =="offense")

stdout<- wilcox.test(Offensive$Week.of.Injury, Defense$Week.of.Injury)
stdout

                                                                                                     
                                                        
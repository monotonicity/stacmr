
library("tidyverse")

## data from Exp. 1 of Dunn, Newell, & Kalish (2012)
delay <- read.table("development/delay.dat")
colnames(delay) <- c("participant", "condition", "dv", 
                     "B1","B2","B3","B4")

delay2 <- delay %>% 
  mutate(participant = factor(participant),
         delay = factor(condition, 
                        levels = 1:2, 
                        labels = c("no delay", "delay")),
         structure = factor(dv, 
                            levels = 1:2,
                            labels = c("rule-based", 
                                       "information-integration")))

delay2 %>% 
  group_by(structure, delay) %>% 
  summarise(n())

delay <- delay2 %>% 
  select(-condition, -dv) %>% 
  gather(key = "block", "pc", B1:B4) %>% 
  mutate(block = factor(block))
str(delay)
# 'data.frame':	520 obs. of  5 variables:
#  $ participant: Factor w/ 130 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#  $ delay      : Factor w/ 2 levels "no delay","delay": 2 1 1 2 2 1 1 2 2 2 ...
#  $ structure  : Factor w/ 2 levels "rule-based","information-integration": 1 2 1 2 1 2 1 2 1 1 ...
#  $ block      : Factor w/ 4 levels "B1","B2","B3",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ pc         : num  0.338 0.287 0.525 0.35 0.237 ...

usethis::use_data(delay, overwrite = TRUE)

xx <- prep_data(delay, 
          col_value = "pc", 
          col_participant = "participant",
          col_dv = "structure", 
          col_within = "block", 
          col_between = "delay")
str(xx)

tt <- prep_data(delay, 
          col_value = "pc", 
          col_participant = "participant",
          col_dv = "structure", 
          col_within = "block", 
          col_between = "delay", return_list = FALSE) 
str(tt)

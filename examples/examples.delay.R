
library("stacmr")

## load data from Exp. 1 of Dunn, Newell, & Kalish (2012)
data(delay)
delay$delay <- factor(delay$delay, levels = c("delay", "no delay"))
#delay$delay <- factor(delay$delay, levels = c("no delay", "delay"))
str(delay, width = 78, strict.width = "cut")
# 'data.frame':	520 obs. of  5 variables:
#  $ participant: Factor w/ 130 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 1..
#  $ delay      : Factor w/ 2 levels "no delay","delay": 2 1 1 2 2 1 1 2 2 2 ...
#  $ structure  : Factor w/ 2 levels "rule-based","information-integration": 1..
#  $ block      : Factor w/ 4 levels "B1","B2","B3",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ pc         : num  0.338 0.287 0.525 0.35 0.237 ...

stats <- sta_stats(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay"
)
stats
str(stats)
summary(stats)

### Fit and test CMR State-Trace Analysis Model
st_d1 <- cmr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  nsample = 1e4
)
st_d1  ## basic information about conjoint-monotonic model

summary(st_d1)  ## full summary information about conjoint-monotonic model

state_trace(st_d1) ## produces state trace plot

plot_null(st_d1)  ## produces histogram of empirical (boot-strapped) null distribution

str(st_d1, 1, give.attr = FALSE) ## overview of information in fitted object

st_d2 <- cmr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  approx = TRUE, 
  nsample = 1e4
)

st_d3 <- cmr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", test = FALSE,
  approx = TRUE, 
  nsample = 1e4
)

### with partial order

## wrong model:
st_d4 <- cmr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  nsample = 1e4, 
  partial = "auto"
)
st_d4

mr_d4 <- mr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  nsample = 1e4, 
  partial = "auto"
)
mr_d4

mr_d5 <- mr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  nsample = 1e4, 
  partial = list(
    delay = "delay < `no delay`",
    block = "B1 < B2 < B3 < B4"
  )
)
mr_d5

mr_d6 <- mr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  nsample = 1e4, 
  partial = list(
    delay = "delay = `no delay`",
    block = "B1 < B2 < B3 < B4"
  )
)
summary(mr_d6)


### OLD STUFF BELOW: CHANGE PLEASE

#### Fit and specify partial order that should be obeyed by data
# Create Partial Order
E <- list(c(1:4),c(5:8),c(5,1),c(6,2),c(7,3),c(8,4))
list2adj(1:8, E)

### Fits Partial Order Model to Data
fit2 <- fit_mr(data=delay, col_value = "pc", 
               col_participant = "participant",
               col_dv = "structure", 
               col_within = "block", 
               col_between = "delay",
               partial=E)
cbind(fit2$x[[1]],fit2$x[[2]]) # simplify presentation
fit2$fval
fit2$shrinkage

### Test fit of Partial Order Model
test2 <- test_mr(data=delay, col_value = "pc", 
                 col_participant = "participant",
                 col_dv = "structure", 
                 col_within = "block", 
                 col_between = "delay", 
                 partial=E, nsample=10000)
test2$p
test2$datafit

#### test conjoint model versus partial order model
### p-Value of Difference in Fit of Conjoint Monotonic and Fit of Partial Order Model
test3 <- test_cmr(data=delay, col_value = "pc", 
                  col_participant = "participant",
                  col_dv = "structure", 
                  col_within = "block", 
                  col_between = "delay", 
                  partial=E, nsample=10000)
test3$p
test3$datafit


# staPLOT(data = delay, 
#         groups = list(c(1:4), c(5:8)), 
#         grouplabels = list("No delay", "Delay"), 
#         axislabels = list("RB","II"),
#         pred=out1$x)







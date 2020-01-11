
library("stacmr")

## load data from Exp. 1 of Dunn, Newell, & Kalish (2012)
data(delay)
str(delay, width = 78, strict.width = "cut")
# 'data.frame':	520 obs. of  5 variables:
#  $ participant: Factor w/ 130 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 1..
#  $ delay      : Factor w/ 2 levels "delay","no delay": 1 2 2 1 1 2 2 1 1 1 ...
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

### cmr() fits conjoint monotonic regression for state-trace analysis
## Fit and test CMR State-Trace Analysis Model
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

summary(st_d1)  ## basic information plus estimated cell means

# state_trace(st_d1) ## produces state trace plot

# plot_null(st_d1)  ## produces histogram of empirical (boot-strapped) null distribution

str(st_d1, 1, give.attr = FALSE) ## overview of information in fitted object

## same model with approximate method gives same result here
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
summary(st_d2)


### mr() fits monotonic regression with specified partial order

## for delay data: order of factor-levels corresponds to expected partial order.
## Therefore, 'partial = "auto"' can be used to enforce this order.
mr_d1 <- mr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  nsample = 1e4, 
  partial = "auto"
)
mr_d1

## Alternatively, partial order can be specified symbolically:
mr_d2 <- mr(
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
mr_d2

## Partial order can also be specified partially symbolically:
mr_d3 <- mr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  nsample = 1e4, 
  partial = list(
    delay = "auto",
    block = "B1 < B2 < B3 < B4"
  )
)
mr_d3


### cmr() also accepts partial order. 
## CMR model is tested against MR model with partial order 
st_d3 <- cmr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  partial = "auto"
)
st_d3
summary(st_d3)


## p-value now changes somewhat with approximate method:
st_d4 <- cmr(
  data = delay, 
  col_value = "pc", 
  col_participant = "participant",
  col_dv = "structure", 
  col_within = "block", 
  col_between = "delay", 
  partial = "auto",
  approx = TRUE, 
  nsample = 1e4
)
st_d4

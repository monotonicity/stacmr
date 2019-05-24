
library("stacmr")

## load data from Exp. 1 of Dunn, Newell, & Kalish (2012)
data(delay)
str(delay, width = 78, strict.width = "cut")
# 'data.frame':	520 obs. of  5 variables:
#  $ participant: Factor w/ 130 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 1..
#  $ delay      : Factor w/ 2 levels "no delay","delay": 2 1 1 2 2 1 1 2 2 2 ...
#  $ structure  : Factor w/ 2 levels "rule-based","information-integration": 1..
#  $ block      : Factor w/ 4 levels "B1","B2","B3",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ pc         : num  0.338 0.287 0.525 0.35 0.237 ...

### Fit CMR State-Trace Analysis Model
fit1 <- fit_cmr(data=delay, col_value = "pc", 
                col_participant = "participant",
                col_dv = "structure", 
                col_within = "block", 
                col_between = "delay")
cbind(fit1$x[[1]], fit1$x[[2]])
fit1$fval
fit1$shrinkage


### Test CMR State-Trace Analysis
test1 <- test_cmr(data=delay, col_value = "pc", 
                  col_participant = "participant",
                  col_dv = "structure", 
                  col_within = "block", 
                  col_between = "delay")
test1


#### Fit and specify partial order that should be obeyed by data
# Create Partial Order
E <- list(c(1:4),c(5:8),c(5,1),c(6,2),c(7,3),c(8,4))

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
test2 <- fit_mr(delay, partial=E, nsample=10000)
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







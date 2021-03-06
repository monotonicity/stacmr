% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delay.R
\docType{data}
\name{delay}
\alias{delay}
\title{Experiment 1 of Dunn, Newell, & Kalish (2012)}
\format{An object of class \code{data.frame} with 520 rows and 5 columns.}
\source{
Dunn, J. C., Newell, B. R., & Kalish, M. L. (2012). The effect of
feedback delay and feedback type on perceptual category learning: The
limits of multiple systems. \emph{Journal of Experimental Psychology:
Learning, Memory, and Cognition}, 38(4), 840-859.
\url{https://doi.org/10.1037/a0027867}
}
\usage{
delay
}
\description{
Data from 130 \code{participant}s with two between-participants factors (\code{delay}
and \code{structure}) with two levels each and one within-participants factor
(\code{block}) with four levels. The dependent variable is proportion correct
\code{pc}.
}
\details{
Participants in this study completed one of two category-learning tasks
defined according to the category \code{structure} that participants learned. For
the rule-based (RB) group, the category structure was defined by a simple
rule. For the information-integration (II) group, the category structure was
more complex and could not be defined by a simple rule.

The experiment consisted of four 80-trial \code{block}s for each participant.
Within each block, all 80 stimuli were presented in a random order (with
different orders for each participant). Participants were told to learn which
of four categories (labeled, 1, 2, 3, and 4) each stimulus belonged to. On
each trial, a stimulus was presented, and participants terminated the display
by pressing one of the keys labeled 1-4 on the computer keyboard
corresponding to Categories 1-4, respectively. Following the response, a mask
appeared. The length with which the mask was shown defined the \code{delay}
condition. Either 0.5-s (No Delay condition) or 5-s (Delay condition).

Following presentation of the mask, feedback appeared on the computer screen
for 0.75 s. If the response was correct, the word "Correct" was presented;
otherwise, the word "Incorrect" was presented.  Following presentation of
feedback, a blank screen followed the duration of which was again defined the
\code{delay} condition. The screen was blank for either 5 s (No Delay condition)
or 0.5 s (Delay condition) before the next trial commenced. The sequence and
timing of these events were same as those used by Maddox and Ing (2005).
}
\examples{

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
}
\references{
Maddox, W. T., & Ing, A. D. (2005). Delayed Feedback Disrupts the
Procedural-Learning System but Not the Hypothesis-Testing System in
Perceptual Category Learning. \emph{Journal of Experimental Psychology: Learning,
Memory, and Cognition}, 31(1), 100-107.
\url{https://doi.org/10.1037/0278-7393.31.1.100}
}
\keyword{datasets}

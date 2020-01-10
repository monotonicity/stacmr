#' CMR State-Trace Analysis
#' @rdname continuous_cmr
#' @aliases continuous_cmr
#' 
#' @description 
#' 
#' `cmr` is the main function that conducts the CMR (state-trace) analysis for
#' continuous data. It takes the data as a `data.frame` and an optional partial
#' order and returns a fitted model object of class `stacmr`. It fits the
#' conjoint monotonic model to the data and calculates the *p*-value.
#' 
#' `mr` conducts monotonic regression on a data structure according to a
#' given partial order. 
#' 
#' @param  data `data.frame` containing data aggregated by participant and
#'   relevant variables in columns.
#' @param  col_value `character`. Name of column in `data` containing numerical
#'   values for analysis (i.e., responses).
#' @param col_participant `character`. Name of column in `data` containing the
#'   participant identifier.
#' @param col_dv `character`. Name of column in `data` containing the dependent
#'   variable(s) spanning the state-trace axes.
#' @param col_within `character`, optional. Name of column(s) in `data`
#'   containing the within-subjects variables.
#' @param col_between `character`, optional. Name of column(s) in `data`
#'   containing the between-subjects variables.
#' @param partial defines a partial order. Either a `character` or a named
#'   `list` of characters. See details for ways to specify a partial order.
#' @param test logical. If `TRUE` (the default) *p*-value is calculated based
#'   double-bootsrap procedure with `nsamples`. If `FALSE`, no test statistic is
#'   approximated and model is only fit.
#' @param nsample number of bootstrap samples to empirically approximate the
#'   null distribution (default is 1000, but about 10000 is probably better).
#'   Only used if `test = TRUE`.
#' @param  shrink Shrinkage parameter (see [sta_stats]). Default calculates
#'   optimum amount of shrinkage.
#' @param approx `FALSE` (the default) uses full algorithm, `TRUE` uses an
#'   approximate algorithm that should be used for large problems.
#' @param tolerance tolerance used during optimization for numerical stability
#'   (function values smaller than `tolerance` are set to 0)
#'   
#' @example examples/examples.delay.R
#' @import rJava
#' 
#' @export
cmr <- function (data, 
                 col_value, col_participant, col_dv, 
                 col_within, col_between, 
                 partial, 
                 test = TRUE,
                 nsample = 1000, 
                 shrink = -1, 
                 approx = FALSE, 
                 tolerance = 1e-4) {
  # wrapper function for staCMRx and jCMRfitsx
  # Fit and Test Multidimensional CMR
  # data is cell array of data or structured output from staSTATS 
  # partial will be transformed into partial order
  # shrink is parameter to control shrinkage of covariance matrix;
  # 0 = no shrinkage; 1 = diagonal matrix; -1 = calculate optimum
  # returns:
  # x = best fitting CMR values to y-means
  # fval = fit statistic
  # shrinkage = estimated shrinkage of covariance matrix
  # approx = F for full algorithm; T = approximate algorithm
  
  cl <- match.call()
  
  data_list <- prep_data(data = data, 
                    col_value = col_value, 
                    col_participant = col_participant, 
                    col_dv = col_dv, 
                    col_within = col_within, 
                    col_between = col_between)
  
  stats <- sta_stats(data = data, 
                     col_value = col_value, 
                     col_participant = col_participant, 
                     col_dv = col_dv, 
                     col_within = col_within, 
                     col_between = col_between)
  stats_df <- summary(stats)
  
  adj_mat <- make_adj_matrix(
    data = data,
    data_list = data_list, 
    col_within = col_within, 
    col_between = col_between,
    stats_df = stats_df,
    partial = partial
  )
  
  fit_out <- staCMRx(
    data_list,
    model = NULL,
    E = adj_mat,
    shrink = shrink,
    tolerance = tolerance,
    proc = -1,
    approx = approx
  )
  
  ## prepare output from fit object
  estimate <- stats_df[,1:4]
  estimate[[1]] <- fit_out$x[[1]]
  estimate[[2]] <- fit_out$x[[2]]
  
  out <- list(
    estimate = estimate,
    fit = fit_out$fval,
    partial = adj_mat
  )
  attr(out, "value_fit") <- "SSE"  ## sum of squared errors
  
  if (test) {
    proc <-  -1
    cheapP <- FALSE
    mrTol <- 0
    seed <- -1
    model <- matrix(1,length(data_list[[1]]),1)
    # input:
    # nsample = no. of Monte Carlo samples (about 10000 is good)
    # data = data structure (cell array or general)
    # model is a nvar * k matrix specifying the linear model, default = ones(nvar,1))
    # partial = optional partial order model e.g. E={[1 2] [3 4 5]} indicates that
      # condition 1 <= condition 2 and condition 3 <= condition 4 <= condition 5
      # default = none (empty)
    # shrink is parameter to control shrinkage of covariance matrix (if input is not stats form);
      # 0 = no shrinkage; 1 = diagonal matrix; -1 = calculate optimum, default = -1
    # approx = approximation algorithm; F = no; T = yes
    test_out <- jCMRfitsx(nsample = nsample,
                          y = data_list, 
                          model = model, 
                          E = adj_mat, 
                          shrink = shrink,
                          proc = proc, cheapP = cheapP, approximate = approx, 
                          mrTol = mrTol, seed = seed) # call java program
    test_out$fits[which(test_out$fits <= tolerance)] = 0;
    # output:
    # p = empirical p-value
    # datafit = observed fit of monotonic (1D) model
    # fits = nsample vector of fits of Monte Carlo samples (it is against this
    # distribution that datafit is compared to calculate p)
    
    out$p <- test_out$p
    out$fit_null_dist <- test_out$fits
    attr(out, "nsample") <- nsample
    
  } else {
    out$p <- NA
    out$fit_null_dist <- NA
    attr(out, "nsample") <- 0
  }
  
  
  out$shrinkage <- fit_out$shrinkage
  out$data_list <- data_list
  out$call <- cl
  
  class(out) <- "stacmr"
  return (out)
}


mr <- function (data, 
                col_value, col_participant, col_dv, 
                col_within, col_between, 
                partial, 
                test = TRUE,
                nsample = 1000, 
                shrink = -1, 
                approx = FALSE, 
                tolerance = 1e-4) {
  # wrapper function for staCMRx and jCMRfitsx
  # Fit and Test Multidimensional CMR
  # data is cell array of data or structured output from staSTATS 
  # partial will be transformed into partial order
  # shrink is parameter to control shrinkage of covariance matrix;
  # 0 = no shrinkage; 1 = diagonal matrix; -1 = calculate optimum
  # returns:
  # x = best fitting CMR values to y-means
  # fval = fit statistic
  # shrinkage = estimated shrinkage of covariance matrix
  # approx = F for full algorithm; T = approximate algorithm
  
  cl <- match.call()
  
  data_list <- prep_data(data = data, 
                    col_value = col_value, 
                    col_participant = col_participant, 
                    col_dv = col_dv, 
                    col_within = col_within, 
                    col_between = col_between)
  
  stats <- sta_stats(data = data, 
                     col_value = col_value, 
                     col_participant = col_participant, 
                     col_dv = col_dv, 
                     col_within = col_within, 
                     col_between = col_between)
  stats_df <- summary(stats)
  
  adj_mat <- make_adj_matrix(
    data = data,
    data_list = data_list, 
    col_within = col_within, 
    col_between = col_between,
    stats_df = stats_df,
    partial = partial
  )
  
  y <- staSTATS (data_list, shrink)
  
  nvar = length(y)
  shrinkage = matrix(0, length(y[[1]]$shrinkage), nvar)
  for (ivar in 1:nvar) {shrinkage[,ivar] = y[[ivar]]$shrinkage}

  # do MR for each dependent variable
  xPrime = vector("list", nvar)
  fit = matrix(0, nvar, 1)
  for (ivar in 1:nvar) {
    out = jMR (y[[ivar]]$means, y[[ivar]]$weights, adj2list(adj_mat))
    xPrime[[ivar]] = out$x
    fit[ivar] = out$fval
  }
  fval = sum(fit)
  if (fval < tolerance) {fval = 0} # round down
  
  for (i in 1:nvar) {xPrime[[i]]=matrix(xPrime[[i]],length(xPrime[[i]]),1)}
  fit_out = list(xPrime, fval, shrinkage)
  names(fit_out) = c("x", "fval", "shrinkage")
  
  ## prepare output from fit object
  estimate <- stats_df[,1:4]
  estimate[[1]] <- fit_out$x[[1]]
  estimate[[2]] <- fit_out$x[[2]]
  
  out <- list(
    estimate = estimate,
    fit = fit_out$fval,
    partial = adj_mat
  )
  attr(out, "value_fit") <- "SSE"  ## sum of squared errors
  
  if (test) {
    
    test_out <- jMRfits(nsample = nsample, 
                        y = y, E = adj2list(adj_mat),
                        shrink = shrink)
    test_out$fits[which(test_out$fits <= tolerance)] <- 0;
    # output:
    # p = empirical p-value
    # datafit = observed fit of monotonic (1D) model
    # fits = nsample vector of fits of Monte Carlo samples (it is against this
    # distribution that datafit is compared to calculate p)
    
    out$p <- test_out$p
    out$fit_null_dist <- test_out$fits
    attr(out, "nsample") <- nsample
    
  } else {
    out$p <- NA
    out$fit_null_dist <- NA
    attr(out, "nsample") <- 0
  }
  
  
  out$shrinkage <- fit_out$shrinkage
  out$data_list <- data_list
  out$call <- cl
  
  class(out) <- "stacmr"
  return (out)
}


#' @rdname continuous_cmr 
#' @export
fit_mr <- function(data, 
                   col_value, col_participant, col_dv, col_within, 
                   col_between,
                   partial = list(), shrink=-1,
                   tolerance = 1e-4) {
  # function [xPrime, fit, shrinkage] = staMR (data, partial, shrink)
  # fits monotonic regression model to data according to partial order
  # data is list of lists of data or structured output from staSTATS
  # partial is partial order in list format
  # shrink is parameter to control shrinkage of covariance matrix;
  # 0 = no shrinkage; 1 = diagonal matrix; -1 = calculate optimum
  # returns:
  #   x = best fitting MR values to y-means
  #   f = total fit statistic
  #   shrinkage = shrinkage from applying staSTATS
  # *************************************************************************
  # modified from matlab 13 September 2016
  # *************************************************************************
  #
  
  ## bring data in list format
  data <- prep_data(data = data, 
                    col_value = col_value, 
                    col_participant = col_participant, 
                    col_dv = col_dv, 
                    col_within = col_within, 
                    col_between = col_between)
  y = staSTATS (data, shrink) # get stats
  
  # # get stats from data (depending on its form)
  # if (is(data,"data.frame")) {
  #   y = gen2list (data) # convert from general format
  #   y = staSTATS (y, shrink) # get stats
  # } else if (is.null(data[[1]]$means)) {y = staSTATS(data, shrink) # in list form, get stats
  # } else {y = data} # already in stats form
  
  # convert partial order to list if in adjacency matrix form
  if (is(partial,"matrix")) {partial = adj2list(partial)}
  
  # extract shrinkage parameters (for information only)
  nvar = length(y)
  shrinkage = matrix(0, length(y[[1]]$shrinkage), nvar)
  for (ivar in 1:nvar) {shrinkage[,ivar] = y[[ivar]]$shrinkage}

  # do MR for each dependent variable
  xPrime = vector("list", nvar)
  fit = matrix(0, nvar, 1)
  for (ivar in 1:nvar) {
    out = jMR (y[[ivar]]$means, y[[ivar]]$weights, partial)
    xPrime[[ivar]] = out$x
    fit[ivar] = out$fval
  }
  fval = sum(fit)
  if (fval < tolerance) {fval = 0} # round down
  
  for (i in 1:nvar) {xPrime[[i]]=matrix(xPrime[[i]],length(xPrime[[i]]),1)}
  output = list(xPrime, fval, shrinkage)
  names(output) = c("x", "fval", "shrinkage")
  
  return(output)
}

#' @rdname continuous_cmr 
#' @param nsample no. of Monte Carlo samples (about 10000 is good)
#' @export
test_mr <- function (data, 
                   col_value, col_participant, col_dv, col_within, 
                   col_between, 
                   partial = list(), nsample=1, shrink=-1,
                   tolerance = 1e-4) {
# input:
  # nsample = no. of Monte Carlo samples (about 10000 is good)
  # data = data structure (cell array or general)
  # partial = optional partial order model e.g. E={[1 2] [3 4 5]} indicates that
  # condition 1 <= condition 2 and condition 3 <= condition 4 <= condition 5
  # default = none (empty)
  # shrink is parameter to control shrinkage of covariance matrix (if input is not stats form);
  # 0 = no shrinkage; 1 = diagonal matrix; -1 = calculate optimum, default = -1
# output:
  # p = empirical p-value
  # datafit = observed fit of partial order model
  # fits = nsample vector of fits of Monte Carlo samples (it is against this
  # distribution that datafit is compared to calculate p)
  # *************************************************************************
  # converted from matlab 7 February 2018
  # *************************************************************************
  
  ## bring data in list format
  y <- prep_data(data = data, 
                    col_value = col_value, 
                    col_participant = col_participant, 
                    col_dv = col_dv, 
                    col_within = col_within, 
                    col_between = col_between)

  # if (is(data,"data.frame")) {
  #   y = gen2list (data) # convert from general format to list format
  # } else {y = data} 
  
  nvar =length(y)
  if (!is.list(partial)) {partial = adj2list(partial)} # convert from adjacency matrix to list
  
  output = jMRfits(nsample, y, partial, shrink);
  #output = jCMRfitsx(nsample, y, model, partial, shrink) # call java program
  
  output$fits[which(output$fits <= tolerance)] = 0;
  
  return (output)
}



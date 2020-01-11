
print.stacmr <- function (x, 
                          digits = max(3, getOption("digits") - 3), 
                          varnames = TRUE,
                          ...) {
  cat("\n", toupper(deparse(x$call[[1]])), 
      " fit to ", nrow(x$estimate), " data points with call:\n", 
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n",
      sep = "")
  
  if (varnames) {
    cat("DVs:", paste0(names(x$data_list[[1]]), collapse = " & "), "\n")
    cat("Within:",  attr(x$data_list, "varnames")["within"], "\n")
    cat("Between:",  attr(x$data_list, "varnames")["between"], "\n\n") 
  }
  
  cat("Fit value (", attr(x, "value_fit"), "): ", 
      format(x$fit, digits = digits), 
      "\n", 
      sep = "")
  cat("p-value (based on", attr(x, "nsample"), "samples):",  
      x$p, "\n")
  cat("\n")
  invisible(x)
}

summary.stacmr <- function(object, 
                           digits = max(3, getOption("digits") - 3), 
                           ...) {
  
  print(object, digits = digits, varnames = FALSE)
  
  cat("Estimated cell means:\n")
  print.data.frame(object$estimate, digits = digits)
  cat("\n")
  invisible(object$estimate)
}

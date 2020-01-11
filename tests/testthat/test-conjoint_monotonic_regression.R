test_that("cmr() produces known results & basic output", {
  data(delay)
  ## basic version
  st_delay <- cmr(
    data = delay, 
    col_value = "pc", 
    col_participant = "participant",
    col_dv = "structure", 
    col_within = "block", 
    col_between = "delay", 
    nsample = 1000, 
    partial = "auto" 
  )
  ### results from Dunn & Kalish section 5.2
  expect_lt(st_delay$p, 0.22)
  expect_gt(st_delay$p, 0.13)
  
  expect_equal(st_delay$fit,  1.7493, tolerance = 0.0002)
  expect_equal(st_delay$fit_diff,  1.5772, tolerance = 0.0002)
  
  expect_output(print(st_delay), "CMR fit to 8 data points with call:")
  expect_output(print(st_delay), "Fit difference to MR model \\(SSE\\):")
  
  expect_output(summary(st_delay), "CMR fit to 8 data points with call:")
  expect_output(summary(st_delay), "Estimated cell means:")
  expect_output(summary(st_delay), "rule.based information.integration within  between")
  
  ## approximate version
  st_delay_a <- cmr(
    data = delay, 
    col_value = "pc", 
    col_participant = "participant",
    col_dv = "structure", 
    col_within = "block", 
    col_between = "delay", 
    nsample = 1000, 
    partial = "auto", 
    approx = TRUE 
  )
  ### results from Dunn & Kalish section 5.2
  
  expect_equal(st_delay_a$fit,  1.7493, tolerance = 0.0002)
  expect_equal(st_delay_a$fit_diff,  1.5772, tolerance = 0.0002)
  
  expect_output(print(st_delay_a), "CMR fit to 8 data points with call:")
  expect_output(print(st_delay_a), "Fit difference to MR model \\(SSE\\):")
  
  expect_output(summary(st_delay_a), "CMR fit to 8 data points with call:")
  expect_output(summary(st_delay_a), "Estimated cell means:")
  expect_output(summary(st_delay_a), "rule.based information.integration within  between")
  
  st_delay_nt <- cmr(
    data = delay, 
    col_value = "pc", 
    col_participant = "participant",
    col_dv = "structure", 
    col_within = "block", 
    col_between = "delay", 
    test = FALSE,
    partial = "auto" 
  )
  ### results from Dunn & Kalish section 5.2
  
  expect_equal(st_delay_nt$fit,  1.7493, tolerance = 0.0002)
  expect_equal(st_delay_nt$fit_diff,  NA)
  
  expect_output(print(st_delay_nt), "CMR fit to 8 data points with call:")
  expect_output(print(st_delay_nt), "Fit difference to MR model \\(SSE\\): NA")
  
  expect_output(summary(st_delay_nt), "CMR fit to 8 data points with call:")
  expect_output(summary(st_delay_nt), "Estimated cell means:")
  expect_output(summary(st_delay_nt), "rule.based information.integration within  between")
  
})

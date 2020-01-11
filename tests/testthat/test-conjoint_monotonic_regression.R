test_that("mr() produces known results", {
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
})

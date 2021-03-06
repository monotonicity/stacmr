test_that("Only Between-Subjects Data is Supported", {
  dbet <- structure(list(Participant = structure(c(89L, 89L, 11L, 11L, 
    90L, 90L, 42L, 42L, 62L, 62L, 66L, 66L, 40L, 40L, 60L, 60L, 72L, 
    72L, 78L, 78L, 47L, 47L, 75L, 75L, 9L, 9L, 69L, 69L, 92L, 92L, 
    83L, 83L, 39L, 39L, 12L, 12L, 96L, 96L, 10L, 10L, 4L, 4L, 30L, 
    30L, 93L, 93L, 94L, 94L, 84L, 84L, 65L, 65L, 45L, 45L, 3L, 3L, 
    20L, 20L, 85L, 85L, 5L, 5L, 80L, 80L, 70L, 70L, 49L, 49L, 59L, 
    59L, 35L, 35L, 22L, 22L, 71L, 71L, 77L, 77L, 29L, 29L, 51L, 51L, 
    33L, 33L, 54L, 54L, 55L, 55L, 13L, 13L, 82L, 82L, 7L, 7L, 1L, 
    1L, 91L, 91L, 50L, 50L, 52L, 52L, 81L, 81L, 25L, 25L, 68L, 68L, 
    63L, 63L, 44L, 44L, 56L, 56L, 28L, 28L, 76L, 76L, 18L, 18L, 23L, 
    23L, 36L, 36L, 57L, 57L, 37L, 37L, 19L, 19L, 95L, 95L, 58L, 58L, 
    67L, 67L, 6L, 6L, 17L, 17L, 41L, 41L, 86L, 86L, 32L, 32L, 87L, 
    87L, 34L, 34L, 48L, 48L, 88L, 88L, 61L, 61L, 16L, 16L, 21L, 21L, 
    14L, 14L, 38L, 38L, 43L, 43L, 2L, 2L, 64L, 64L, 26L, 26L, 24L, 
    24L, 46L, 46L, 73L, 73L, 31L, 31L, 53L, 53L, 27L, 27L, 74L, 74L, 
    8L, 8L, 79L, 79L, 15L, 15L), .Label = c("AGMX", "AMVX", "ARHV", 
    "AWCT", "BCDX", "BHTE", "BIGC", "BNOA", "BTWB", "CAJQ", "CHBD", 
    "CHOM", "CHVB", "DLTM", "DOFD", "DTBA", "EMUA", "EQJK", "EYUG", 
    "FFWK", "FMJI", "FNGR", "FTBE", "FUIN", "FUNJ", "GBAI", "GCDV", 
    "GGLS", "GOAG", "GQLJ", "HBAR", "HFCX", "HFQO", "HHFT", "HPCY", 
    "HQCN", "HRAF", "HRHX", "HSMR", "IBIO", "IDUX", "IYRT", "JACY", 
    "JDDW", "JECH", "JIRI", "JNUW", "JOVL", "JRRL", "KDRY", "KEBF", 
    "KGKB", "KXDA", "LDNH", "LNHH", "LPVJ", "MFON", "MJTF", "NFCP", 
    "NLAH", "NMMR", "NOOJ", "NPAX", "OOGU", "PITR", "PJFI", "QDTV", 
    "QKTB", "QXER", "RAPA", "RRRB", "SBVY", "SOHR", "SVTS", "TALL", 
    "TLMO", "UDTG", "UGFM", "UIAK", "UKHH", "VGOR", "VKRP", "VQCH", 
    "VQJW", "WFEI", "WFGV", "WHJA", "WNQR", "WWSC", "XGTN", "XWSL", 
    "YEFF", "YILC", "YMYM", "YOHW", "YVBB"), class = "factor"), 
    Item.type = structure(c(2L, 
    1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 
    2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 
    2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 
    2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 
    2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 
    2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 
    2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 
    2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 
    2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 
    1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 
    1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 
    1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L), 
    .Label = c("NEW", "OLD"), class = "factor"), 
    Answer = c(0.26, 0.08, 0.17, 0.19, 
    0.24, 0.13, 0.31, 0.05, 0.12, 0.18, 0.24, 0.26, 0.17, 0.24, 0.13, 
    0.25, 0.1, 0.28, 0.2, 0.1, 0.1, 0.24, 0.19, 0.15, 0.18, 0.12, 
    0.06, 0.14, 0.24, 0.11, 0.19, 0.16, 0.22, 0.26, 0.22, 0.17, 0.09, 
    0.24, 0.21, 0.04, 0.21, 0.26, 0.31, 0.14, 0.06, 0.27, 0.18, 0.04, 
    0.04, 0.29, 0.16, 0.17, 0.12, 0.2, 0.1, 0.32, 0.1, 0.15, 0.25, 
    0.06, 0.31, 0.06, 0.14, 0.22, 0.18, 0.18, 0.26, 0.2, 0.17, 0.12, 
    0.19, 0.18, 0.3, 0.14, 0.08, 0.27, 0.14, 0.26, 0.11, 0.24, 0.15, 
    0.24, 0.26, 0.13, 0.04, 0.2, 0.24, 0.12, 0.18, 0.21, 0.21, 0.12, 
    0.27, 0.06, 0.25, 0.15, 0.06, 0.16, 0.17, 0.07, 0.03, 0.29, 0.12, 
    0.22, 0.24, 0.13, 0.3, 0.08, 0.07, 0.24, 0.15, 0.18, 0.1, 0.17, 
    0.16, 0.04, 0.05, 0.09, 0.23, 0.14, 0.09, 0.16, 0.04, 0.26, 0.31, 
    0.04, 0.26, 0.25, 0.08, 0.21, 0.21, 0.07, 0.31, 0.12, 0.27, 0.03, 
    0.08, 0.18, 0.13, 0.09, 0.13, 0.22, 0.12, 0.24, 0.24, 0.07, 0.13, 
    0.15, 0.13, 0.21, 0.04, 0.31, 0.24, 0.24, 0.06, 0.22, 0.02, 0.3, 
    0.23, 0.15, 0.1, 0.11, 0.24, 0.21, 0.23, 0.14, 0.28, 0.01, 0.22, 
    0.13, 0.14, 0.27, 0.16, 0.23, 0.1, 0.19, 0.27, 0.04, 0.24, 0.16, 
    0.17, 0.26, 0.26, 0.15, 0.03, 0.24, 0.3, 0.03, 0.14, 0.22, 0.31, 
    0.04), Scaletype = c(100L, 100L, 100L, 100L, 100L, 100L, 100L, 
    100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 
    100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 
    100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 
    100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 20L, 20L, 20L, 
    20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 
    20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 
    20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 
    20L, 20L, 20L, 20L, 20L, 20L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
    5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
    5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
    5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
    4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L)), row.names = c(1L, 2L, 401L, 
    402L, 801L, 803L, 1201L, 1206L, 1601L, 1602L, 2001L, 2003L, 2401L, 
    2403L, 2801L, 2802L, 3201L, 3202L, 3601L, 3602L, 4001L, 4002L, 
    4401L, 4403L, 4801L, 4802L, 5201L, 5203L, 5601L, 5604L, 6001L, 
    6002L, 6401L, 6403L, 6801L, 6802L, 7201L, 7202L, 7601L, 7603L, 
    8001L, 8002L, 8401L, 8402L, 8801L, 8806L, 9201L, 9202L, 9601L, 
    9603L, 10001L, 10002L, 10401L, 10406L, 10801L, 10802L, 11201L, 
    11207L, 11601L, 11602L, 12001L, 12002L, 12401L, 12402L, 12801L, 
    12803L, 13201L, 13206L, 13601L, 13607L, 14001L, 14007L, 14401L, 
    14404L, 14801L, 14803L, 15201L, 15202L, 15601L, 15602L, 16001L, 
    16002L, 16401L, 16402L, 16801L, 16802L, 17201L, 17205L, 17601L, 
    17605L, 18001L, 18004L, 18401L, 18403L, 18801L, 18803L, 19201L, 
    19204L, 19601L, 19602L, 20001L, 20002L, 20401L, 20402L, 20801L, 
    20803L, 21201L, 21205L, 21601L, 21604L, 22001L, 22003L, 22401L, 
    22403L, 22801L, 22802L, 23201L, 23202L, 23601L, 23603L, 24001L, 
    24003L, 24401L, 24403L, 24801L, 24803L, 25201L, 25202L, 25601L, 
    25602L, 26001L, 26003L, 26401L, 26404L, 26801L, 26802L, 27201L, 
    27203L, 27601L, 27602L, 28001L, 28004L, 28401L, 28405L, 28801L, 
    28802L, 29201L, 29203L, 29601L, 29603L, 30001L, 30002L, 30401L, 
    30402L, 30801L, 30802L, 31201L, 31202L, 31601L, 31602L, 32001L, 
    32003L, 32401L, 32402L, 32801L, 32802L, 33201L, 33203L, 33601L, 
    33602L, 34001L, 34003L, 34401L, 34402L, 34801L, 34802L, 35201L, 
    35203L, 35601L, 35605L, 36001L, 36005L, 36401L, 36404L, 36801L, 
    36802L, 37201L, 37202L, 37601L, 37602L, 38001L, 38005L), 
    class = "data.frame")
  
  stat1 <- sta_stats(
    data = dbet, 
    col_value = "Answer", 
    col_participant = "Participant",
    col_dv = "Item.type",
    col_between = "Scaletype"
  )
  expect_s3_class(stat1, "sta_stats")
  
  mod1 <- cmr(
    data = dbet, 
    col_value = "Answer", 
    col_participant = "Participant",
    col_dv = "Item.type",
    col_between = "Scaletype", 
    nsample = 10
  )
  expect_s3_class(mod1, "sta_cmr")
  
})

test_that("Only Between-Subjects Data is Supported", {
  
  data(delay)
  
  stats <- sta_stats(
    data = delay, 
    col_value = "pc", 
    col_participant = "participant",
    col_dv = "structure", 
    col_within = "block"
  )
  
  expect_s3_class(stats, "sta_stats")
  
  st_delay <- cmr(
    data = delay, 
    col_value = "pc", 
    col_participant = "participant",
    col_dv = "structure", 
    col_within = "block", 
    nsample = 10, 
    partial = "auto" 
  )
  expect_s3_class(st_delay, "sta_cmr")
})

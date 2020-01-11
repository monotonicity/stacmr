
pkgload::load_all()
.jinit(classpath="inst/java/fxMR-0.3.41-jar-with-dependencies.jar")

devtools::document()

devtools::test()

library("usethis")
### packages
use_package("magic")
# usethis::use_package("tidyr")

### setup

use_roxygen_md()
use_pkgdown()
use_travis()
use_readme_rmd()

library("testthat")
use_testthat()
use_test("monotonic_regression")
use_test("conjoint_monotonic_regression")
use_test("partial_order_specification")

### prepare data

delay <- read.table("../STACMR-R/Data files/delay.dat")
colnames(delay) <- c("participant", "condition", "dv", 
                     "B1","B2","B3","B4")
use_data(delay)
###


## nicer:
library("ggplot2")
staPLOT(data = delay, 
        groups = list(c(1:4), c(5:8)), 
        grouplabels = list("No delay", "Delay"), 
        axislabels = list("RB","II")) +
  theme(panel.background=element_blank(),
        axis.text.x=element_text(colour="black",size=12),
        axis.text.y=element_text(colour="black",size=12),
        axis.line=element_line(colour="black"),
        legend.title = element_blank(),
        legend.background = element_rect(color = "black", size = .5, linetype = "solid"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position = c(.2,.9)
  )


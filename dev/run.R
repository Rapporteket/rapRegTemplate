

devtools::install("../rapbase", upgrade = FALSE)
devtools::install(".", upgrade = FALSE)
source("dev/renv.R")
rapRegTemplate::run_app(browser = TRUE)

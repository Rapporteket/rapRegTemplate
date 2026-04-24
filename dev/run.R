

devtools::install(".", upgrade = FALSE, dependencies = FALSE)
devtools::install("../rapbase", upgrade = FALSE, dependencies = FALSE)
source("dev/renv.R")
rapRegTemplate::run_app(browser = TRUE)

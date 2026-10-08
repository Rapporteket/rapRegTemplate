

devtools::install(".", upgrade = FALSE, dependencies = FALSE)
devtools::install("../rapbase", upgrade = FALSE, dependencies = FALSE)
devtools::install("../rapFigurer", upgrade = FALSE, dependencies = FALSE)
source("dev/renv.R")
source("../../!Sikker lagring/renv.R")
rapRegTemplate::run_app(browser = TRUE)

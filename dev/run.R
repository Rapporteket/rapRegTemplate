
devtools::install(".", upgrade = FALSE, dependencies = FALSE)
devtools::install("../rapbase", upgrade = FALSE, dependencies = FALSE)
devtools::install("../rapFigurer", upgrade = FALSE, dependencies = FALSE)

source("dev/renv.R")
source("../../!Sikker lagring/renv.R")

# For lokal testing (vil kjøre uten databaser)
# Sys.setenv(R_RAP_INSTANCE = "azure")

rapRegTemplate::run_app(browser = TRUE)

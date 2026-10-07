

devtools::install(".", upgrade = FALSE, dependencies = FALSE)
devtools::install("../rapbase", upgrade = FALSE, dependencies = FALSE)
source("dev/renv.R")

# sqlite database setup
#Sys.setenv(DB_TYPE = "sqlite")
#Sys.setenv(MYSQL_DB_DATA = ":memory:")
#create_sqlite_db()

rapRegTemplate::run_app(browser = TRUE)

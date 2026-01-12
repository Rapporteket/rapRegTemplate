

devtools::install("../rapbase", upgrade = FALSE)
source("dev/renv.R")
rapRegTemplate::run_app(browser = TRUE)


con <- rapbase::rapOpenDbConnection("autoreport")$con

conf <- rapbase:::getDbConfig("autoreport")

con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "FreeTDS",
  database = conf$name,
  server = conf$host,
  uid = conf$user,
  pwd = conf$pass,
  port = 1433
)

con <- DBI::dbDisconnect(con)
con <- NULL

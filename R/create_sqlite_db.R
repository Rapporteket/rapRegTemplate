#' Create log and autoreport sqlite database files and tables
#'
#' This function will create two files that can be used as database
#' when we do not have access to database infrastructure.
#' It will create the necessary tables within these files.
#' Please be aware that this will delete any existing tables
#' with the same names!
#'
#' @return No return value, called for side effects
#' (creates SQLite database files and tables)
#' @export
create_sqlite_db <- function() {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Missing RSQLite package! Please install.packages('RSQLite').")
  }
  if (Sys.getenv("DB_TYPE") != "sqlite") {
    return()
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = Sys.getenv("MYSQL_DB_AUTOREPORT"))

  query <- paste0(
    "DROP TABLE IF EXISTS `autoreport`;"
  )
  DBI::dbExecute(con, query)

  query <- paste0(
    "CREATE TABLE `autoreport` (",
    "  id varchar(255) DEFAULT NULL,",
    "  synopsis varchar(255) DEFAULT NULL,",
    "  package varchar(255) DEFAULT NULL,",
    "  fun varchar(255) DEFAULT NULL,",
    "  params varchar(1025),",
    "  owner varchar(255) DEFAULT NULL,",
    "  email varchar(255) DEFAULT NULL,",
    "  organization varchar(255) DEFAULT NULL,",
    "  terminateDate varchar(255) DEFAULT NULL,",
    "  `interval` varchar(255) DEFAULT NULL,",
    "  intervalName varchar(255) DEFAULT NULL,",
    "  runDayOfYear varchar(255) DEFAULT NULL,",
    "  `type` varchar(255) DEFAULT NULL,",
    "  ownerName varchar(255) DEFAULT NULL,",
    "  startDate varchar(255) DEFAULT NULL",
    ");"
  )

  DBI::dbExecute(con, query)

  DBI::dbDisconnect(con)

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = Sys.getenv("MYSQL_DB_LOG"))

  query <- paste0(
    "DROP TABLE IF EXISTS `appLog`;"
  )
  DBI::dbExecute(con, query)

  query <- paste0(
    "CREATE TABLE `appLog` (",
    "  id varchar(255) DEFAULT NULL,",
    " `time` datetime DEFAULT NULL,",
    "  `user` varchar(255) DEFAULT NULL,",
    "  name varchar(255) DEFAULT NULL,",
    "  `group` varchar(255) DEFAULT NULL,",
    "  role varchar(255) DEFAULT NULL,",
    "  resh_id varchar(255) DEFAULT NULL,",
    "  message text DEFAULT NULL",
    ");"
  )

  DBI::dbExecute(con, query)

  query <- paste0(
    "DROP TABLE IF EXISTS `reportLog`;"
  )
  DBI::dbExecute(con, query)

  query <- paste0(
    "CREATE TABLE `reportLog` (",
    "  id varchar(255) DEFAULT NULL,",
    "  `time` datetime DEFAULT NULL,",
    "  `user` varchar(255) DEFAULT NULL,",
    "  name varchar(255) DEFAULT NULL,",
    "  `group` varchar(255) DEFAULT NULL,",
    "  role varchar(255) DEFAULT NULL,",
    "  resh_id varchar(255) DEFAULT NULL,",
    "  environment varchar(255) DEFAULT NULL,",
    "  `call` varchar(2047) DEFAULT NULL,",
    "  message text DEFAULT NULL",
    ");"
  )

  DBI::dbExecute(con, query)

  DBI::dbDisconnect(con)

}

#' Prepare Azure environment and create SQLite databases
#'
#' This function sets up the necessary environment variables for running the
#' application in Azure and creates the required SQLite database files and
#' tables.
#'
#' @return No return value, called for side effects
#' (sets environment variables and creates SQLite databases)
#' @keywords internal
#'
azure_prep <- function() {

  define_azure_env()

  create_sqlite_db()
}

#' Define Azure environment variables
#'
#' This function sets the necessary environment variables for running the
#' application in Azure.
#'
#' @return No return value, called for side effects (sets environment variables)
#' @keywords internal
#'
define_azure_env <- function() {
  Sys.setenv(DB_TYPE = "sqlite")
  Sys.setenv(FALK_EXTENDED_USER_RIGHTS = "[
{\"A\":80,\"R\":\"SC\",\"U\":111},
{\"A\":80,\"R\":\"LU\",\"U\":111},
{\"A\":81,\"R\":\"LC\",\"U\":111},
{\"A\":80,\"R\":\"SC\",\"U\":222},
{\"A\":80,\"R\":\"LC\",\"U\":222},
{\"A\":81,\"R\":\"LC\",\"U\":222},
{\"A\":80,\"R\":\"SC\",\"U\":333},
{\"A\":80,\"R\":\"LC\",\"U\":333},
{\"A\":81,\"R\":\"LC\",\"U\":333}
]")
  Sys.setenv(FALK_APP_ID = "80")

  Sys.setenv(MYSQL_DB_LOG = "db_log")
  Sys.setenv(MYSQL_DB_AUTOREPORT = "db_autoreport")
  Sys.setenv(MYSQL_DB_DATA = ":memory:")
  Sys.setenv(SHINYPROXY_USERNAME = "rapporteket")
  Sys.setenv(SHINYPROXY_APPID = "tech")
  Sys.setenv(FALK_USER_FULLNAME = "Rapp O. R. Teket")
  Sys.setenv(FALK_USER_EMAIL = "rapporteket@skde.no")
  Sys.setenv(FALK_USER_PHONE = "+4747474747")

}

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
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = Sys.getenv("MYSQL_DB_AUTOREPORT")
  )

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
    "  id INTEGER PRIMARY KEY AUTOINCREMENT,",
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
    "  id INTEGER PRIMARY KEY AUTOINCREMENT,",
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

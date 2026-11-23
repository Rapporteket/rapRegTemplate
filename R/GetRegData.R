#' Load registry data
#'
#' Fetches the data used by the registry application by running a SQL query
#' through `rapbase`. This function is a template; replace
#' `YOUR_REGISTRY_TABLE` with the name of the table for the registry and
#' adjust the query to select the variables needed by the application.
#'
#' The database connection and configuration are handled by
#' `rapbase::loadRegData()`. The returned object is therefore dependent on
#' the active registry and database environment.
#'
#' @return A data frame containing the rows and columns returned by the SQL
#'   query.
#'
#' @seealso [?rapbase::loadRegData()] for loading registry data
#'
#' @export

getRegData <- function() {

  # nocov start
  query <- "
    SELECT
      *,
      COUNT(*) AS n
    FROM
      YOUR_REGISTRY_TABLE;
    "

  rapbase::loadRegData("data", query)
  # nocov end

}

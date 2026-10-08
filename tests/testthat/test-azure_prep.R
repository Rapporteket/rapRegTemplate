test_that("define_azure_env sets the expected SQLite configuration", {
  env_vars <- c(
    "DB_TYPE",
    "FALK_EXTENDED_USER_RIGHTS",
    "FALK_APP_ID",
    "MYSQL_DB_LOG",
    "MYSQL_DB_AUTOREPORT",
    "MYSQL_DB_DATA",
    "SHINYPROXY_USERNAME",
    "SHINYPROXY_APPID",
    "FALK_USER_FULLNAME",
    "FALK_USER_EMAIL",
    "FALK_USER_PHONE"
  )

  old_values <- Sys.getenv(env_vars, unset = NA_character_)
  on.exit({
    for (name in env_vars) {
      value <- old_values[[name]]
      if (is.na(value)) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(list(value), name))
      }
    }
  }, add = TRUE)

  for (name in env_vars) {
    Sys.unsetenv(name)
  }

  rapRegTemplate:::define_azure_env()

  expect_equal(Sys.getenv("DB_TYPE"), "sqlite")
  expect_equal(Sys.getenv("FALK_APP_ID"), "80")
  expect_equal(Sys.getenv("SHINYPROXY_USERNAME"), "rapporteket")
  expect_equal(Sys.getenv("FALK_USER_EMAIL"), "rapporteket@skde.no")
  expect_true(grepl('"A":80', Sys.getenv("FALK_EXTENDED_USER_RIGHTS")))
  expect_equal(Sys.getenv("MYSQL_DB_DATA"), ":memory:")
})

test_that("create_sqlite_db creates SQLite files and required tables", {
  skip_if_not_installed("RSQLite")

  tmp_dir <- tempfile("azure_prep_")
  dir.create(tmp_dir)
  autoreport_path <- file.path(tmp_dir, "autoreport.sqlite")
  log_path <- file.path(tmp_dir, "app_log.sqlite")

  env_vars <- c(
    "DB_TYPE",
    "MYSQL_DB_AUTOREPORT",
    "MYSQL_DB_LOG"
  )

  old_values <- Sys.getenv(env_vars, unset = NA_character_)
  on.exit({
    for (name in env_vars) {
      value <- old_values[[name]]
      if (is.na(value)) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(list(value), name))
      }
    }
  }, add = TRUE)

  Sys.setenv(
    DB_TYPE = "sqlite",
    MYSQL_DB_AUTOREPORT = autoreport_path,
    MYSQL_DB_LOG = log_path
  )

  on.exit({
    if (file.exists(autoreport_path)) {
      unlink(autoreport_path, force = TRUE)
    }
    if (file.exists(log_path)) {
      unlink(log_path, force = TRUE)
    }
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  rapRegTemplate:::create_sqlite_db()

  expect_true(file.exists(autoreport_path))
  expect_true(file.exists(log_path))

  autoreport_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = Sys.getenv("MYSQL_DB_AUTOREPORT"))
  on.exit(DBI::dbDisconnect(autoreport_con), add = TRUE)

  expect_setequal(DBI::dbListTables(autoreport_con), "autoreport")
  expect_true(all(c(
    "id",
    "synopsis",
    "package",
    "fun",
    "params",
    "owner",
    "email",
    "organization",
    "terminateDate",
    "interval",
    "intervalName",
    "runDayOfYear",
    "type",
    "ownerName",
    "startDate"
  ) %in% DBI::dbListFields(autoreport_con, "autoreport")))

  log_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = Sys.getenv("MYSQL_DB_LOG"))
  on.exit(DBI::dbDisconnect(log_con), add = TRUE)

  expect_setequal(DBI::dbListTables(log_con), c("appLog", "reportLog"))
  expect_true(all(c("id", "time", "user", "name", "group", "role", "resh_id", "message") %in% DBI::dbListFields(log_con, "appLog")))
  expect_true(all(c("id", "time", "user", "name", "group", "role", "resh_id", "environment", "call", "message") %in% DBI::dbListFields(log_con, "reportLog")))
})

test_that("azure_prep creates the environment and SQLite database files", {
  skip_if_not_installed("RSQLite")

  env_vars <- c(
    "DB_TYPE",
    "MYSQL_DB_AUTOREPORT",
    "MYSQL_DB_LOG"
  )

  old_values <- Sys.getenv(env_vars, unset = NA_character_)
  on.exit({
    for (name in env_vars) {
      value <- old_values[[name]]
      if (is.na(value)) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(list(value), name))
      }
    }
    if (file.exists("db_autoreport")) {
      unlink("db_autoreport", force = TRUE)
    }
    if (file.exists("db_log")) {
      unlink("db_log", force = TRUE)
    }
  }, add = TRUE)

  Sys.unsetenv("DB_TYPE")
  Sys.unsetenv("MYSQL_DB_AUTOREPORT")
  Sys.unsetenv("MYSQL_DB_LOG")

  rapRegTemplate:::azure_prep()

  expect_equal(Sys.getenv("DB_TYPE"), "sqlite")
  expect_equal(Sys.getenv("MYSQL_DB_AUTOREPORT"), "db_autoreport")
  expect_equal(Sys.getenv("MYSQL_DB_LOG"), "db_log")
  expect_true(file.exists("db_autoreport"))
  expect_true(file.exists("db_log"))
})

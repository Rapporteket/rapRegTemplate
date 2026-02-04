

devtools::install(".", upgrade = FALSE)
devtools::install("../rapbase", upgrade = FALSE)
source("dev/renv.R")
rapRegTemplate::run_app(browser = TRUE)


query <- paste0("SELECT * FROM autoreport;")
res <- rapbase::loadRegData("autoreport", query)
nms <- names(res) |>
  setdiff(c("id", "email"))

rest2 <- res |>
  dplyr::summarise(
    email = list(unique(email)),
    .by = nms
  )

#' Hent og flatt ut nestede SKDE-indikatordata
#'
#' Henter indikatorpayload for et register fra SKDE-API-et og omformer hver
#' nestet observasjon til en rad per registrering i en data.frame med relevante
#' indikatorverdier og metadata.
#'
#' @param registryShortname Kort navn på registret som skal hentes, for eksempel "hjertestans".
#' @param indicator_id Valgfri indikator-ID som brukes til å filtrere data før flattening.
#' @return En data.frame med én rad per registrering og kolonner: year, orgnr,
#' var, denominator, ind_id, context, title, short_description, levelDirection
#' og kvalIndgrenser. Hvis ingen treff finnes, returneres en tom data.frame med
#' samme kolonner.
#' @export
fetchSkdeIndicatorData <- function(
  registryShortname = "hjertestans",
  indicator_id = NULL
) {

  url <- paste0("https://prod-api.skde.org/data/", registryShortname, "/nestedData")

  response <- httr::GET(url)
  payload <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)

  rows <- do.call(rbind, lapply(payload, function(register) {
    do.call(rbind, lapply(register$indicatorData, function(indicator) {
      if (!is.null(indicator_id) && !is.null(indicator$indicatorID) && indicator$indicatorID != indicator_id) {
        return(NULL)
      }

      kval_values <- c(indicator$levelYellow, indicator$levelGreen)
      if (length(kval_values) == 0L) {
        kval_values <- NULL
      }

      indicator_meta <- data.frame(
        ind_id = as.character(indicator$indicatorID),
        title = as.character(indicator$indicatorTitle),
        short_description = as.character(indicator$shortDescription),
        levelDirection = if (is.null(indicator$levelDirection)) NA_real_ else as.numeric(indicator$levelDirection),
        kvalIndgrenser = I(list(kval_values)),
        stringsAsFactors = FALSE
      )

      row_data <- do.call(rbind, lapply(indicator$data, function(row) {
        if (is.null(row$year) || is.null(row$unitName) || is.null(row$var) || is.null(row$denominator)) {
          return(NULL)
        }

        data.frame(
          year = as.integer(row$year),
          orgnr = as.character(row$unitName),
          var = as.numeric(row$var),
          denominator = as.numeric(row$denominator),
          ind_id = as.character(indicator$indicatorID),
          context = as.character(row$context),
          stringsAsFactors = FALSE
        )
      }))

      if (is.null(row_data) || nrow(row_data) == 0) {
        return(NULL)
      }

      merge(row_data, indicator_meta, by = "ind_id", all.x = TRUE)
    }))
  }))

  if (is.null(rows) || nrow(rows) == 0) {
    return(data.frame(
      year = integer(),
      orgnr = character(),
      var = numeric(),
      denominator = numeric(),
      ind_id = character(),
      context = character(),
      title = character(),
      short_description = character(),
      levelDirection = numeric(),
      kvalIndgrenser = list(),
      stringsAsFactors = FALSE
    ))
  }

  data <- rows[, c("year", "orgnr", "var", "denominator",
                   "ind_id", "context", "title", "short_description",
                   "levelDirection", "kvalIndgrenser")]
  indicator_meta <- unique(
    data[
      ,
      c("ind_id", "title", "short_description", "kvalIndgrenser", "levelDirection"),
      drop = FALSE
    ]
  )
  return(list(data = data, indicator_meta = indicator_meta))
}

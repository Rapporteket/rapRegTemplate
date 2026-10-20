#' Hent og flatt ut nestede SKDE-indikatordata
#'
#' Henter indikatorpayload for et register fra SKDE-API-et og omformer hver
#' nestet observasjon til en rad per registrering i en data.frame med relevante
#' indikatorverdier og metadata.
#'
#' @param registryShortname Kort navn på registret som skal hentes, for eksempel "hjertestans".
#' @param indicator_id Valgfri indikator-ID som brukes til å filtrere data før flattening.
#' @return En liste med to data.frames: \code{data} (én rad per registrering,
#' med kolonnene year, orgnr, var, denominator, ind_id, context, title,
#' short_description, levelDirection og kvalIndgrenser) og
#' \code{indicator_meta} (unik metadata per indikator med kolonnene ind_id,
#' title, short_description, kvalIndgrenser og levelDirection). Hvis ingen
#' treff finnes, returneres begge som tomme data.frames med samme skjema.
#' @export
fetchSkdeIndicatorData <- function(
  registryShortname = "hjertestans",
  indicator_id = NULL
) {

  scalar_chr <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      return(NA_character_)
    }
    as.character(x[[1]])
  }

  scalar_num <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      return(NA_real_)
    }
    as.numeric(x[[1]])
  }

  empty_data <- data.frame(
    year = integer(),
    orgnr = character(),
    unitName = character(),
    var = numeric(),
    denominator = numeric(),
    ind_id = character(),
    context = character(),
    title = character(),
    short_description = character(),
    levelDirection = numeric(),
    kvalIndgrenser = list(),
    stringsAsFactors = FALSE
  )

  empty_indicator_meta <- data.frame(
    ind_id = character(),
    title = character(),
    short_description = character(),
    kvalIndgrenser = list(),
    levelDirection = numeric(),
    stringsAsFactors = FALSE
  )

  url <- paste0("https://prod-api.skde.org/data/", registryShortname, "/nestedData")

  response <- httr::GET(url)
  payload <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)

  unitNametoOrgNr <- list()
  orgnr_counter <- 100

  rows <- do.call(rbind, lapply(payload, function(register) {
    do.call(rbind, lapply(register$indicatorData, function(indicator) {
      indicator_id_value <- scalar_chr(indicator$indicatorID)
      if (!is.null(indicator_id) && (is.na(indicator_id_value) || indicator_id_value != as.character(indicator_id))) {
        return(NULL)
      }

      kval_values <- c(indicator$levelYellow, indicator$levelGreen)
      if (length(kval_values) == 0L) {
        kval_values <- NULL
      }

      indicator_meta <- data.frame(
        ind_id = indicator_id_value,
        title = scalar_chr(indicator$indicatorTitle),
        short_description = scalar_chr(indicator$shortDescription),
        levelDirection = scalar_num(indicator$levelDirection),
        kvalIndgrenser = I(list(kval_values)),
        stringsAsFactors = FALSE
      )

      row_data <- do.call(rbind, lapply(indicator$data, function(row) {
        if (is.null(row$year) || is.null(row$unitName) || is.null(row$var) || is.null(row$denominator)) {
          return(NULL)
        }

        unitName <- as.character(row$unitName)
        if (!unitName %in% names(unitNametoOrgNr)) {
          unitNametoOrgNr[[unitName]] <<- as.character(orgnr_counter)
          orgnr_counter <<- orgnr_counter + 1
        }

        data.frame(
          year = as.integer(row$year),
          orgnr = unitNametoOrgNr[[unitName]],
          unitName = unitName,
          var = as.numeric(row$var),
          denominator = as.numeric(row$denominator),
          ind_id = indicator_id_value,
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
    return(list(data = empty_data, indicator_meta = empty_indicator_meta))
  }

  data <- rows[, c("year", "orgnr", "unitName", "var", "denominator",
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

# Funksjoner til modulen "mod_fordeling"

# Standard filtreringsfunksjon



#' Preprosessering
#' @param data datasett "licorice gargle"
#' @return datasett med norske nivåer og verdier
#' @export

forbered_data_fordeling <- function(data) {

  # Endre nivåene
  data <- data |>
    dplyr::mutate(
      preOp_gender = dplyr::recode_values(
        .data$preOp_gender,
        0 ~ "mann",
        1 ~ "kvinne"
      ),
      preOp_smoking = dplyr::recode_values(
        .data$preOp_smoking,
        1 ~ "Naa",
        2 ~ "Foer",
        3 ~ "Aldri"
      ),
      preOp_pain = dplyr::recode_values(
        .data$preOp_pain,
        0 ~ "Nei",
        1 ~ "Ja"
      ),
      treat = dplyr::recode_values(
        .data$treat,
        0 ~ "Sukker",
        1 ~ "Lakris"
      ),
      extubation_cough = dplyr::recode_values(
        .data$extubation_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      pacu30min_cough = dplyr::recode_values(
        .data$pacu30min_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      pacu90min_cough = dplyr::recode_values(
        .data$pacu90min_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      postOp4hour_cough = dplyr::recode_values(
        .data$postOp4hour_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      pod1am_cough = dplyr::recode_values(
        .data$pod1am_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      intraOp_surgerySize = dplyr::recode_values(
        .data$intraOp_surgerySize,
        1 ~ "liten",
        2 ~ "medium",
        3 ~ "stor"
      )
    )

  data$preOp_calcBMI_cat <- cut(
    data$preOp_calcBMI,
    breaks = c(0, 18.5, 24.9, 29.9, 34.4, 39.9, 100),
    labels = c("undervektig", "normal", "overvektig", "fedme", "moderat fedme", "alvorlig fedme")
  )

  return(data)
}


#' Funksjon som gjør utvalg basert på  ui-valg
#'
#' @param data datafil som utgangspunkt
#' @param alder1 minste alder
#' @param roek røykestatus
#' @param alder2 høyeste alder
#'
#' @return datafil der utvalg er gjort
#' @export


utvalg_fordeling <- function(data, alder1, alder2, roek) {

  data <- data |>
    dplyr::filter(dplyr::between(.data$preOp_age, {{alder1}}, {{alder2}}))


  data <- data |>
    dplyr::filter(.data$preOp_smoking == dplyr::case_when(
      {{roek}} == "Naa" ~ "Naa",
      {{roek}} == "Foer" ~ "Foer",
      {{roek}} == "Aldri" ~ "Aldri",
      {{roek}} == "alle_valg" ~ preOp_smoking
    ))
  return(data)
}


#' Tabell som viser fordeling, med villkår basert på ui-valg
#' @param data datafil som har vært gjennom forbered_data_fordeling() og utvalg_fordeling()
#' @param var variabel valgt av bruker i ui-delen
#' @param valg_sammenligne_grupper "ja" eller "nei" valg av bruker i ui-delen
#' @param var_sammenligne variabel for sammenligning valg av bruker i ui-delen
#' @return datasett med antall i hver gruppe
#' @export

lag_fordeling_tabell <- function(data, var, valg_sammenligne_grupper, var_sammenligne) {

  if (valg_sammenligne_grupper == "Ja") {
    tabell <- data |>
      dplyr::group_by(.data[[var_sammenligne]]) |>
      dplyr::count(.data[[var]]) |>
      dplyr::rename("antall" = "n")
  } else {
    tabell <- data |>
      dplyr::count(.data[[var]]) |>
      dplyr::rename("antall" = "n")
  }
  return(tabell)
}


#' Plot: fordeling
#'
#' @param data datafil som har vært gjennom forbered_data_fordeling() og utvalg_fordeling()
#' @param var variabel valgt av bruker i ui-delen
#' @param valg_sammenligne_grupper "ja" eller "nei" valg av bruker i ui-delen
#' @param y_lab_size størrelse på y-akse tekst
#' @param x_lab_size størrelse på x-akse tekst
#' @param var_sammenligne variabel for sammenligning valg av bruker i ui-delen
#'
#' @return ggplot2-object som viser fordeling pr. gruppe
#' @export

lag_fordeling_plot <- function(data, var, valg_sammenligne_grupper, var_sammenligne,
                               y_lab_size = 12, x_lab_size = 12) {

  fordeling_plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_bar(fill = "#6CACE4", alpha = .7) +
    ggplot2::ylab("Antall") +
    ggplot2::xlab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(vjust = 3, size = 15, face = "bold"),
      axis.text.x = ggplot2::element_text(size = y_lab_size),
      axis.text.y = ggplot2::element_text(size = x_lab_size)
    )

  if (valg_sammenligne_grupper == "Ja") {
    fordeling_plot <- fordeling_plot +
      ggplot2::facet_wrap(~ .data[[var_sammenligne]])
  }

  return(fordeling_plot)

}


#' Plot: gruppert fordeling
#'
#' @param data datafil som har vært gjennom forbered_data_fordeling() og utvalg_fordeling()
#' @param fordelingsVariabel variabel for fordeling, NULL for totalt
#' @param binVariabel variabel for binning
#' @param binNavn optional mapping for bin values
#' @param colors optional color palette
#' @param totalLabel label for total row
#' @param yLabel label for y-axis
#' @param fillLabel label for fill legend
#'
#' @return ggplot2-object som viser fordeling pr. gruppe
#' @export

plotGruppertFordeling <- function(
  data,
  fordelingsVariabel = NULL,
  binVariabel,
  binNavn = NULL,
  colors = NULL,
  totalLabel = "Totalt",
  yLabel = "Andel",
  fillLabel = NULL
) {
  if (is.null(fillLabel)) {
    fillLabel <- binVariabel
  }

  data_long <- data |>
    dplyr::filter(!is.na(.data[[binVariabel]]))

  if (is.null(fordelingsVariabel)) {
    data_long <- data_long |>
      dplyr::mutate(
        group = totalLabel,
        bin = as.character(.data[[binVariabel]])
      )
  } else {
    data_long <- data_long |>
      dplyr::filter(!is.na(.data[[fordelingsVariabel]])) |>
      dplyr::mutate(
        group = as.character(.data[[fordelingsVariabel]]),
        bin = as.character(.data[[binVariabel]])
      )
  }

  if (!is.null(binNavn)) {
    data_long <- data_long |>
      dplyr::mutate(
        bin = dplyr::recode(.data$bin, !!!binNavn)
      )
  }

  bin_levels <- unique(data_long$bin)

  data_long <- data_long |>
    dplyr::mutate(
      bin = factor(.data$bin, levels = bin_levels)
    )

  if (is.null(fordelingsVariabel)) {
    data_plot <- data_long
  } else {
    total_row <- data_long |>
      dplyr::mutate(group = totalLabel)

    data_plot <- dplyr::bind_rows(data_long, total_row)
  }

  data_plot <- data_plot |>
    dplyr::count(.data$group, .data$bin, name = "n") |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(
      prop = .data$n / sum(.data$n),
      tooltip = paste0(
        "<br>", .data$bin,
        "<br>Antall: ", .data$n,
        "<br>Andel: ", scales::percent(.data$prop, accuracy = 0.1)
      )
    ) |>
    dplyr::ungroup()

  if (is.null(fordelingsVariabel)) {
    data_plot <- data_plot |>
      dplyr::mutate(
        group = factor(.data$group, levels = totalLabel)
      )
  } else {
    data_plot <- data_plot |>
      dplyr::mutate(
        group = factor(
          .data$group,
          levels = c(
            sort(unique(.data$group[.data$group != totalLabel])),
            totalLabel
          )
        )
      )
  }

  p <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(
      x = .data$group,
      y = .data$prop,
      fill = .data$bin,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      y = yLabel,
      fill = fillLabel
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "black"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title.x = ggplot2::element_text(size = 11),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = 15,
        r = 70,
        b = 15,
        l = 20
      )
    )

  if (!is.null(colors)) {
    p <- p +
      ggplot2::scale_fill_manual(
        values = colors,
        drop = FALSE
      )
  }

  p
}

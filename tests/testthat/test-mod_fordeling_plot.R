make_fordeling_test_data <- function() {
	data.frame(
		preOp_age = c(18, 27, 35, 44, 53, 62),
		preOp_gender = c(0, 1, 0, 1, 0, 1),
		preOp_smoking = c(1, 2, 3, 1, 2, 3),
		preOp_pain = c(0, 1, 0, 1, 0, 1),
		treat = c(0, 1, 1, 0, 1, 0),
		extubation_cough = c(0, 1, 2, 3, 1, 0),
		pacu30min_cough = c(0, 1, 2, 3, 0, 1),
		pacu90min_cough = c(1, 2, 3, 0, 1, 2),
		postOp4hour_cough = c(2, 3, 0, 1, 2, 3),
		pod1am_cough = c(3, 0, 1, 2, 3, 0),
		intraOp_surgerySize = c(1, 2, 3, 1, 2, 3),
		preOp_calcBMI = c(19, 24, 28, 33, 36, 42),
		stringsAsFactors = FALSE
	)
}

test_that("mod_fordeling_plot_ui exposes expected controls and outputs", {
	ui <- mod_fordeling_plot_ui("fordeling")
	ui_text <- as.character(ui)

	expect_true(inherits(ui, "shiny.tag.list"))
	expect_match(ui_text, "fordeling-x_var")
	expect_match(ui_text, "fordeling-alder_var")
	expect_match(ui_text, "fordeling-roeking")
	expect_match(ui_text, "fordeling-fordeling_plot")
	expect_match(ui_text, "fordeling-fordeling_tabell")
	expect_match(ui_text, "fordeling-gruppert_fordeling_plot")
})

test_that("mod_fordeling_plot_server reactives work without group comparison", {
	shiny::testServer(
		mod_fordeling_plot_server,
		args = list(data = make_fordeling_test_data()),
		{
			session$setInputs(
				alder_var = c(0, 100),
				roeking = "alle_valg",
				x_var = "preOp_pain",
				sammenligne_grupper = "Nei",
				var_sammenligning = "preOp_gender"
			)

			tabell <- tabell_reactive()
			plot <- plot_reactive()
			gruppert_plot <- plot_fordeling_reactive()

			expect_true(nrow(tabell) > 0)
			expect_s3_class(plot, "ggplot")
			expect_s3_class(plot$facet, "FacetNull")
			expect_s3_class(gruppert_plot, "ggplot")
			expect_equal(unique(as.character(gruppert_plot$data$group)), "Totalt")
		}
	)
})

test_that("mod_fordeling_plot_server reactives support group comparison", {
	shiny::testServer(
		mod_fordeling_plot_server,
		args = list(data = make_fordeling_test_data()),
		{
			session$setInputs(
				alder_var = c(20, 60),
				roeking = "alle_valg",
				x_var = "preOp_pain",
				sammenligne_grupper = "Ja",
				var_sammenligning = "preOp_gender"
			)

			tabell <- tabell_reactive()
			plot <- plot_reactive()
			gruppert_plot <- plot_fordeling_reactive()

			expect_true(all(c("preOp_gender", "preOp_pain", "antall") %in% names(tabell)))
			expect_s3_class(plot$facet, "FacetWrap")
			expect_true("Totalt" %in% as.character(gruppert_plot$data$group))
		}
	)
})

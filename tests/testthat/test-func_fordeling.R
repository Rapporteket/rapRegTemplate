test_that("forbered_data_fordeling recodes values and creates BMI category", {
	data <- data.frame(
		preOp_gender = c(0, 1),
		preOp_smoking = c(1, 3),
		preOp_pain = c(0, 1),
		treat = c(0, 1),
		extubation_cough = c(0, 3),
		pacu30min_cough = c(1, 2),
		pacu90min_cough = c(2, 1),
		postOp4hour_cough = c(3, 0),
		pod1am_cough = c(1, 2),
		intraOp_surgerySize = c(1, 3),
		preOp_calcBMI = c(24, 35),
		stringsAsFactors = FALSE
	)

	result <- forbered_data_fordeling(data)

	expect_equal(result$preOp_gender, c("mann", "kvinne"))
	expect_equal(result$preOp_smoking, c("Naa", "Aldri"))
	expect_equal(result$preOp_pain, c("Nei", "Ja"))
	expect_equal(result$treat, c("Sukker", "Lakris"))
	expect_equal(result$intraOp_surgerySize, c("liten", "stor"))
	expect_true(is.factor(result$preOp_calcBMI_cat))
	expect_equal(as.character(result$preOp_calcBMI_cat), c("normal", "moderat fedme"))
})

test_that("utvalg_fordeling filters by age interval and smoking status", {
	data <- data.frame(
		preOp_age = c(20, 30, 40),
		preOp_smoking = c("Naa", "Foer", "Aldri"),
		stringsAsFactors = FALSE
	)

	result_single <- utvalg_fordeling(data, alder1 = 25, alder2 = 40, roek = "Foer")
	result_all <- utvalg_fordeling(data, alder1 = 25, alder2 = 40, roek = "alle_valg")

	expect_equal(nrow(result_single), 1)
	expect_equal(result_single$preOp_smoking, "Foer")
	expect_equal(result_single$preOp_age, 30)

	expect_equal(nrow(result_all), 2)
	expect_equal(sort(result_all$preOp_smoking), c("Aldri", "Foer"))
})

test_that("lag_fordeling_tabell counts with and without comparison groups", {
	data <- data.frame(
		treat = c("Lakris", "Lakris", "Sukker"),
		preOp_gender = c("mann", "kvinne", "mann"),
		stringsAsFactors = FALSE
	)

	tabell_uten <- lag_fordeling_tabell(
		data,
		var = "treat",
		valg_sammenligne_grupper = "Nei",
		var_sammenligne = "preOp_gender"
	)

	tabell_med <- lag_fordeling_tabell(
		data,
		var = "treat",
		valg_sammenligne_grupper = "Ja",
		var_sammenligne = "preOp_gender"
	)

	expect_true(all(c("treat", "antall") %in% names(tabell_uten)))
	expect_equal(sum(tabell_uten$antall), 3)

	expect_true(all(c("preOp_gender", "treat", "antall") %in% names(tabell_med)))
	expect_equal(sum(tabell_med$antall), 3)
	expect_equal(nrow(tabell_med), 3)
})

test_that("lag_fordeling_plot returns ggplot and optional faceting", {
	data <- data.frame(
		treat = c("Lakris", "Lakris", "Sukker"),
		preOp_gender = c("mann", "kvinne", "mann"),
		stringsAsFactors = FALSE
	)

	plot_uten <- lag_fordeling_plot(
		data,
		var = "treat",
		valg_sammenligne_grupper = "Nei",
		var_sammenligne = "preOp_gender"
	)

	plot_med <- lag_fordeling_plot(
		data,
		var = "treat",
		valg_sammenligne_grupper = "Ja",
		var_sammenligne = "preOp_gender"
	)

	expect_s3_class(plot_uten, "ggplot")
	expect_s3_class(plot_med, "ggplot")
	expect_s3_class(plot_uten$facet, "FacetNull")
	expect_s3_class(plot_med$facet, "FacetWrap")
})

test_that("plotGruppertFordeling includes totals and valid proportions", {
	data <- data.frame(
		preOp_smoking = c("Naa", "Naa", "Foer", "Foer"),
		treat = c("Lakris", "Sukker", "Lakris", "Lakris"),
		stringsAsFactors = FALSE
	)

	plot <- plotGruppertFordeling(
		data = data,
		fordelingsVariabel = "preOp_smoking",
		binVariabel = "treat",
		totalLabel = "Totalt",
		yLabel = "Andel",
		fillLabel = "Behandling"
	)

	expect_s3_class(plot, "ggplot")
	expect_true("Totalt" %in% as.character(plot$data$group))
	expect_equal(plot$labels$fill, "Behandling")

	prop_sum <- tapply(plot$data$prop, plot$data$group, sum)
	expect_true(all(abs(as.numeric(prop_sum) - 1) < 1e-8))
})

test_that("plotGruppertFordeling supports total-only mode and recoding", {
	data <- data.frame(
		treat = c(0, 1, 1, 0),
		stringsAsFactors = FALSE
	)

	plot <- plotGruppertFordeling(
		data = data,
		fordelingsVariabel = NULL,
		binVariabel = "treat",
		binNavn = c("0" = "Sukker", "1" = "Lakris"),
		totalLabel = "Totalt"
	)

	expect_s3_class(plot, "ggplot")
	expect_equal(unique(as.character(plot$data$group)), "Totalt")
	expect_setequal(as.character(plot$data$bin), c("Sukker", "Lakris"))
})

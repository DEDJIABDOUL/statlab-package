# Helpers ---------------------------------------------------------------------

.projet_plots <- function() {
  repertoire <- tempfile("statlab_plots_")
  dir.create(repertoire)
  repertoire
}

.fake_config_plots <- function(dictionnaire, declinaisons = c("ecran", "impression")) {
  structure(
    list(dictionnaire = dictionnaire, rendu = list(declinaisons = declinaisons)),
    class = "statlab_config", valid = TRUE
  )
}

.dictionnaire_plots <- function() {
  list(
    age = list(nature = "continue", libelle = "Age", unite = "annees"),
    imc = list(nature = "continue", libelle = "IMC", unite = "kg/m2"),
    groupe = list(nature = "nominale", libelle = "Groupe", modalites = c("Controle", "Intervention")),
    sexe = list(nature = "binaire", libelle = "Sexe", modalites = c("homme", "femme"))
  )
}

.layer_geoms <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[1], character(1))
}

# Tests : st_plot_box -----------------------------------------------------------

test_that("st_plot_box construit une boite a moustaches avec libelles du dictionnaire", {
  set.seed(1)
  data <- data.frame(age = stats::rnorm(150, 50, 10), groupe = sample(c("Controle", "Intervention"), 150, TRUE))
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_box(data, "age", "groupe", config)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Age (annees)")
  expect_true("GeomBoxplot" %in% .layer_geoms(p))
})

test_that("st_plot_box superpose les points individuels si n < 100", {
  set.seed(2)
  data_petit <- data.frame(age = stats::rnorm(40, 50, 10), groupe = sample(c("Controle", "Intervention"), 40, TRUE))
  data_grand <- data.frame(age = stats::rnorm(150, 50, 10), groupe = sample(c("Controle", "Intervention"), 150, TRUE))
  config <- .fake_config_plots(.dictionnaire_plots())

  p_petit <- st_plot_box(data_petit, "age", "groupe", config)
  p_grand <- st_plot_box(data_grand, "age", "groupe", config)

  expect_true("GeomPoint" %in% .layer_geoms(p_petit))
  expect_false("GeomPoint" %in% .layer_geoms(p_grand))
})

test_that("st_plot_box exclut les valeurs manquantes et le signale en note", {
  data <- data.frame(age = c(stats::rnorm(60, 50, 10), NA, NA), groupe = c(sample(c("Controle", "Intervention"), 60, TRUE), "Controle", NA))
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_box(data, "age", "groupe", config)
  expect_match(p$labels$caption, "exclue")
})

test_that("st_plot_box ajoute une annotation de significativite pour deux groupes", {
  set.seed(3)
  data <- data.frame(age = stats::rnorm(80, 50, 10), groupe = sample(c("Controle", "Intervention"), 80, TRUE))
  config <- .fake_config_plots(.dictionnaire_plots())
  test_result <- list(p_value = 0.012, test_name = "t de Student")

  p <- st_plot_box(data, "age", "groupe", config, test_result = test_result)
  expect_true(any(grepl("signif", .layer_geoms(p), ignore.case = TRUE)))
})

test_that("st_plot_box place la p-value en note pour plus de deux groupes", {
  set.seed(4)
  data <- data.frame(
    imc = stats::rnorm(90, 25, 4),
    groupe3 = sample(c("A", "B", "C"), 90, TRUE)
  )
  dictionnaire <- .dictionnaire_plots()
  dictionnaire$groupe3 <- list(nature = "nominale", libelle = "Groupe", modalites = c("A", "B", "C"))
  config <- .fake_config_plots(dictionnaire)
  test_result <- list(p_value = 0.0004, test_name = "Kruskal-Wallis")

  p <- st_plot_box(data, "imc", "groupe3", config, test_result = test_result)
  expect_match(p$labels$caption, "p < 0,001")
  expect_match(p$labels$caption, "Kruskal-Wallis")
})

test_that("st_plot_box s'arrete si la variable est introuvable", {
  data <- data.frame(age = 1:10, groupe = rep(c("A", "B"), 5))
  config <- .fake_config_plots(.dictionnaire_plots())
  expect_error(st_plot_box(data, "inexistante", "groupe", config), "introuvable")
})

test_that("st_plot_box s'arrete si aucune observation n'est complete", {
  data <- data.frame(age = c(NA, NA), groupe = c("Controle", "Intervention"))
  config <- .fake_config_plots(.dictionnaire_plots())
  expect_error(st_plot_box(data, "age", "groupe", config), "[Aa]ucune observation")
})

# Tests : st_plot_bar -----------------------------------------------------------

test_that("st_plot_bar construit un diagramme en barres sans groupe, avec pourcentage et effectif", {
  set.seed(5)
  data <- data.frame(sexe = sample(c("homme", "femme"), 100, TRUE))
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_bar(data, "sexe", config = config)
  expect_s3_class(p, "ggplot")
  expect_true("GeomCol" %in% .layer_geoms(p))
  expect_true("GeomErrorbar" %in% .layer_geoms(p))
  expect_match(p$labels$caption, "Denominateur")
})

test_that("st_plot_bar construit un diagramme en barres groupe (dodge)", {
  set.seed(6)
  data <- data.frame(
    sexe = sample(c("homme", "femme"), 120, TRUE),
    groupe = sample(c("Controle", "Intervention"), 120, TRUE)
  )
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_bar(data, "sexe", "groupe", config = config)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption, "Controle")
  expect_match(p$labels$caption, "Intervention")
})

test_that("st_plot_bar signale les valeurs manquantes exclues", {
  data <- data.frame(sexe = c(sample(c("homme", "femme"), 50, TRUE), NA, NA))
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_bar(data, "sexe", config = config)
  expect_match(p$labels$caption, "exclue")
})

test_that("st_plot_bar ajoute la p-value en note quand test_result est fourni (groupe)", {
  set.seed(7)
  data <- data.frame(
    sexe = sample(c("homme", "femme"), 100, TRUE),
    groupe = sample(c("Controle", "Intervention"), 100, TRUE)
  )
  config <- .fake_config_plots(.dictionnaire_plots())
  test_result <- list(p_value = 0.03, test_name = "Chi2")

  p <- st_plot_bar(data, "sexe", "groupe", config = config, test_result = test_result)
  expect_match(p$labels$caption, "0,030")
  expect_match(p$labels$caption, "Chi2")
})

test_that("st_plot_bar s'arrete si la variable de groupe est introuvable", {
  data <- data.frame(sexe = c("homme", "femme"))
  config <- .fake_config_plots(.dictionnaire_plots())
  expect_error(st_plot_bar(data, "sexe", "inexistant", config = config), "introuvable")
})

# Tests : st_plot_scatter ---------------------------------------------------------

test_that("st_plot_scatter trace un nuage de points avec droite de regression et annotation", {
  set.seed(8)
  data <- data.frame(age = stats::rnorm(50, 50, 10))
  data$imc <- 20 + 0.1 * data$age + stats::rnorm(50, 0, 2)
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_scatter(data, "age", "imc", config)
  expect_s3_class(p, "ggplot")
  expect_true("GeomSmooth" %in% .layer_geoms(p))
  expect_match(p$labels$subtitle, "^r = ")
  expect_match(p$labels$subtitle, "p")
})

test_that("st_plot_scatter omet la droite de regression si smooth = FALSE", {
  set.seed(9)
  data <- data.frame(age = stats::rnorm(50, 50, 10), imc = stats::rnorm(50, 25, 4))
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_scatter(data, "age", "imc", config, smooth = FALSE)
  expect_false("GeomSmooth" %in% .layer_geoms(p))
  expect_null(p$labels$subtitle)
})

test_that("st_plot_scatter colore par groupe quand fourni", {
  set.seed(10)
  data <- data.frame(
    age = stats::rnorm(60, 50, 10), imc = stats::rnorm(60, 25, 4),
    groupe = sample(c("Controle", "Intervention"), 60, TRUE)
  )
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_scatter(data, "age", "imc", config, group = "groupe")
  expect_true("colour" %in% names(p$mapping) || "colour" %in% names(p$layers[[1]]$mapping))
})

test_that("st_plot_scatter signale les valeurs manquantes exclues", {
  data <- data.frame(age = c(stats::rnorm(30, 50, 10), NA), imc = c(stats::rnorm(30, 25, 4), 22))
  config <- .fake_config_plots(.dictionnaire_plots())

  p <- st_plot_scatter(data, "age", "imc", config)
  expect_match(p$labels$caption, "exclue")
})

test_that("st_plot_scatter s'arrete si moins de deux observations completes", {
  data <- data.frame(age = c(50, NA, NA), imc = c(25, NA, 22))
  config <- .fake_config_plots(.dictionnaire_plots())
  expect_error(st_plot_scatter(data, "age", "imc", config), "[Mm]oins de deux")
})

test_that("st_plot_scatter s'arrete si la variable x est introuvable", {
  data <- data.frame(imc = 1:10)
  config <- .fake_config_plots(.dictionnaire_plots())
  expect_error(st_plot_scatter(data, "inexistante", "imc", config), "introuvable")
})

# Tests : st_save_plot -----------------------------------------------------------

test_that("st_save_plot produit un fichier par combinaison format x declinaison", {
  repertoire <- .projet_plots()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) + ggplot2::geom_boxplot()
  config <- .fake_config_plots(list(), declinaisons = c("ecran", "impression"))

  fichiers <- st_save_plot(
    p, file.path(repertoire, "fig1"), config,
    formats = c("pdf", "png", "svg"), width = 5, height = 4, dpi = c(100)
  )

  expect_length(fichiers, 6)
  expect_true(all(file.exists(fichiers)))
  expect_true(file.path(repertoire, "fig1_ecran.pdf") %in% fichiers)
  expect_true(file.path(repertoire, "fig1_ecran.svg") %in% fichiers)
  expect_true(file.path(repertoire, "fig1_ecran_100.png") %in% fichiers)
  expect_true(file.path(repertoire, "fig1_impression_100.png") %in% fichiers)
})

test_that("st_save_plot produit un fichier png par resolution demandee", {
  repertoire <- .projet_plots()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) + ggplot2::geom_boxplot()
  config <- .fake_config_plots(list(), declinaisons = "ecran")

  fichiers <- st_save_plot(p, file.path(repertoire, "fig2"), config, formats = "png", width = 5, height = 4, dpi = c(72, 150))

  expect_length(fichiers, 2)
  expect_true(file.path(repertoire, "fig2_ecran_72.png") %in% fichiers)
  expect_true(file.path(repertoire, "fig2_ecran_150.png") %in% fichiers)
})

test_that("st_save_plot s'arrete si aucune declinaison n'est disponible ni fournie", {
  repertoire <- .projet_plots()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) + ggplot2::geom_boxplot()
  config <- structure(list(dictionnaire = list(), rendu = list()), class = "statlab_config", valid = TRUE)

  expect_error(
    st_save_plot(p, file.path(repertoire, "fig3"), config, formats = "png", width = 5, height = 4),
    "declinaison"
  )
})

test_that("st_save_plot s'arrete sur une declinaison invalide", {
  repertoire <- .projet_plots()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) + ggplot2::geom_boxplot()
  config <- .fake_config_plots(list())

  expect_error(
    st_save_plot(p, file.path(repertoire, "fig4"), config, formats = "png", variants = "projecteur", width = 5, height = 4),
    "invalide"
  )
})

test_that("st_save_plot s'arrete si l'objet n'est pas un ggplot", {
  config <- .fake_config_plots(list())
  expect_error(st_save_plot(list(), "fig5", config, width = 5, height = 4))
})

# Tests : formatage des p-values --------------------------------------------------

test_that(".format_p_value formate selon les conventions francaises et n'affiche jamais 0,000", {
  expect_equal(statlab:::.format_p_value(0.012), "p = 0,012")
  expect_equal(statlab:::.format_p_value(0.00099), "p < 0,001")
  expect_equal(statlab:::.format_p_value(0.0004), "p < 0,001")
  expect_equal(statlab:::.format_p_value(0.001), "p = 0,001")
  expect_equal(statlab:::.format_p_value(0.45), "p = 0,450")
  expect_false(grepl("0,000", statlab:::.format_p_value(0.0000001)))
  expect_true(is.na(statlab:::.format_p_value(NA_real_)))
})

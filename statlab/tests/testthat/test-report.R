# Helpers -----------------------------------------------------------------

.creer_projet_rapport <- function() {
  repertoire <- tempfile("statlab_rapport_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  repertoire
}

.ecrire_source_rapport <- function(repertoire, nom = "inclusion.csv") {
  writeLines(c(
    "id;age;groupe;sexe",
    "P1;45;A;homme",
    "P2;52;B;femme",
    "P3;38;A;femme",
    "P4;60;B;homme",
    "P5;41;A;homme",
    "P6;55;B;femme",
    "P7;33;A;femme",
    "P8;48;B;homme",
    "P9;39;A;femme",
    "P10;58;B;homme"
  ), file.path(repertoire, "donnees_brutes", nom))
}

.config_rapport_simple <- function(repertoire, sections_extra = character(0)) {
  .ecrire_source_rapport(repertoire)
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test rapport",
    "  client: CH Exemple",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "dictionnaire:",
    "  age:",
    "    nature: continue",
    "    libelle: Age",
    "  groupe:",
    "    nature: nominale",
    "    libelle: Groupe",
    '    modalites: ["A", "B"]',
    "  sexe:",
    "    nature: binaire",
    "    libelle: Sexe",
    "preparation:",
    "  exclusions:",
    '    - condition: "as.numeric(age) < 10"',
    '      motif: "Age improbable"',
    "  manquants:",
    "    - strategie: conserver",
    "analyse:",
    "  tableau_1:",
    "    stratification: groupe",
    '    variables: ["age", "sexe"]',
    "  comparaisons:",
    '    - variables: ["age"]',
    "      groupe: groupe",
    "      apparie: false",
    sections_extra
  ), chemin_config)
  chemin_config
}

.quarto_disponible <- function() nzchar(Sys.which("quarto"))
.latex_disponible <- function() nzchar(Sys.which("pdflatex")) || nzchar(Sys.getenv("TINYTEX_ROOT"))

# Tests : logique pure (sans rendu Quarto) -----------------------------------

test_that(".rp_parse_exclusion_details extrait motif/effectifs d'une ligne de journal", {
  parsed <- statlab:::.rp_parse_exclusion_details("n_avant=42 n_apres=40 n_exclues=2 motif=Age improbable, hors bornes")
  expect_equal(parsed$n_avant, 42L)
  expect_equal(parsed$n_apres, 40L)
  expect_equal(parsed$n_exclues, 2L)
  expect_equal(parsed$motif, "Age improbable, hors bornes")
})

test_that(".rp_parse_exclusion_details s'arrete sur une ligne mal formee", {
  expect_error(statlab:::.rp_parse_exclusion_details("ceci n'est pas une ligne valide"))
})

test_that(".rp_extract_exclusions ne retient que les nouveaux evenements d'exclusion", {
  repertoire <- .creer_projet_rapport()
  st_log_init(repertoire)
  st_log("evenement_non_lie", module = "x", level = "info")
  avant <- statlab:::.rp_read_journal_safe(repertoire)

  st_log_exclusion(20, 18, "Premier motif")
  st_log("evenement_intercale", module = "x", level = "info")
  st_log_exclusion(18, 15, "Second motif")
  apres <- statlab:::.rp_read_journal_safe(repertoire)

  resultat <- statlab:::.rp_extract_exclusions(avant, apres)

  expect_equal(nrow(resultat), 2)
  expect_equal(resultat$motif, c("Premier motif", "Second motif"))
  expect_equal(resultat$n_exclues, c(2L, 3L))
})

test_that(".rp_extract_exclusions retourne un data.frame vide sans nouvelle exclusion", {
  repertoire <- .creer_projet_rapport()
  st_log_init(repertoire)
  avant <- statlab:::.rp_read_journal_safe(repertoire)
  st_log("autre_evenement", module = "x", level = "info")
  apres <- statlab:::.rp_read_journal_safe(repertoire)

  resultat <- statlab:::.rp_extract_exclusions(avant, apres)
  expect_equal(nrow(resultat), 0)
})

test_that(".rp_reading_sentence signale une difference significative", {
  resultat <- list(p_value = 0.01, test_name = "Test t de Student")
  phrase <- statlab:::.rp_reading_sentence(resultat, "Age", "Groupe")
  expect_match(phrase, "significative")
  expect_match(phrase, "p = 0,010")
})

test_that(".rp_reading_sentence signale l'absence de difference significative", {
  resultat <- list(p_value = 0.8, test_name = "Test t de Student")
  phrase <- statlab:::.rp_reading_sentence(resultat, "Age", "Groupe")
  expect_match(phrase, "Aucune difference")
})

test_that(".rp_build_comparisons construit un element par variable comparee, avec graphique et phrase de lecture", {
  repertoire <- .creer_projet_rapport()
  chemin <- .config_rapport_simple(repertoire)
  config <- st_validate_config(st_read_config(chemin))
  st_log_init(repertoire)

  donnees <- st_prepare(st_read_all_sources(config)[[1]], config)
  resultats <- statlab:::.rp_build_comparisons(donnees, config)

  expect_length(resultats, 1)
  expect_equal(resultats[[1]]$variable, "age")
  expect_equal(resultats[[1]]$label_variable, "Age")
  expect_s3_class(resultats[[1]]$plot, "ggplot")
  expect_true(nzchar(resultats[[1]]$reading_sentence))
})

# Tests : erreurs -------------------------------------------------------------

test_that("st_report s'arrete si plusieurs sources sont declarees sans reconciliation", {
  repertoire <- .creer_projet_rapport()
  .ecrire_source_rapport(repertoire)
  .ecrire_source_rapport(repertoire, "suivi.csv")
  chemin <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test rapport",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "  - id: suivi",
    "    fichier: donnees_brutes/suivi.csv"
  ), chemin)

  expect_error(st_report(chemin), "reconciliation")
})

test_that("st_report s'arrete si le fichier config n'existe pas", {
  expect_error(st_report(tempfile(fileext = ".yml")))
})

# Test d'integration : rendu Quarto complet (docx + pdf) ---------------------

test_that("st_report genere le rapport docx et pdf, avec les annexes A et B", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")
  testthat::skip_if_not(.latex_disponible(), "distribution LaTeX non disponible")

  repertoire <- .creer_projet_rapport()
  chemin <- .config_rapport_simple(repertoire)

  resultat <- st_report(chemin, output_dir = "sorties/rapport", formats = c("docx", "pdf"))

  expect_length(resultat, 2)
  expect_true(all(file.exists(resultat)))
  expect_true(file.exists(file.path(repertoire, "sorties/rapport/analyse.R")))
  expect_true(file.exists(file.path(repertoire, "sorties/rapport/attestation.txt")))
})

test_that("st_report journalise sa propre execution", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")
  testthat::skip_if_not(.latex_disponible(), "distribution LaTeX non disponible")

  repertoire <- .creer_projet_rapport()
  chemin <- .config_rapport_simple(repertoire)

  st_report(chemin, formats = "pdf")
  journal <- st_log_read(repertoire)

  expect_true("rapport" %in% journal$evenement)
})

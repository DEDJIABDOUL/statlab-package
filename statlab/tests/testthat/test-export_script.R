# Helpers -----------------------------------------------------------------

.creer_projet_export <- function() {
  repertoire <- tempfile("statlab_export_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  repertoire
}

.ecrire_source_export <- function(repertoire, nom = "inclusion.csv") {
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

.config_export_simple <- function(repertoire, sections_extra = character(0)) {
  .ecrire_source_export(repertoire)
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test export",
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
    "  manquants:",
    "    - strategie: conserver",
    "analyse:",
    "  comparaisons:",
    '    - variables: ["age"]',
    "      groupe: groupe",
    "      apparie: false",
    sections_extra
  ), chemin_config)
  chemin_config
}

.config_valide_export <- function(repertoire, sections_extra = character(0)) {
  chemin <- .config_export_simple(repertoire, sections_extra)
  st_validate_config(st_read_config(chemin))
}

.rscript_bin <- function() {
  file.path(R.home("bin"), if (identical(.Platform$OS.type, "windows")) "Rscript.exe" else "Rscript")
}

# Tests : generation de base ------------------------------------------------

test_that("st_export_script genere un fichier au chemin par defaut", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)

  expect_true(file.exists(resultat))
  expect_equal(resultat, normalizePath(file.path(repertoire, "sorties/rapport/analyse.R"), winslash = "/"))
})

test_that("st_export_script accepte un chemin de sortie personnalise", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config, output = "annexe/script.R")

  expect_true(file.exists(resultat))
  expect_equal(resultat, normalizePath(file.path(repertoire, "annexe/script.R"), winslash = "/"))
})

test_that("st_export_script s'arrete si la configuration n'est pas validee", {
  repertoire <- .creer_projet_export()
  chemin <- .config_export_simple(repertoire)
  config_non_validee <- st_read_config(chemin)

  expect_error(st_export_script(config_non_validee), "valid")
})

# Tests : contenu de l'en-tete -----------------------------------------------

test_that("st_export_script inclut le nom du projet, la version de R et du referentiel en en-tete", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  rules <- st_load_rules()
  expect_match(contenu, "Etude test export", fixed = TRUE)
  expect_match(contenu, R.version.string, fixed = TRUE)
  expect_match(contenu, sprintf("version %s", attr(rules, "version")), fixed = TRUE)
})

test_that("st_export_script inclut l'empreinte SHA-256 exacte du fichier source", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)
  contenu <- readLines(resultat)

  empreinte <- digest::digest(file = file.path(repertoire, "donnees_brutes", "inclusion.csv"), algo = "sha256")
  expect_true(any(grepl(empreinte, contenu, fixed = TRUE)))
})

test_that("st_export_script inclut l'empreinte SHA-256 du fichier config.yml", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)
  contenu <- readLines(resultat)

  empreinte <- digest::digest(file = attr(config, "path"), algo = "sha256")
  expect_true(any(grepl(empreinte, contenu, fixed = TRUE)))
})

# Tests : autonomie (aucune dependance a statlab) ---------------------------

test_that("le script genere ne reference jamais le package statlab", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)
  contenu <- readLines(resultat)
  lignes_code <- contenu[!grepl("^\\s*#", contenu)]

  expect_false(any(grepl("statlab", lignes_code, fixed = TRUE)))
  expect_false(any(grepl("library\\(statlab\\)", contenu)))
})

test_that("le script genere ne charge que des packages publics du CRAN declares", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)
  contenu <- readLines(resultat)

  appels_library <- grep("^library\\(", contenu, value = TRUE)
  expect_true(length(appels_library) > 0)
  paquets <- gsub("^library\\(([A-Za-z0-9._]+)\\)$", "\\1", appels_library)
  expect_true(all(vapply(paquets, requireNamespace, logical(1), quietly = TRUE)))
})

# Tests : comparaisons et justifications -------------------------------------

test_that("le script genere inclut le test retenu et sa justification methodologique", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "Test retenu :")
  expect_match(contenu, "Justification :")
  expect_match(contenu, "## Comparaison : age selon groupe")
})

test_that("la p-value figee en commentaire correspond exactement a celle de st_compare()", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat_direct <- st_compare(
    st_prepare(st_read_all_sources(config)[[1]], config), "age", "groupe", config
  )

  resultat <- st_export_script(config)
  contenu <- readLines(resultat)
  ligne_p <- grep("p-value obtenue a la generation", contenu, value = TRUE)
  p_extraite <- as.numeric(sub(".*: ", "", ligne_p))

  expect_equal(p_extraite, resultat_direct$p_value, tolerance = 1e-5)
})

# Tests : erreurs -------------------------------------------------------------

test_that("st_export_script s'arrete si plusieurs sources sont declarees sans reconciliation", {
  repertoire <- .creer_projet_export()
  .ecrire_source_export(repertoire)
  .ecrire_source_export(repertoire, "suivi.csv")
  chemin <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test export",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "  - id: suivi",
    "    fichier: donnees_brutes/suivi.csv"
  ), chemin)
  config <- st_validate_config(st_read_config(chemin))

  expect_error(st_export_script(config), "reconciliation")
})

test_that("st_export_script journalise sa propre execution", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  st_export_script(config)
  journal <- st_log_read(repertoire)

  expect_true("export_script" %in% journal$evenement)
})

# Test d'integration : le script genere s'execute seul, sans le package statlab --

test_that("le script genere s'execute de bout en bout dans un processus R independant", {
  repertoire <- .creer_projet_export()
  config <- .config_valide_export(repertoire)

  resultat <- st_export_script(config)

  ancien_wd <- getwd()
  setwd(repertoire)
  on.exit(setwd(ancien_wd), add = TRUE)

  sortie <- suppressWarnings(system2(
    .rscript_bin(), args = c("--vanilla", shQuote(resultat)),
    stdout = TRUE, stderr = TRUE
  ))
  statut <- attr(sortie, "status")

  expect_true(is.null(statut) || identical(statut, 0L))
  expect_true(any(grepl("Two Sample t-test|Welch", sortie)) || any(grepl("t\\.test", sortie)))
})

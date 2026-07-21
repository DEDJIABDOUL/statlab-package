# Helpers -----------------------------------------------------------------

.creer_projet_attestation <- function() {
  repertoire <- tempfile("statlab_attestation_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  repertoire
}

.ecrire_config_attestation <- function(repertoire, nom_projet = "Etude test", client = NULL, lignes_source = c("id,age", "P1,45", "P2,52")) {
  writeLines(lignes_source, file.path(repertoire, "donnees_brutes", "inclusion.csv"))
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    sprintf("  nom: %s", nom_projet),
    if (!is.null(client)) sprintf("  client: %s", client),
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv"
  ), chemin_config)
  chemin_config
}

# Tests -----------------------------------------------------------------

test_that("st_attestation genere un fichier a la racine du projet avec les informations du projet", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire, "Etude ACTIF", client = "CH Exemple")

  resultat <- st_attestation(chemin_config)

  expect_true(file.exists(resultat))
  expect_equal(dirname(resultat), normalizePath(repertoire, winslash = "/"))

  contenu <- paste(readLines(resultat), collapse = "\n")
  expect_match(contenu, "ATTESTATION DE REPRODUCTIBILITE")
  expect_match(contenu, "Etude ACTIF")
  expect_match(contenu, "CH Exemple")
})

test_that("st_attestation calcule l'empreinte exacte du config.yml", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  resultat <- st_attestation(chemin_config)
  contenu <- readLines(resultat)

  empreinte_attendue <- digest::digest(file = chemin_config, algo = "sha256")
  expect_true(any(grepl(empreinte_attendue, contenu, fixed = TRUE)))
})

test_that("st_attestation calcule l'empreinte exacte de chaque source", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  resultat <- st_attestation(chemin_config)
  contenu <- readLines(resultat)

  empreinte_source <- digest::digest(file = file.path(repertoire, "donnees_brutes", "inclusion.csv"), algo = "sha256")
  expect_true(any(grepl(empreinte_source, contenu, fixed = TRUE)))
  expect_true(any(grepl("inclusion", contenu, fixed = TRUE)))
})

test_that("st_attestation reporte la version du referentiel methodologique", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  resultat <- st_attestation(chemin_config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  rules <- st_load_rules()
  expect_match(contenu, sprintf("version %s", attr(rules, "version")), fixed = TRUE)
})

test_that("st_attestation signale l'absence de derogation pour un projet neuf", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  resultat <- st_attestation(chemin_config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "Aucune derogation enregistree")
})

test_that("st_attestation liste les derogations enregistrees dans le journal", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  st_log_init(repertoire)
  st_log("derogation_regle", module = "rules", regle = "GROUPE-001", level = "derogation")

  resultat <- st_attestation(chemin_config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "GROUPE-001")
  expect_false(grepl("Aucune derogation enregistree", contenu))
})

test_that("st_attestation resume les evenements du journal", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  st_log_init(repertoire)
  st_log_exclusion(10, 8, "Motif de test")
  st_log_exclusion(8, 7, "Autre motif")

  resultat <- st_attestation(chemin_config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "exclusion_observations : 2")
})

test_that("st_attestation accepte un chemin de sortie personnalise", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)
  sortie <- file.path(repertoire, "certificat.txt")

  resultat <- st_attestation(chemin_config, output = "certificat.txt")

  expect_equal(resultat, normalizePath(sortie, winslash = "/"))
  expect_true(file.exists(sortie))
})

test_that("st_attestation liste les versions des packages utilises", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  resultat <- st_attestation(chemin_config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, sprintf("digest : %s", as.character(utils::packageVersion("digest"))), fixed = TRUE)
})

test_that("st_attestation journalise sa propre execution", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)

  st_attestation(chemin_config)
  journal <- st_log_read(repertoire)

  expect_true("attestation" %in% journal$evenement)
})

test_that("st_attestation s'arrete si un fichier source declare a disparu", {
  repertoire <- .creer_projet_attestation()
  chemin_config <- .ecrire_config_attestation(repertoire)
  file.remove(file.path(repertoire, "donnees_brutes", "inclusion.csv"))

  expect_error(st_attestation(chemin_config), "introuvable")
})

test_that("st_attestation s'arrete si le fichier config n'existe pas", {
  expect_error(st_attestation(tempfile(fileext = ".yml")))
})

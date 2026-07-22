# Helpers -----------------------------------------------------------------

.creer_projet_attestation <- function() {
  repertoire <- tempfile("statlab_attestation_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  repertoire
}

.ecrire_source_attestation <- function(repertoire, nom = "inclusion.csv", lignes = c("id,age", "P1,45", "P2,52", "P3,38")) {
  writeLines(lignes, file.path(repertoire, "donnees_brutes", nom))
}

.config_valide_attestation <- function(repertoire, nom_projet = "Etude test", client = NULL, sections_extra = character(0)) {
  .ecrire_source_attestation(repertoire)
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    sprintf("  nom: %s", nom_projet),
    if (!is.null(client)) sprintf("  client: %s", client),
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "preparation:",
    "  manquants:",
    "    - strategie: conserver",
    sections_extra
  ), chemin_config)
  st_validate_config(st_read_config(chemin_config))
}

# Tests -----------------------------------------------------------------

test_that("st_attestation genere un fichier a la racine du projet avec les informations du projet", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire, "Etude ACTIF", client = "CH Exemple")

  resultat <- st_attestation(config)

  expect_true(file.exists(resultat))
  expect_equal(dirname(resultat), normalizePath(repertoire, winslash = "/"))

  contenu <- paste(readLines(resultat), collapse = "\n")
  expect_match(contenu, "ATTESTATION DE REPRODUCTIBILITE")
  expect_match(contenu, "Etude ACTIF")
  expect_match(contenu, "CH Exemple")
})

test_that("st_attestation s'arrete si la configuration n'est pas validee", {
  repertoire <- .creer_projet_attestation()
  .ecrire_source_attestation(repertoire)
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:", "  nom: Etude test", "sources:", "  - id: inclusion", "    fichier: donnees_brutes/inclusion.csv"
  ), chemin_config)
  config_non_validee <- st_read_config(chemin_config)

  expect_error(st_attestation(config_non_validee), "valid")
})

test_that("st_attestation reporte la version de R et le systeme d'exploitation", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, R.version.string, fixed = TRUE)
  expect_match(contenu, sessioninfo::platform_info()$os, fixed = TRUE)
})

test_that("st_attestation calcule l'empreinte, la taille et la date de modification exactes de chaque source", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  resultat <- st_attestation(config)
  contenu <- readLines(resultat)

  chemin_source <- file.path(repertoire, "donnees_brutes", "inclusion.csv")
  empreinte_source <- digest::digest(file = chemin_source, algo = "sha256")
  taille <- file.info(chemin_source)$size

  expect_true(any(grepl(empreinte_source, contenu, fixed = TRUE)))
  expect_true(any(grepl("inclusion.csv", contenu, fixed = TRUE)))
  expect_true(any(grepl(format(taille, big.mark = " ", scientific = FALSE), contenu, fixed = TRUE)))
})

test_that("st_attestation reporte la version du referentiel methodologique et de statlab", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  rules <- st_load_rules()
  expect_match(contenu, sprintf("version %s", attr(rules, "version")), fixed = TRUE)
  expect_match(contenu, "Package statlab")
})

test_that("st_attestation liste les versions exactes des packages utilises (via sessioninfo)", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, sprintf("digest : %s", as.character(utils::packageVersion("digest"))), fixed = TRUE)
  expect_match(contenu, sprintf("cli : %s", as.character(utils::packageVersion("cli"))), fixed = TRUE)
})

test_that("st_attestation reporte les effectifs lus, l'absence de reconciliation et l'effectif final", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "inclusion : 3")
  expect_match(contenu, "sans objet - source unique")
  expect_match(contenu, "Effectif final analyse : 3")
})

test_that("st_attestation reporte les exclusions avec leur motif et leurs effectifs avant/apres", {
  repertoire <- .creer_projet_attestation()
  .ecrire_source_attestation(repertoire)
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "preparation:",
    "  exclusions:",
    '    - condition: "as.numeric(age) < 40"',
    '      motif: "Age improbable"',
    "  manquants:",
    "    - strategie: conserver"
  ), chemin_config)
  config <- st_validate_config(st_read_config(chemin_config))

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "Age improbable : 3 -> 2 \\(1 exclue\\(s\\)\\)")
  expect_match(contenu, "Effectif final analyse : 2")
})

test_that("st_attestation signale l'absence d'exclusion et de derogation pour un projet neuf", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "Aucune exclusion appliquee")
  expect_match(contenu, "Aucune derogation enregistree")
})

test_that("st_attestation liste les derogations enregistrees dans le journal", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  st_log_init(repertoire)
  st_log("derogation_regle", module = "rules", regle = "GROUPE-001", level = "derogation")

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "GROUPE-001")
  expect_false(grepl("Aucune derogation enregistree", contenu))
})

test_that("st_attestation accepte un chemin de sortie personnalise", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)
  sortie <- file.path(repertoire, "certificat.txt")

  resultat <- st_attestation(config, output = "certificat.txt")

  expect_equal(resultat, normalizePath(sortie, winslash = "/"))
  expect_true(file.exists(sortie))
})

test_that("st_attestation journalise sa propre execution", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)

  st_attestation(config)
  journal <- st_log_read(repertoire)

  expect_true("attestation" %in% journal$evenement)
})

test_that("st_attestation s'arrete si un fichier source declare a disparu", {
  repertoire <- .creer_projet_attestation()
  config <- .config_valide_attestation(repertoire)
  file.remove(file.path(repertoire, "donnees_brutes", "inclusion.csv"))

  expect_error(st_attestation(config), "introuvable")
})

test_that("st_attestation s'arrete si plusieurs sources sont declarees sans reconciliation", {
  repertoire <- .creer_projet_attestation()
  .ecrire_source_attestation(repertoire, "inclusion.csv")
  .ecrire_source_attestation(repertoire, "suivi.csv")
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "  - id: suivi",
    "    fichier: donnees_brutes/suivi.csv"
  ), chemin_config)
  config <- st_validate_config(st_read_config(chemin_config))

  expect_error(st_attestation(config), "reconciliation")
})

test_that("st_attestation reporte l'effectif apres reconciliation lorsqu'elle est declaree", {
  repertoire <- .creer_projet_attestation()
  .ecrire_source_attestation(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52", "P3,38"))
  .ecrire_source_attestation(repertoire, "suivi.csv", c("id,visite", "P1,J8", "P2,J8", "P3,J8"))
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "  - id: suivi",
    "    fichier: donnees_brutes/suivi.csv",
    "reconciliation:",
    "  - operation: joindre",
    "    gauche: inclusion",
    "    droite: suivi",
    "    cle: id",
    "    resultat: donnees_completes",
    "preparation:",
    "  manquants:",
    "    - strategie: conserver"
  ), chemin_config)
  config <- st_validate_config(st_read_config(chemin_config))

  resultat <- st_attestation(config)
  contenu <- paste(readLines(resultat), collapse = "\n")

  expect_match(contenu, "Lignes apres reconciliation : 3")
})

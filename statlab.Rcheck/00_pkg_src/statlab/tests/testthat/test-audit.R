# Helpers -----------------------------------------------------------------

.creer_projet_audit <- function() {
  repertoire <- tempfile("statlab_audit_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  st_log_init(repertoire)
  repertoire
}

.ecrire_config_audit <- function(repertoire, nom_source = "inclusion.csv", lignes_source, nom_projet = "Etude test") {
  writeLines(lignes_source, file.path(repertoire, "donnees_brutes", nom_source))
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    sprintf("  nom: %s", nom_projet),
    "sources:",
    "  - id: inclusion",
    sprintf("    fichier: donnees_brutes/%s", nom_source)
  ), chemin_config)
  chemin_config
}

.quarto_disponible <- function() nzchar(Sys.which("quarto"))

# Tests : .build_audit_payload (logique pure, sans rendu) --------------------

test_that(".build_audit_payload consolide les sources, anomalies et totaux", {
  repertoire <- .creer_projet_audit()
  chemin_config <- .ecrire_config_audit(repertoire, lignes_source = c(
    "id,age", "P1,45", "P2,52", "P3,38", "P1,45"
  ))
  config <- st_validate_config(st_read_config(chemin_config))
  sources_data <- st_read_all_sources(config)

  payload <- .build_audit_payload(config, sources_data)

  expect_equal(payload$project$name, "Etude test")
  expect_equal(payload$totals$n_sources, 1)
  expect_equal(payload$totals$n_rows, 4)
  expect_true(payload$totals$anomaly_counts$bloquant >= 1)
  expect_true("duplicate_rows" %in% payload$anomalies$check_id)
  expect_true(all(c("excerpt", "source") %in% names(payload$anomalies)))
})

test_that(".build_audit_payload retourne des totaux nuls sans anomalie", {
  repertoire <- .creer_projet_audit()
  chemin_config <- .ecrire_config_audit(repertoire, lignes_source = c(
    "id,age", "P1,45", "P2,52", "P3,38"
  ))
  config <- st_validate_config(st_read_config(chemin_config))
  sources_data <- st_read_all_sources(config)

  payload <- .build_audit_payload(config, sources_data)

  expect_equal(nrow(payload$anomalies), 0)
  expect_match(payload$recommendations, "Aucune anomalie", all = FALSE)
})

test_that(".build_anomaly_excerpt limite l'extrait a 10 lignes et inclut l'identifiant", {
  df <- data.frame(
    patient_id = paste0("P", 1:20),
    valeur = as.character(1:20),
    stringsAsFactors = FALSE
  )
  profil <- data.frame(name = c("patient_id", "valeur"), inferred_nature = c("identifiant", "continue"), stringsAsFactors = FALSE)
  anomaly_row <- data.frame(variable = "valeur", stringsAsFactors = FALSE)
  anomaly_row$rows_affected <- list(1:20)

  extrait <- .build_anomaly_excerpt(df, anomaly_row, profil)

  expect_equal(nrow(extrait), 10)
  expect_true("patient_id" %in% names(extrait))
  expect_true("valeur" %in% names(extrait))
  expect_true("ligne" %in% names(extrait))
})

test_that(".build_recommendations trie par severite et deduplique", {
  anomalies <- data.frame(
    check_id = c("outliers", "duplicate_rows", "outliers"),
    severity = c("avertissement", "bloquant", "avertissement"),
    variable = c("age", NA_character_, "age"),
    stringsAsFactors = FALSE
  )
  recommandations <- .build_recommendations(anomalies)

  expect_equal(length(recommandations), 2)
  expect_match(recommandations[1], "bloquant")
})

# Tests : st_audit -- validations d'arguments ---------------------------------

test_that("st_audit rejette un format inconnu", {
  repertoire <- .creer_projet_audit()
  chemin_config <- .ecrire_config_audit(repertoire, lignes_source = c("id,age", "P1,45", "P2,52"))

  expect_error(
    st_audit(chemin_config, output_dir = file.path(repertoire, "sorties"), formats = "docx"),
    "subset"
  )
})

test_that("st_audit s'arrete si la configuration est invalide", {
  repertoire <- .creer_projet_audit()
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c("projet:", "  nom: Etude test"), chemin_config)

  expect_error(st_audit(chemin_config, output_dir = file.path(repertoire, "sorties")), "sources")
})

# Tests : rendu Quarto reel (necessite le CLI quarto) -------------------------

test_that("st_audit genere un rapport HTML complet", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")

  repertoire <- .creer_projet_audit()
  chemin_config <- .ecrire_config_audit(repertoire, lignes_source = c(
    "id,age,groupe,commentaire",
    "P1,45,Intervention,Rien a signaler",
    "P2,52,Controle,999",
    "P3,38,Intervention,Ok",
    "P4,61,Controle,NR",
    "P5,29,Intervention,Ok"
  ))
  sortie <- file.path(repertoire, "sorties", "audit")

  resultat <- st_audit(chemin_config, output_dir = sortie, formats = "html")

  expect_true(file.exists(resultat))
  expect_match(resultat, "audit\\.html$")

  contenu <- readLines(resultat, warn = FALSE)
  texte <- paste(contenu, collapse = "\n")
  expect_match(texte, "Synthèse")
  expect_match(texte, "Anomalies détectées")
  expect_match(texte, "Recommandations")
})

test_that("st_audit cree le repertoire de sortie s'il n'existe pas", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")

  repertoire <- .creer_projet_audit()
  chemin_config <- .ecrire_config_audit(repertoire, lignes_source = c("id,age", "P1,45", "P2,52", "P3,38"))
  sortie <- file.path(repertoire, "sorties", "audit_inexistant")

  expect_false(dir.exists(sortie))
  st_audit(chemin_config, output_dir = sortie, formats = "html")
  expect_true(dir.exists(sortie))
})

test_that("st_audit journalise l'operation", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")

  repertoire <- .creer_projet_audit()
  chemin_config <- .ecrire_config_audit(repertoire, lignes_source = c("id,age", "P1,45", "P2,52", "P3,38"))
  sortie <- file.path(repertoire, "sorties", "audit")

  st_audit(chemin_config, output_dir = sortie, formats = "html")
  journal <- st_log_read(repertoire)

  expect_true("audit" %in% journal$evenement)
})

test_that("st_audit genere un rapport PDF", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")
  testthat::skip_if_not(nzchar(Sys.which("pdflatex")) || nzchar(Sys.getenv("TINYTEX_ROOT")), "distribution LaTeX non disponible")

  repertoire <- .creer_projet_audit()
  chemin_config <- .ecrire_config_audit(repertoire, lignes_source = c("id,age", "P1,45", "P2,52", "P3,38"))
  sortie <- file.path(repertoire, "sorties", "audit")

  resultat <- st_audit(chemin_config, output_dir = sortie, formats = "pdf")
  expect_true(file.exists(resultat))
  expect_match(resultat, "audit\\.pdf$")
})

# Helpers -----------------------------------------------------------------

.mini_profile <- function(names, natures) {
  data.frame(name = names, inferred_nature = natures, stringsAsFactors = FALSE)
}

.build_config_with_dictionary <- function(dictionary_yaml_lines) {
  repertoire <- tempfile("statlab_anomalies_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  writeLines("id,valeur", file.path(repertoire, "donnees_brutes", "inclusion.csv"))

  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude anomalies",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "dictionnaire:",
    dictionary_yaml_lines
  ), chemin_config)

  st_validate_config(st_read_config(chemin_config))
}

# Tests : check_missing_codes -----------------------------------------------

test_that("check_missing_codes signale les codages heterogenes", {
  df <- data.frame(x = c("1", "999", "2", "NR", "3"), stringsAsFactors = FALSE)
  resultat <- check_missing_codes(df)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$check_id, "missing_codes")
  expect_equal(resultat$n_affected, 2)
  expect_equal(resultat$rows_affected[[1]], c(2L, 4L))
})

test_that("check_missing_codes ne signale rien si un seul code est utilise", {
  df <- data.frame(x = c("1", "999", "2", "999", "3"), stringsAsFactors = FALSE)
  resultat <- check_missing_codes(df)
  expect_equal(nrow(resultat), 0)
})

# Tests : check_missing_rate -------------------------------------------------

test_that("check_missing_rate classe en avertissement entre 20 et 50 pourcent", {
  df <- data.frame(x = c("1", "2", NA, NA, NA, "4", "5", "6", "7", "8"), stringsAsFactors = FALSE)
  resultat <- check_missing_rate(df)
  expect_equal(resultat$severity, "avertissement")
  expect_equal(resultat$n_affected, 3)
})

test_that("check_missing_rate classe en bloquant au-dela de 50 pourcent", {
  df <- data.frame(x = c(NA, NA, NA, "1", "2"), stringsAsFactors = FALSE)
  resultat <- check_missing_rate(df)
  expect_equal(resultat$severity, "bloquant")
})

test_that("check_missing_rate ne signale rien sous 20 pourcent", {
  df <- data.frame(x = c(NA, rep("1", 10)), stringsAsFactors = FALSE)
  resultat <- check_missing_rate(df)
  expect_equal(nrow(resultat), 0)
})

# Tests : check_duplicate_rows -----------------------------------------------

test_that("check_duplicate_rows detecte des lignes strictement identiques", {
  df <- data.frame(
    a = c("1", "1", "2", "3"),
    b = c("x", "x", "y", "z"),
    stringsAsFactors = FALSE
  )
  resultat <- check_duplicate_rows(df)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$check_id, "duplicate_rows")
  expect_equal(resultat$severity, "bloquant")
  expect_true(is.na(resultat$variable))
  expect_equal(sort(resultat$rows_affected[[1]]), c(1L, 2L))
})

test_that("check_duplicate_rows ne signale rien en l'absence de doublons", {
  df <- data.frame(a = c("1", "2", "3"), stringsAsFactors = FALSE)
  expect_equal(nrow(check_duplicate_rows(df)), 0)
})

# Tests : check_duplicate_ids ------------------------------------------------

test_that("check_duplicate_ids signale les valeurs d'identifiant dupliquees", {
  df <- data.frame(patient_id = c("P1", "P2", "P1", "P3"), stringsAsFactors = FALSE)
  profil <- .mini_profile("patient_id", "identifiant")
  resultat <- check_duplicate_ids(df, profile = profil)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$variable, "patient_id")
  expect_equal(resultat$n_affected, 2)
})

test_that("check_duplicate_ids ne signale rien sans profil (nature inconnue)", {
  df <- data.frame(patient_id = c("P1", "P2", "P1", "P3"), stringsAsFactors = FALSE)
  resultat <- check_duplicate_ids(df, profile = NULL)
  expect_equal(nrow(resultat), 0)
})

# Tests : check_constant_columns ---------------------------------------------

test_that("check_constant_columns detecte une colonne strictement constante", {
  df <- data.frame(site = rep("CHU_Paris", 10), stringsAsFactors = FALSE)
  resultat <- check_constant_columns(df)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$severity, "information")
  expect_match(resultat$detail, "constante")
})

test_that("check_constant_columns detecte une colonne quasi-constante", {
  df <- data.frame(x = c(rep("A", 19), "B"), stringsAsFactors = FALSE)
  resultat <- check_constant_columns(df)
  expect_equal(nrow(resultat), 1)
  expect_match(resultat$detail, "quasi-constante")
  expect_equal(resultat$n_affected, 19)
})

test_that("check_constant_columns ne signale rien pour une colonne variee", {
  df <- data.frame(x = c("A", "B", "C", "D"), stringsAsFactors = FALSE)
  expect_equal(nrow(check_constant_columns(df)), 0)
})

# Tests : check_empty_columns -------------------------------------------------

test_that("check_empty_columns detecte une colonne entierement vide", {
  df <- data.frame(x = rep(NA_character_, 5), stringsAsFactors = FALSE)
  resultat <- check_empty_columns(df)
  expect_equal(nrow(resultat), 1)
  expect_match(resultat$detail, "entierement vide")
})

test_that("check_empty_columns detecte une colonne presque vide", {
  df <- data.frame(x = c(rep(NA_character_, 96), rep("1", 4)), stringsAsFactors = FALSE)
  resultat <- check_empty_columns(df)
  expect_equal(nrow(resultat), 1)
  expect_match(resultat$detail, "presque vide")
})

test_that("check_empty_columns ne signale rien sous 95 pourcent de manquants", {
  df <- data.frame(x = c(rep(NA_character_, 90), rep("1", 10)), stringsAsFactors = FALSE)
  expect_equal(nrow(check_empty_columns(df)), 0)
})

# Tests : check_level_variants ------------------------------------------------

test_that("check_level_variants regroupe les modalites equivalentes", {
  df <- data.frame(reponse = c("Oui", "oui", "OUI ", "Non", "Non"), stringsAsFactors = FALSE)
  profil <- .mini_profile("reponse", "nominale")
  resultat <- check_level_variants(df, profile = profil)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$n_affected, 3)
})

test_that("check_level_variants ignore les variables non categorielles", {
  df <- data.frame(commentaire = c("Oui", "oui", "OUI "), stringsAsFactors = FALSE)
  profil <- .mini_profile("commentaire", "texte")
  resultat <- check_level_variants(df, profile = profil)
  expect_equal(nrow(resultat), 0)
})

# Tests : check_high_cardinality ---------------------------------------------

test_that("check_high_cardinality signale plus de 30 modalites pour une variable nominale", {
  df <- data.frame(groupe = paste0("G", 1:35), stringsAsFactors = FALSE)
  profil <- .mini_profile("groupe", "nominale")
  resultat <- check_high_cardinality(df, profile = profil)
  expect_equal(nrow(resultat), 1)
  expect_match(resultat$detail, "35")
})

test_that("check_high_cardinality ne signale rien a 30 modalites ou moins", {
  df <- data.frame(groupe = paste0("G", 1:20), stringsAsFactors = FALSE)
  profil <- .mini_profile("groupe", "nominale")
  expect_equal(nrow(check_high_cardinality(df, profile = profil)), 0)
})

# Tests : check_impossible_values ---------------------------------------------

test_that("check_impossible_values signale une valeur hors bornes plausibles", {
  df <- data.frame(age = as.character(c(25, 40, 200, 55)), stringsAsFactors = FALSE)
  profil <- .mini_profile("age", "continue")
  resultat <- check_impossible_values(df, profile = profil)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$n_affected, 1)
  expect_equal(resultat$rows_affected[[1]], 3L)
})

test_that("check_impossible_values n'agit pas sur une variable sans regle correspondante", {
  df <- data.frame(score_test = as.character(c(1, 2, 99999, 4)), stringsAsFactors = FALSE)
  profil <- .mini_profile("score_test", "continue")
  expect_equal(nrow(check_impossible_values(df, profile = profil)), 0)
})

# Tests : check_outliers ------------------------------------------------------

test_that("check_outliers signale une valeur extreme selon la regle de Tukey", {
  df <- data.frame(
    score_test = as.character(c(10, 11, 12, 9, 10, 11, 10, 9, 12, 500)),
    stringsAsFactors = FALSE
  )
  profil <- .mini_profile("score_test", "continue")
  resultat <- check_outliers(df, profile = profil)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$n_affected, 1)
  expect_equal(resultat$rows_affected[[1]], 10L)
})

test_that("check_outliers exclut les valeurs deja signalees comme impossibles", {
  df <- data.frame(
    age = as.character(c(25, 26, 24, 27, 25, 26, 24, 27, 25, 300)),
    stringsAsFactors = FALSE
  )
  profil <- .mini_profile("age", "continue")

  resultat_outliers <- check_outliers(df, profile = profil)
  resultat_impossibles <- check_impossible_values(df, profile = profil)

  expect_equal(nrow(resultat_impossibles), 1)
  expect_equal(resultat_impossibles$rows_affected[[1]], 10L)
  expect_equal(nrow(resultat_outliers), 0)
})

# Tests : check_date_coherence ------------------------------------------------

test_that("check_date_coherence signale une inversion chronologique minoritaire", {
  df <- data.frame(
    date_naissance = c(
      "01/01/1980", "02/02/1990", "03/03/1970", "04/04/1985", "05/05/1975",
      "06/06/2000", "07/07/1988", "08/08/1992", "09/09/1965", "10/10/1978"
    ),
    date_inclusion = c(
      "01/01/2020", "02/02/2020", "03/03/2020", "01/01/1960", "05/05/2020",
      "06/06/2020", "07/07/2020", "08/08/2020", "09/09/2020", "10/10/2020"
    ),
    stringsAsFactors = FALSE
  )
  profil <- .mini_profile(c("date_naissance", "date_inclusion"), c("date", "date"))
  resultat <- check_date_coherence(df, profile = profil)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$n_affected, 1)
  expect_equal(resultat$rows_affected[[1]], 4L)
})

test_that("check_date_coherence ne signale rien sans majorite claire", {
  df <- data.frame(
    date_a = c("01/01/2020", "01/01/2020", "01/01/2020", "01/01/2020"),
    date_b = c("01/01/2019", "01/01/2021", "01/01/2019", "01/01/2021"),
    stringsAsFactors = FALSE
  )
  profil <- .mini_profile(c("date_a", "date_b"), c("date", "date"))
  expect_equal(nrow(check_date_coherence(df, profile = profil)), 0)
})

# Tests : check_numeric_in_text -----------------------------------------------

test_that("check_numeric_in_text signale une variable texte majoritairement numerique", {
  df <- data.frame(commentaire = c("12", "34", "56", "78", "texte libre"), stringsAsFactors = FALSE)
  profil <- .mini_profile("commentaire", "texte")
  resultat <- check_numeric_in_text(df, profile = profil)
  expect_equal(nrow(resultat), 1)
  expect_equal(resultat$n_affected, 4)
})

test_that("check_numeric_in_text ne signale rien pour du texte majoritairement non numerique", {
  df <- data.frame(commentaire = c("bonjour", "au revoir", "12", "merci"), stringsAsFactors = FALSE)
  profil <- .mini_profile("commentaire", "texte")
  expect_equal(nrow(check_numeric_in_text(df, profile = profil)), 0)
})

# Tests : st_detect_anomalies -------------------------------------------------

test_that("st_detect_anomalies consolide et trie par severite", {
  df <- data.frame(
    patient_id = c("P1", "P2", "P1", "P3"),
    age = as.character(c(30, 40, 30, 200)),
    site = rep("CHU_Paris", 4),
    stringsAsFactors = FALSE
  )
  profil <- .mini_profile(
    c("patient_id", "age", "site"),
    c("identifiant", "continue", "nominale")
  )

  resultat <- st_detect_anomalies(df, profil)

  expect_true(all(c("check_id", "severity", "variable", "n_affected", "pct_affected", "detail", "rows_affected") %in% names(resultat)))
  expect_true("duplicate_ids" %in% resultat$check_id)
  expect_true("impossible_values" %in% resultat$check_id)
  expect_true("constant_columns" %in% resultat$check_id)

  severites <- resultat$severity
  positions_bloquant <- which(severites == "bloquant")
  positions_information <- which(severites == "information")
  if (length(positions_bloquant) > 0 && length(positions_information) > 0) {
    expect_true(max(positions_bloquant) < min(positions_information))
  }
})

test_that("st_detect_anomalies respecte la nature declaree dans config au lieu du profil", {
  df <- data.frame(
    reference_libre = c("REF-01", "REF-02", "REF-01", "REF-03"),
    stringsAsFactors = FALSE
  )
  profil <- .mini_profile("reference_libre", "texte")
  config <- .build_config_with_dictionary(c(
    "  reference_libre:",
    "    nature: identifiant"
  ))

  sans_config <- st_detect_anomalies(df, profil, config = NULL)
  avec_config <- st_detect_anomalies(df, profil, config = config)

  expect_false("duplicate_ids" %in% sans_config$check_id)
  expect_true("duplicate_ids" %in% avec_config$check_id)
})

test_that("st_detect_anomalies retourne un data.frame vide si aucune anomalie n'est detectee", {
  df <- data.frame(x = c("A", "B", "C"), y = c("1", "2", "3"), stringsAsFactors = FALSE)
  profil <- .mini_profile(c("x", "y"), c("nominale", "entiere"))
  resultat <- st_detect_anomalies(df, profil)
  expect_s3_class(resultat, "data.frame")
  expect_true(all(c("check_id", "severity", "variable", "n_affected", "pct_affected", "detail", "rows_affected") %in% names(resultat)))
})

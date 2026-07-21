# Helpers -----------------------------------------------------------------

.nature_de <- function(profil, nom) {
  profil$inferred_nature[profil$name == nom]
}

# Tests : st_profile -- inference de nature -------------------------------

test_that("st_profile infere identifiant quand toutes les valeurs sont uniques", {
  df <- data.frame(
    reference_libre = paste0("X", 1:20),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "reference_libre"), "identifiant")
})

test_that("st_profile infere identifiant sur un ratio eleve avec un nom evocateur", {
  valeurs <- c(sprintf("C%03d", 1:96), sprintf("C%03d", 1:4))
  df <- data.frame(code_patient = valeurs, stringsAsFactors = FALSE)
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "code_patient"), "identifiant")
})

test_that("st_profile ne retient pas identifiant si le nom n'est pas evocateur", {
  valeurs <- c(paste0("mot", 1:38), "motA", "motA")
  df <- data.frame(valeur_libre = valeurs, stringsAsFactors = FALSE)
  profil <- st_profile(df)
  expect_false(.nature_de(profil, "valeur_libre") == "identifiant")
})

test_that("st_profile infere date sur un format jj/mm/aaaa", {
  df <- data.frame(
    date_inclusion = c("01/02/2020", "15/06/2021", "28/12/2019", "05/05/2022", "01/02/2020"),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "date_inclusion"), "date")
})

test_that("st_profile infere date sur un format aaaa-mm-jj", {
  df <- data.frame(
    date_iso = c("2020-02-01", "2021-06-15", "2019-12-28", "2022-05-05", "2020-02-01"),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "date_iso"), "date")
})

test_that("st_profile infere date sur une serie numerique Excel", {
  df <- data.frame(
    date_excel = as.character(c(44000, 44010, 44020, 44030, 44040, 44000)),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "date_excel"), "date")
})

test_that("st_profile infere binaire quand il y a exactement 2 modalites", {
  df <- data.frame(sexe = rep(c("H", "F"), 10), stringsAsFactors = FALSE)
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "sexe"), "binaire")
})

test_that("st_profile infere continue pour un numerique a forte cardinalite", {
  df <- data.frame(
    age = as.character(c(round(seq(18, 90, length.out = 25), 1), 18)),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "age"), "continue")
})

test_that("st_profile infere continue pour un numerique a decimales avec virgule", {
  df <- data.frame(
    taille = as.character(c(seq(1.5, 2.0, length.out = 20), 1.5)),
    stringsAsFactors = FALSE
  )
  df$taille <- gsub("\\.", ",", df$taille)
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "taille"), "continue")
})

test_that("st_profile infere entiere pour un numerique entier a faible cardinalite", {
  df <- data.frame(
    score = as.character(sample(1:10, 40, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "score"), "entiere")
})

test_that("st_profile infere nominale pour une variable categorielle a cardinalite moderee", {
  df <- data.frame(
    groupe = sample(c("Intervention", "Controle", "Placebo"), 30, replace = TRUE),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "groupe"), "nominale")
})

test_that("st_profile infere texte pour une variable a forte cardinalite non numerique", {
  df <- data.frame(
    commentaire = c(paste("Commentaire libre numero", 1:35), "Commentaire libre numero 1"),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  expect_equal(.nature_de(profil, "commentaire"), "texte")
})

# Tests : st_profile -- statistiques et non-mutation -----------------------

test_that("st_profile calcule les statistiques descriptives des variables continues", {
  valeurs <- c(20, 30, 40, 50, 60, 25, 35, 45, 55, 65, 22, 33, 44, 56, 61, 27, 20)
  df <- data.frame(age = as.character(valeurs), stringsAsFactors = FALSE)
  profil <- st_profile(df)
  ligne <- profil[profil$name == "age", ]
  expect_equal(ligne$min, 20)
  expect_equal(ligne$max, 65)
  expect_equal(ligne$mean, mean(valeurs))
})

test_that("st_profile calcule le nombre et le pourcentage de manquants", {
  df <- data.frame(
    variable = c("1", "2", "999", "", "3", "NA", "4", "5"),
    stringsAsFactors = FALSE
  )
  profil <- st_profile(df)
  ligne <- profil[profil$name == "variable", ]
  expect_equal(ligne$n_total, 8)
  expect_equal(ligne$n_missing, 3)
  expect_equal(ligne$pct_missing, 3 / 8)
})

test_that("st_profile ne modifie pas le data.frame fourni en entree", {
  df <- data.frame(x = c("1", "2", "999", "3"), stringsAsFactors = FALSE)
  copie <- df
  st_profile(df)
  expect_identical(df, copie)
})

test_that("st_profile retourne les colonnes attendues", {
  df <- data.frame(x = c("1", "2", "3"), stringsAsFactors = FALSE)
  profil <- st_profile(df)
  attendu <- c(
    "name", "inferred_nature", "n_total", "n_missing", "pct_missing",
    "n_distinct", "n_unique_ratio", "min", "max", "median", "mean", "sd",
    "top_levels", "sample_values"
  )
  expect_true(all(attendu %in% names(profil)))
})

# Tests : normalize_missing -------------------------------------------------

test_that("normalize_missing reconnait les codes de manquant standards", {
  x <- c("1", "", " ", "NA", "N/A", "NR", "ND", "NC", "-", "--", ".", "?", "999", "9999", "-99", "2")
  resultat <- normalize_missing(x)
  expect_equal(sum(is.na(resultat$x)), length(x) - 2)
  expect_false(is.na(resultat$x[1]))
  expect_false(is.na(resultat$x[length(x)]))
})

test_that("normalize_missing est insensible a la casse", {
  resultat <- normalize_missing(c("na", "Na", "NA", "valeur"))
  expect_true(all(is.na(resultat$x[1:3])))
  expect_false(is.na(resultat$x[4]))
})

test_that("normalize_missing retourne les codes rencontres", {
  resultat <- normalize_missing(c("1", "999", "999", "NR", "2"))
  expect_setequal(resultat$codes, c("999", "NR"))
})

test_that("normalize_missing ne modifie pas le vecteur d'entree", {
  x <- c("1", "999", "2")
  copie <- x
  normalize_missing(x)
  expect_identical(x, copie)
})

# Tests : normalize_levels --------------------------------------------------

test_that("normalize_levels regroupe les modalites equivalentes apres normalisation", {
  x <- c("Intervention", "intervention", " INTERVENTION ", "Contrôle", "controle", "Placebo")
  groupes <- normalize_levels(x)
  tailles <- sort(lengths(groupes))
  expect_equal(tailles, c(2, 3))
})

test_that("normalize_levels ne retourne rien si aucune modalite ne se regroupe", {
  groupes <- normalize_levels(c("A", "B", "C"))
  expect_equal(length(groupes), 0)
})

# Tests : parse_dates_robust -------------------------------------------------

test_that("parse_dates_robust detecte le format jj/mm/aaaa avec un taux de succes eleve", {
  resultat <- parse_dates_robust(c("01/02/2020", "15/06/2021", "28/12/2019"))
  expect_equal(resultat$format, "%d/%m/%Y")
  expect_equal(resultat$success_rate, 1)
})

test_that("parse_dates_robust retourne un taux de succes nul si aucune date ne correspond", {
  resultat <- parse_dates_robust(c("abc", "def"))
  expect_equal(resultat$success_rate, 0)
})

test_that("parse_dates_robust gere les valeurs manquantes sans les compter dans le taux", {
  resultat <- parse_dates_robust(c("01/02/2020", "15/06/2021", NA, ""))
  expect_equal(resultat$success_rate, 1)
  expect_true(is.na(resultat$dates[3]))
})

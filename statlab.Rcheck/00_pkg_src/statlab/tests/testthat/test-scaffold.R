# Helpers -----------------------------------------------------------------

.creer_projet_scaffold <- function() {
  repertoire <- tempfile("statlab_scaffold_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  repertoire
}

.ecrire_csv_scaffold <- function(repertoire, nom, lignes) {
  chemin <- file.path(repertoire, "donnees_brutes", nom)
  writeLines(lignes, chemin)
  chemin
}

# Tests -----------------------------------------------------------------

test_that("st_scaffold_config genere un config.yml valide sans modification", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c(
    "id,age,sexe",
    "P1,45,H", "P2,52,F", "P3,38,H", "P4,61,F", "P5,29,H"
  ))
  sortie <- file.path(repertoire, "config.yml")

  resultat <- st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)

  expect_true(file.exists(sortie))
  expect_equal(resultat, normalizePath(sortie, winslash = "/"))

  config <- st_validate_config(st_read_config(sortie))
  expect_true(attr(config, "valid"))
  expect_equal(config$sources[[1]]$id, "inclusion")
})

test_that("st_scaffold_config refuse d'ecraser un fichier existant par defaut", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52"))
  sortie <- file.path(repertoire, "config.yml")
  writeLines("existant", sortie)

  expect_error(
    st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie),
    "existe deja"
  )
  expect_equal(readLines(sortie), "existant")
})

test_that("st_scaffold_config ecrase le fichier si overwrite = TRUE", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52"))
  sortie <- file.path(repertoire, "config.yml")
  writeLines("existant", sortie)

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie, overwrite = TRUE)
  expect_false(identical(readLines(sortie), "existant"))
})

test_that("st_scaffold_config deduit l'identifiant de source depuis le nom de fichier si non nomme", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52"))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = chemin_source, output = sortie)
  config <- st_validate_config(st_read_config(sortie))
  expect_equal(config$sources[[1]]$id, "inclusion")
})

test_that("st_scaffold_config s'arrete sur des identifiants de sources dupliques", {
  repertoire <- .creer_projet_scaffold()
  s1 <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45"))
  dir.create(file.path(repertoire, "donnees_brutes", "archive"))
  s2 <- file.path(repertoire, "donnees_brutes", "archive", "inclusion.csv")
  writeLines(c("id,age", "P2,50"), s2)
  sortie <- file.path(repertoire, "config.yml")

  expect_error(st_scaffold_config(sources = c(s1, s2), output = sortie), "doublons")
})

test_that("st_scaffold_config s'arrete si le repertoire de destination est introuvable", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45"))
  sortie <- file.path(repertoire, "repertoire_absent", "config.yml")

  expect_error(
    st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie),
    "introuvable"
  )
})

test_that("st_scaffold_config fusionne plusieurs sources dans un dictionnaire commun", {
  repertoire <- .creer_projet_scaffold()
  s1 <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52", "P3,38"))
  s2 <- .ecrire_csv_scaffold(repertoire, "suivi.csv", c("id,valeur", "P1,10", "P2,20", "P3,30"))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = s1, suivi = s2), output = sortie)
  config <- st_validate_config(st_read_config(sortie))

  expect_equal(length(config$sources), 2)
  expect_true(all(c("id", "age", "valeur") %in% names(config$dictionnaire)))
})

test_that("st_scaffold_config liste les modalites observees pour les variables categorielles", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c(
    "id,groupe", "P1,Intervention", "P2,Controle", "P3,Intervention", "P4,Controle"
  ))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  config <- st_validate_config(st_read_config(sortie))

  expect_setequal(config$dictionnaire$groupe$modalites, c("Intervention", "Controle"))
})

test_that("st_scaffold_config propose un libelle via label_hints.yml", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c(
    "id,age,date_adm", "P1,45,01/01/2020", "P2,52,02/02/2020", "P3,38,03/03/2020"
  ))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  lignes <- readLines(sortie)
  config <- st_validate_config(st_read_config(sortie))

  expect_equal(config$dictionnaire$age$libelle, "Âge")
  expect_equal(config$dictionnaire$date_adm$libelle, "Date d'admission")
})

test_that("st_scaffold_config signale un libelle devine quand aucun indice ne correspond", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c(
    "id,variable_exotique_xyz", "P1,a", "P2,b", "P3,c"
  ))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  lignes <- readLines(sortie)

  expect_true(any(grepl("libelle devine", lignes)))
  config <- st_validate_config(st_read_config(sortie))
  expect_equal(config$dictionnaire$variable_exotique_xyz$libelle, "Variable exotique xyz")
})

test_that("st_scaffold_config signale les doublons stricts au niveau d'une source", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c(
    "id,age", "P1,45", "P2,52", "P1,45"
  ))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  lignes <- readLines(sortie)

  expect_true(any(grepl("A VERIFIER.*doublons stricts", lignes)))
})

test_that("st_scaffold_config signale les codages de manquant heterogenes", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c(
    "id,valeur", "P1,999", "P2,NR", "P3,10", "P4,20", "P5,30"
  ))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  lignes <- readLines(sortie)

  expect_true(any(grepl("A VERIFIER.*[Cc]odages de manquant heterogenes", lignes)))
})

test_that("st_scaffold_config prend en charge un onglet Excel et le declare dans config.yml", {
  repertoire <- .creer_projet_scaffold()
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Donnees")
  openxlsx::writeData(wb, "Donnees", rbind(c("id", "age"), c("P1", "45"), c("P2", "52")), colNames = FALSE)
  chemin_source <- file.path(repertoire, "donnees_brutes", "inclusion.xlsx")
  openxlsx::saveWorkbook(wb, chemin_source, overwrite = TRUE)
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  config <- st_validate_config(st_read_config(sortie))

  expect_equal(config$sources[[1]]$onglet, "Donnees")
  expect_equal(config$sources[[1]]$ligne_entete, 1)
})

test_that("st_scaffold_config journalise l'operation", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52"))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  journal <- st_log_read(repertoire)

  expect_true("generation_config" %in% journal$evenement)
})

test_that("st_scaffold_config laisse les sections analyse et preparation en gabarit commente", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52", "P3,38"))
  sortie <- file.path(repertoire, "config.yml")

  st_scaffold_config(sources = c(inclusion = chemin_source), output = sortie)
  contenu <- st_read_config(sortie)

  expect_null(contenu$analyse)
  expect_null(contenu$preparation)

  lignes <- readLines(sortie)
  expect_true(any(grepl("^# preparation:", lignes)))
  expect_true(any(grepl("^# analyse:", lignes)))
})

test_that("st_scaffold_config accepte des chemins de source relatifs au repertoire courant", {
  repertoire <- .creer_projet_scaffold()
  chemin_source <- .ecrire_csv_scaffold(repertoire, "inclusion.csv", c("id,age", "P1,45", "P2,52"))
  sortie <- file.path(repertoire, "config.yml")

  ancien_wd <- getwd()
  setwd(repertoire)
  on.exit(setwd(ancien_wd), add = TRUE)

  st_scaffold_config(sources = c(inclusion = "donnees_brutes/inclusion.csv"), output = "config.yml")
  config <- st_validate_config(st_read_config("config.yml"))
  expect_equal(config$sources[[1]]$fichier, "donnees_brutes/inclusion.csv")
})

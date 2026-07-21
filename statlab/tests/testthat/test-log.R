# Helpers -----------------------------------------------------------------

.creer_projet_journal <- function() {
  repertoire <- tempfile("statlab_journal_")
  dir.create(repertoire)
  repertoire
}

# Tests ---------------------------------------------------------------------

test_that("st_log_init cree le journal avec un en-tete de session", {
  repertoire <- .creer_projet_journal()
  chemin <- st_log_init(repertoire)

  expect_true(file.exists(chemin))
  lignes <- readLines(chemin)
  expect_true(any(startsWith(lignes, "===")))
  expect_match(lignes[1], "Session statlab")
  expect_match(lignes[1], "statlab")
  expect_match(lignes[1], "R version|R ")
})

test_that("st_log ecrit les 4 niveaux dans le journal", {
  repertoire <- .creer_projet_journal()
  st_log_init(repertoire)

  st_log("evt_info", module = "test", level = "info")
  st_log("evt_warn", module = "test", level = "warn")
  st_log("evt_error", module = "test", level = "error")
  st_log("evt_derogation", module = "test", level = "derogation")

  journal <- st_log_read(repertoire)
  expect_setequal(journal$niveau, c("info", "warn", "error", "derogation"))
  expect_setequal(journal$evenement, c("evt_info", "evt_warn", "evt_error", "evt_derogation"))
})

test_that("st_log s'arrete si le journal n'a pas ete initialise", {
  etat <- environment(st_log_init)$.statlab_log_state
  ancien_chemin <- etat$path
  etat$path <- NULL
  on.exit(etat$path <- ancien_chemin, add = TRUE)

  expect_error(st_log("evt", module = "test"), "initialise")
})

test_that("st_log_read relit le journal en data.frame avec les bonnes colonnes", {
  repertoire <- .creer_projet_journal()
  st_log_init(repertoire)
  st_log("lecture", module = "config", fichier = "config.yml", niveau_test = "ok")

  journal <- st_log_read(repertoire)
  expect_s3_class(journal, "data.frame")
  expect_setequal(names(journal), c("horodatage", "niveau", "module", "evenement", "details"))
  expect_equal(nrow(journal), 1)
  expect_equal(journal$module[1], "config")
  expect_equal(journal$evenement[1], "lecture")
  expect_match(journal$details[1], "fichier=config.yml")
})

test_that("l'horodatage ecrit est un ISO 8601 valide", {
  repertoire <- .creer_projet_journal()
  st_log_init(repertoire)
  st_log("evt", module = "test")

  journal <- st_log_read(repertoire)
  expect_match(journal$horodatage[1], "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}")
})

test_that("st_log_exclusion journalise n_before, n_after et le motif", {
  repertoire <- .creer_projet_journal()
  st_log_init(repertoire)
  st_log_exclusion(100, 90, "Age hors bornes")

  journal <- st_log_read(repertoire)
  expect_equal(journal$evenement[1], "exclusion_observations")
  expect_match(journal$details[1], "n_avant=100")
  expect_match(journal$details[1], "n_apres=90")
  expect_match(journal$details[1], "n_exclues=10")
  expect_match(journal$details[1], "motif=Age hors bornes")
})

test_that("st_log_exclusion s'arrete si n_after > n_before", {
  repertoire <- .creer_projet_journal()
  st_log_init(repertoire)
  expect_error(st_log_exclusion(10, 20, "motif"), "superieur")
})

test_that("options(statlab.quiet = TRUE) supprime les messages a l'ecran mais ecrit toujours le journal", {
  repertoire <- .creer_projet_journal()
  st_log_init(repertoire)

  old <- options(statlab.quiet = TRUE)
  on.exit(options(old), add = TRUE)

  expect_silent(st_log("evt_silencieux", module = "test"))

  journal <- st_log_read(repertoire)
  expect_true("evt_silencieux" %in% journal$evenement)
})

test_that("st_log_read retourne un data.frame vide si le journal ne contient aucun evenement", {
  repertoire <- .creer_projet_journal()
  st_log_init(repertoire)

  journal <- st_log_read(repertoire)
  expect_s3_class(journal, "data.frame")
  expect_equal(nrow(journal), 0)
})

test_that("st_log_read s'arrete si le journal n'existe pas", {
  repertoire <- .creer_projet_journal()
  expect_error(st_log_read(repertoire), "introuvable")
})

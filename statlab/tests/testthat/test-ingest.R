# Helpers -----------------------------------------------------------------

.creer_projet_ingest <- function() {
  repertoire <- tempfile("statlab_ingest_")
  dir.create(repertoire)
  st_log_init(repertoire)
  repertoire
}

.ecrire_texte_brut <- function(repertoire, nom, lignes) {
  chemin <- file.path(repertoire, nom)
  writeLines(lignes, chemin, useBytes = TRUE)
  chemin
}

.ecrire_xlsx <- function(repertoire, nom, feuilles) {
  chemin <- file.path(repertoire, nom)
  wb <- openxlsx::createWorkbook()
  for (nom_feuille in names(feuilles)) {
    openxlsx::addWorksheet(wb, nom_feuille)
    openxlsx::writeData(wb, nom_feuille, feuilles[[nom_feuille]], colNames = FALSE)
  }
  openxlsx::saveWorkbook(wb, chemin, overwrite = TRUE)
  chemin
}

# Tests : detect_delimiter ----------------------------------------------------

test_that("detect_delimiter reconnait la virgule", {
  repertoire <- tempfile("statlab_delim_")
  dir.create(repertoire)
  chemin <- .ecrire_texte_brut(repertoire, "a.csv", c("id,nom,age", "1,Alice,30", "2,Bob,25"))
  expect_equal(detect_delimiter(chemin), ",")
})

test_that("detect_delimiter reconnait le point-virgule", {
  repertoire <- tempfile("statlab_delim_")
  dir.create(repertoire)
  chemin <- .ecrire_texte_brut(repertoire, "a.csv", c("id;nom;age", "1;Alice;30", "2;Bob;25"))
  expect_equal(detect_delimiter(chemin), ";")
})

test_that("detect_delimiter reconnait la tabulation", {
  repertoire <- tempfile("statlab_delim_")
  dir.create(repertoire)
  chemin <- .ecrire_texte_brut(repertoire, "a.tsv", c("id\tnom\tage", "1\tAlice\t30", "2\tBob\t25"))
  expect_equal(detect_delimiter(chemin), "\t")
})

test_that("detect_delimiter reconnait la barre verticale", {
  repertoire <- tempfile("statlab_delim_")
  dir.create(repertoire)
  chemin <- .ecrire_texte_brut(repertoire, "a.txt", c("id|nom|age", "1|Alice|30", "2|Bob|25"))
  expect_equal(detect_delimiter(chemin), "|")
})

# Tests : detect_encoding ------------------------------------------------------

test_that("detect_encoding reconnait l'UTF-8 avec BOM", {
  repertoire <- tempfile("statlab_enc_")
  dir.create(repertoire)
  chemin <- file.path(repertoire, "a.csv")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("id,nom\n1,Ecole\n")), chemin)
  expect_equal(detect_encoding(chemin), "UTF-8")
})

test_that("detect_encoding reconnait l'UTF-8 sans BOM", {
  repertoire <- tempfile("statlab_enc_")
  dir.create(repertoire)
  chemin <- file.path(repertoire, "a.csv")
  writeBin(charToRaw(enc2utf8("id,nom\n1,École\n")), chemin)
  expect_equal(detect_encoding(chemin), "UTF-8")
})

test_that("detect_encoding reconnait le Latin-1", {
  repertoire <- tempfile("statlab_enc_")
  dir.create(repertoire)
  chemin <- file.path(repertoire, "a.csv")
  octets <- iconv("id,nom\n1,Résumé\n", from = "UTF-8", to = "ISO-8859-1", toRaw = TRUE)[[1]]
  writeBin(octets, chemin)
  expect_equal(detect_encoding(chemin), "Latin-1")
})

test_that("detect_encoding reconnait le Windows-1252", {
  repertoire <- tempfile("statlab_enc_")
  dir.create(repertoire)
  chemin <- file.path(repertoire, "a.csv")
  octets <- iconv("id,nom\n1,l’etude\n", from = "UTF-8", to = "WINDOWS-1252", toRaw = TRUE)[[1]]
  writeBin(octets, chemin)
  expect_equal(detect_encoding(chemin), "Windows-1252")
})

# Tests : detect_decimal_mark --------------------------------------------------

test_that("detect_decimal_mark reconnait le point", {
  repertoire <- tempfile("statlab_dec_")
  dir.create(repertoire)
  chemin <- .ecrire_texte_brut(repertoire, "a.csv", c("id,valeur", "1,3.14", "2,2.71"))
  expect_equal(detect_decimal_mark(chemin, ","), ".")
})

test_that("detect_decimal_mark reconnait la virgule quand le delimiteur est le point-virgule", {
  repertoire <- tempfile("statlab_dec_")
  dir.create(repertoire)
  chemin <- .ecrire_texte_brut(repertoire, "a.csv", c("id;valeur", "1;3,14", "2;2,71"))
  expect_equal(detect_decimal_mark(chemin, ";"), ",")
})

test_that("detect_decimal_mark retourne le point par defaut si aucun indice", {
  repertoire <- tempfile("statlab_dec_")
  dir.create(repertoire)
  chemin <- .ecrire_texte_brut(repertoire, "a.csv", c("id,nom", "1,Alice", "2,Bob"))
  expect_equal(detect_decimal_mark(chemin, ","), ".")
})

# Tests : detect_header_row ----------------------------------------------------

test_that("detect_header_row trouve la ligne 1 quand elle est plausible", {
  raw <- data.frame(
    V1 = c("id", "1", "2", "3"),
    V2 = c("nom", "Alice", "Bob", "Chloe"),
    stringsAsFactors = FALSE
  )
  expect_equal(detect_header_row(raw), 1L)
})

test_that("detect_header_row ignore une ligne de titre et trouve la vraie ligne d'en-tete", {
  raw <- data.frame(
    V1 = c("Rapport", "identifiant", "1", "2", "3", "4", "5"),
    V2 = c(NA, "valeur", "10", "20", "30", "40", "50"),
    stringsAsFactors = FALSE
  )
  expect_equal(detect_header_row(raw), 2L)
})

test_that("detect_header_row retourne NA si aucune ligne plausible", {
  raw <- data.frame(
    V1 = c("1", "4", "7"),
    V2 = c("2", "5", "8"),
    V3 = c("3", "6", "9"),
    stringsAsFactors = FALSE
  )
  expect_true(is.na(detect_header_row(raw)))
})

test_that("detect_header_row retourne NA sur un tableau vide", {
  raw <- data.frame(V1 = character(0), stringsAsFactors = FALSE)
  expect_true(is.na(detect_header_row(raw)))
})

# Tests : pick_sheet ------------------------------------------------------------

test_that("pick_sheet retient l'unique onglet disponible", {
  repertoire <- tempfile("statlab_sheet_")
  dir.create(repertoire)
  chemin <- .ecrire_xlsx(repertoire, "a.xlsx", list(Feuille1 = data.frame(a = 1:3, b = 4:6)))
  expect_equal(pick_sheet(chemin), "Feuille1")
})

test_that("pick_sheet s'arrete et liste les onglets si plusieurs sont disponibles", {
  repertoire <- tempfile("statlab_sheet_")
  dir.create(repertoire)
  chemin <- .ecrire_xlsx(
    repertoire, "a.xlsx",
    list(Feuille1 = data.frame(a = 1:3), Feuille2 = data.frame(b = 1:5, c = 1:5))
  )
  err <- tryCatch(pick_sheet(chemin), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "Feuille1")
  expect_match(conditionMessage(err), "Feuille2")
})

# Tests : st_read_source -- CSV -------------------------------------------------

test_that("st_read_source lit un CSV minimal et retourne toutes les colonnes en caractere", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(repertoire, "donnees.csv", c("id,nom,age", "1,Alice,30", "2,Bob,25"))

  resultat <- st_read_source(list(id = "s1", fichier = chemin))

  expect_s3_class(resultat, "data.frame")
  expect_true(all(vapply(resultat, is.character, logical(1))))
  expect_equal(colnames(resultat), c("id", "nom", "age"))
  expect_equal(nrow(resultat), 2)
  expect_equal(attr(resultat, "source_id"), "s1")
  expect_equal(attr(resultat, "header_row"), 1L)
  expect_equal(attr(resultat, "n_raw_rows"), 3L)
  expect_true(is.na(attr(resultat, "sheet")))
  expect_true(nzchar(attr(resultat, "file_hash")))
  expect_true(file.exists(attr(resultat, "file_path")))
})

test_that("st_read_source respecte ligne_entete declaree en config", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(
    repertoire, "donnees.csv",
    c("Rapport de test", "id,nom", "1,Alice", "2,Bob")
  )

  resultat <- st_read_source(list(id = "s1", fichier = chemin, ligne_entete = 2))

  expect_equal(colnames(resultat), c("id", "nom"))
  expect_equal(nrow(resultat), 2)
  expect_equal(attr(resultat, "header_row"), 2L)
})

test_that("st_read_source s'arrete si ligne_entete est hors bornes", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(repertoire, "donnees.csv", c("id,nom", "1,Alice"))

  expect_error(
    st_read_source(list(id = "s1", fichier = chemin, ligne_entete = 10)),
    "hors bornes"
  )
})

test_that("st_read_source s'arrete si la ligne d'en-tete ne peut pas etre detectee", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(repertoire, "donnees.csv", c("1,2,3", "4,5,6", "7,8,9"))

  expect_error(st_read_source(list(id = "s1", fichier = chemin)), "detecter")
})

test_that("st_read_source s'arrete si le fichier source est introuvable", {
  expect_error(
    st_read_source(list(id = "s1", fichier = file.path(tempdir(), "absent.csv"))),
    "introuvable"
  )
})

test_that("st_read_source s'arrete sur un format de fichier non pris en charge", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(repertoire, "donnees.json", c("{}"))
  expect_error(st_read_source(list(id = "s1", fichier = chemin)), "non pris en charge")
})

test_that("st_read_source ne modifie jamais le fichier source (lecture stricte)", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(repertoire, "donnees.csv", c("id,nom", "1,Alice", "2,Bob"))
  empreinte_avant <- digest::digest(file = chemin, algo = "sha256")

  st_read_source(list(id = "s1", fichier = chemin))

  empreinte_apres <- digest::digest(file = chemin, algo = "sha256")
  expect_equal(empreinte_avant, empreinte_apres)
})

test_that("st_read_source journalise chaque lecture", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(repertoire, "donnees.csv", c("id,nom", "1,Alice"))

  st_read_source(list(id = "s1", fichier = chemin))

  journal <- st_log_read(repertoire)
  expect_true("lecture_source" %in% journal$evenement)
  expect_match(journal$details[journal$evenement == "lecture_source"], "source_id=s1")
})

# Tests : st_read_source -- XLSX ------------------------------------------------

test_that("st_read_source lit un XLSX a onglet unique", {
  repertoire <- .creer_projet_ingest()
  feuille <- rbind(c("id", "nom"), c("1", "Alice"), c("2", "Bob"))
  chemin <- .ecrire_xlsx(repertoire, "donnees.xlsx", list(Feuille1 = feuille))

  resultat <- st_read_source(list(id = "s1", fichier = chemin))

  expect_equal(colnames(resultat), c("id", "nom"))
  expect_equal(nrow(resultat), 2)
  expect_equal(attr(resultat, "sheet"), "Feuille1")
  expect_true(all(vapply(resultat, is.character, logical(1))))
})

test_that("st_read_source utilise l'onglet declare en config parmi plusieurs onglets", {
  repertoire <- .creer_projet_ingest()
  feuille_a <- rbind(c("x", "y"), c("1", "2"))
  feuille_b <- rbind(c("id", "nom"), c("1", "Alice"), c("2", "Bob"))
  chemin <- .ecrire_xlsx(repertoire, "donnees.xlsx", list(A = feuille_a, B = feuille_b))

  resultat <- st_read_source(list(id = "s1", fichier = chemin, onglet = "B"))

  expect_equal(colnames(resultat), c("id", "nom"))
  expect_equal(attr(resultat, "sheet"), "B")
})

test_that("st_read_source s'arrete si l'onglet declare est introuvable", {
  repertoire <- .creer_projet_ingest()
  feuille_a <- rbind(c("x", "y"), c("1", "2"))
  chemin <- .ecrire_xlsx(repertoire, "donnees.xlsx", list(A = feuille_a))

  expect_error(
    st_read_source(list(id = "s1", fichier = chemin, onglet = "Z")),
    "introuvable"
  )
})

test_that("st_read_source s'arrete sur un XLSX multi-onglets sans onglet declare", {
  repertoire <- .creer_projet_ingest()
  feuille_a <- rbind(c("x", "y"), c("1", "2"))
  feuille_b <- rbind(c("id", "nom"), c("1", "Alice"))
  chemin <- .ecrire_xlsx(repertoire, "donnees.xlsx", list(A = feuille_a, B = feuille_b))

  expect_error(st_read_source(list(id = "s1", fichier = chemin)), "aucun n'est declare")
})

# Tests : bascule sur data.table::fread pour les gros fichiers -----------------

test_that("st_read_source bascule sur data.table::fread au-dela du seuil de taille", {
  repertoire <- .creer_projet_ingest()
  chemin <- .ecrire_texte_brut(repertoire, "donnees.csv", c("id,nom", "1,Alice", "2,Bob"))

  ns <- environment(st_read_source)
  ancien_seuil <- ns$.LARGE_FILE_BYTES
  unlockBinding(".LARGE_FILE_BYTES", ns)
  ns$.LARGE_FILE_BYTES <- 0
  lockBinding(".LARGE_FILE_BYTES", ns)
  on.exit({
    unlockBinding(".LARGE_FILE_BYTES", ns)
    ns$.LARGE_FILE_BYTES <- ancien_seuil
    lockBinding(".LARGE_FILE_BYTES", ns)
  }, add = TRUE)

  resultat <- st_read_source(list(id = "s1", fichier = chemin))

  expect_equal(colnames(resultat), c("id", "nom"))
  expect_equal(nrow(resultat), 2)
  expect_true(all(vapply(resultat, is.character, logical(1))))
})

# Tests : st_read_all_sources ---------------------------------------------------

test_that("st_read_all_sources lit toutes les sources et nomme la liste par id", {
  repertoire <- .creer_projet_ingest()
  dir.create(file.path(repertoire, "donnees_brutes"))
  .ecrire_texte_brut(repertoire, "donnees_brutes/inclusion.csv", c("id,age", "1,30", "2,25"))
  .ecrire_texte_brut(repertoire, "donnees_brutes/suivi.csv", c("id,valeur", "1,10", "2,20"))

  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude ingest",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "  - id: suivi",
    "    fichier: donnees_brutes/suivi.csv"
  ), chemin_config)

  config <- st_validate_config(st_read_config(chemin_config))
  resultats <- st_read_all_sources(config)

  expect_named(resultats, c("inclusion", "suivi"))
  expect_equal(nrow(resultats$inclusion), 2)
  expect_equal(colnames(resultats$suivi), c("id", "valeur"))
})

test_that("st_read_all_sources s'arrete si la config n'a pas ete validee", {
  repertoire <- .creer_projet_ingest()
  dir.create(file.path(repertoire, "donnees_brutes"))
  .ecrire_texte_brut(repertoire, "donnees_brutes/inclusion.csv", c("id,age", "1,30"))

  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude ingest",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv"
  ), chemin_config)

  config <- st_read_config(chemin_config)
  expect_error(st_read_all_sources(config), "validee")
})

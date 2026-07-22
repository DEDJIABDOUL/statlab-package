# Helpers -----------------------------------------------------------------

.projet_reconcile <- function() {
  repertoire <- tempfile("statlab_reconcile_")
  dir.create(repertoire)
  st_log_init(repertoire)
  repertoire
}

.fake_config <- function(reconciliation) {
  structure(
    list(reconciliation = reconciliation),
    class = "statlab_config",
    valid = TRUE
  )
}

.op_empiler <- function(sources, resultat, sur_colonnes_divergentes = "erreur") {
  list(
    operation = "empiler", sources = sources, resultat = resultat,
    sur_colonnes_divergentes = sur_colonnes_divergentes
  )
}

.op_joindre <- function(gauche, droite, cle, resultat, normaliser_cle = TRUE,
                         type = "gauche", alerte_explosion = TRUE) {
  list(
    operation = "joindre", gauche = gauche, droite = droite, cle = cle,
    normaliser_cle = normaliser_cle, type = type, resultat = resultat,
    alerte_explosion = alerte_explosion
  )
}

.op_pivoter <- function(source, cles, mesures, nom_temps, nom_valeur, resultat) {
  list(
    operation = "pivoter_long", source = source, cles = cles, mesures = mesures,
    nom_temps = nom_temps, nom_valeur = nom_valeur, resultat = resultat
  )
}

# Tests : normalize_key -------------------------------------------------------

test_that("normalize_key supprime les espaces, majuscule, et nettoie les caracteres", {
  expect_equal(normalize_key("  ab-12  "), "AB12")
  expect_equal(normalize_key("a b c"), "ABC")
  expect_equal(normalize_key("P-001"), "P001")
})

test_that("normalize_key harmonise les zeros initiaux des cles numeriques", {
  expect_equal(normalize_key("007"), "7")
  expect_equal(normalize_key("7"), "7")
  expect_equal(normalize_key("0000"), "0")
})

test_that("normalize_key ne tronque pas les zeros d'une cle non purement numerique", {
  expect_equal(normalize_key("A007"), "A007")
})

# Tests : operation empiler ---------------------------------------------------

test_that("st_reconcile empile deux sources aux colonnes identiques", {
  repertoire <- .projet_reconcile()
  sources <- list(
    a = data.frame(id = c("1", "2"), valeur = c("10", "20"), stringsAsFactors = FALSE),
    b = data.frame(id = c("3", "4"), valeur = c("30", "40"), stringsAsFactors = FALSE)
  )
  config <- .fake_config(list(.op_empiler(c("a", "b"), "empile")))

  resultat <- st_reconcile(sources, config)

  expect_equal(nrow(resultat$table), 4)
  expect_equal(resultat$audit$empile$n_resultat, 4)
  expect_equal(resultat$audit$empile$colonnes_divergentes, character(0))
})

test_that("st_reconcile s'arrete sur des colonnes divergentes par defaut (erreur)", {
  repertoire <- .projet_reconcile()
  sources <- list(
    a = data.frame(id = "1", valeur = "10", stringsAsFactors = FALSE),
    b = data.frame(id = "2", autre = "20", stringsAsFactors = FALSE)
  )
  config <- .fake_config(list(.op_empiler(c("a", "b"), "empile", "erreur")))

  expect_error(st_reconcile(sources, config), "[Dd]ivergentes")
})

test_that("st_reconcile empile sur l'intersection des colonnes", {
  repertoire <- .projet_reconcile()
  sources <- list(
    a = data.frame(id = "1", valeur = "10", extra_a = "x", stringsAsFactors = FALSE),
    b = data.frame(id = "2", valeur = "20", extra_b = "y", stringsAsFactors = FALSE)
  )
  config <- .fake_config(list(.op_empiler(c("a", "b"), "empile", "intersection")))

  resultat <- st_reconcile(sources, config)
  expect_setequal(names(resultat$table), c(".source_empilee", "id", "valeur"))
})

test_that("st_reconcile empile sur l'union des colonnes en completant par NA", {
  repertoire <- .projet_reconcile()
  sources <- list(
    a = data.frame(id = "1", valeur = "10", stringsAsFactors = FALSE),
    b = data.frame(id = "2", extra = "y", stringsAsFactors = FALSE)
  )
  config <- .fake_config(list(.op_empiler(c("a", "b"), "empile", "union")))

  resultat <- st_reconcile(sources, config)
  expect_true(is.na(resultat$table$extra[resultat$table$id == "1"]))
  expect_true(is.na(resultat$table$valeur[resultat$table$id == "2"]))
})

# Tests : operation joindre ----------------------------------------------------

test_that("st_reconcile joint deux sources et calcule l'audit de jointure", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c("1", "2", "3"), age = c("30", "40", "50"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("1", "2", "4"), poids = c("60", "70", "80"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint")))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  audit <- resultat$audit$joint

  expect_equal(audit$n_gauche, 3)
  expect_equal(audit$n_droite, 3)
  expect_equal(audit$n_apparies, 2)
  expect_equal(audit$n_orphelins_gauche, 1)
  expect_equal(audit$n_orphelins_droite, 1)
  expect_equal(audit$orphelins_gauche$id, "3")
  expect_equal(audit$orphelins_droite$id, "4")
  expect_equal(audit$ratio_expansion, 3 / 3)
  expect_equal(nrow(resultat$table), 3)
})

test_that("st_reconcile s'arrete sur une explosion de jointure par defaut", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c("1", "2"), age = c("30", "40"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("1", "1", "2"), poids = c("60", "61", "70"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint")))

  err <- tryCatch(st_reconcile(list(g = gauche, d = droite), config), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "[Ee]xplosion")
  expect_match(conditionMessage(err), "1 \\(x2\\)")
})

test_that("st_reconcile autorise l'explosion de jointure si alerte_explosion = FALSE", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c("1", "2"), age = c("30", "40"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("1", "1", "2"), poids = c("60", "61", "70"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", alerte_explosion = FALSE)))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  expect_equal(nrow(resultat$table), 3)
  expect_equal(resultat$audit$joint$ratio_expansion, 3 / 2)
})

test_that("st_reconcile applique une jointure interieure (inner)", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c("1", "2", "3"), age = c("30", "40", "50"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("1", "2", "4"), poids = c("60", "70", "80"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", type = "interieure", normaliser_cle = FALSE)))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  expect_equal(nrow(resultat$table), 2)
  expect_setequal(resultat$table$id, c("1", "2"))
})

test_that("st_reconcile applique une jointure complete (full) et garde les deux orphelins", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c("1", "2"), age = c("30", "40"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("2", "3"), poids = c("70", "90"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", type = "complete")))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  expect_equal(nrow(resultat$table), 3)
})

test_that("st_reconcile rapporte les orphelins nominatifs des deux cotes d'une jointure", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(
    id = c("1", "2", "3"), age = c("30", "40", "50"),
    stringsAsFactors = FALSE
  )
  droite <- data.frame(
    id = c("2", "3", "9"), poids = c("70", "80", "99"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", type = "complete", normaliser_cle = FALSE)))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  audit <- resultat$audit$joint

  expect_equal(audit$n_orphelins_gauche, 1)
  expect_equal(audit$n_orphelins_droite, 1)
  expect_equal(audit$orphelins_gauche$id, "1")
  expect_equal(audit$orphelins_droite$id, "9")
  expect_equal(nrow(resultat$table), 4)
  expect_true(all(c("1", "9") %in% resultat$table$id))
})

test_that("st_reconcile conserve la cle originale et signale les appariements obtenus apres normalisation", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c(" p1 ", "p2"), age = c("30", "40"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("P1", "P2"), poids = c("60", "70"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", normaliser_cle = TRUE)))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  audit <- resultat$audit$joint

  expect_equal(nrow(resultat$table), 2)
  expect_true(all(c("id_gauche", "id_droite") %in% names(resultat$table)))
  expect_equal(nrow(audit$appariements_apres_normalisation), 2)
})

test_that("st_reconcile n'apparie pas des cles qui different sans normalisation", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c(" p1 ", "p2"), age = c("30", "40"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("P1", "P2"), poids = c("60", "70"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", normaliser_cle = FALSE)))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  audit <- resultat$audit$joint

  expect_equal(audit$n_apparies, 0)
  expect_equal(audit$n_orphelins_gauche, 2)
})

test_that("st_reconcile signale les conflits de noms de colonnes et les suffixes", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = "1", valeur = "10", stringsAsFactors = FALSE)
  droite <- data.frame(id = "1", valeur = "99", stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", normaliser_cle = FALSE)))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  audit <- resultat$audit$joint

  expect_equal(audit$conflits_colonnes, "valeur")
  expect_true(all(c("valeur_gauche", "valeur_droite") %in% names(resultat$table)))
})

test_that("st_reconcile limite la liste nominative des orphelins a 100", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = as.character(1:150), stringsAsFactors = FALSE)
  droite <- data.frame(id = character(0), poids = character(0), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint")))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  expect_equal(resultat$audit$joint$n_orphelins_gauche, 150)
  expect_equal(nrow(resultat$audit$joint$orphelins_gauche), 100)
})

test_that("st_reconcile suggere des appariements approximatifs sans jamais les appliquer", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = c("ALICE", "BOB"), age = c("30", "40"), stringsAsFactors = FALSE)
  droite <- data.frame(id = c("ALICEE", "ROBERT"), poids = c("60", "70"), stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint", normaliser_cle = FALSE)))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  audit <- resultat$audit$joint

  expect_equal(audit$n_apparies, 0)
  expect_true(nrow(audit$suggestions_appariement_approximatif) >= 1)
  expect_true("ALICE" %in% audit$suggestions_appariement_approximatif$cle_gauche)
  expect_equal(nrow(resultat$table), 2)
})

test_that("st_reconcile joint sur une cle composite", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(
    id = c("1", "1", "2"), visite = c("t0", "t1", "t0"), age = c("30", "31", "40"),
    stringsAsFactors = FALSE
  )
  droite <- data.frame(
    id = c("1", "1"), visite = c("t0", "t1"), poids = c("60", "61"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config(list(.op_joindre("g", "d", c("id", "visite"), "joint")))

  resultat <- st_reconcile(list(g = gauche, d = droite), config)
  expect_equal(resultat$audit$joint$n_apparies, 2)
  expect_equal(resultat$audit$joint$n_orphelins_gauche, 1)
})

# Tests : operation pivoter_long -----------------------------------------------

test_that("st_reconcile pivote au format long", {
  repertoire <- .projet_reconcile()
  source_data <- data.frame(
    id_patient = c("1", "2"), t0 = c("10", "20"), t3 = c("11", "21"), t12 = c("12", "22"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config(list(
    .op_pivoter("s", cles = "id_patient", mesures = c("t0", "t3", "t12"), nom_temps = "temps", nom_valeur = "valeur", resultat = "long")
  ))

  resultat <- st_reconcile(list(s = source_data), config)

  expect_equal(nrow(resultat$table), 6)
  expect_setequal(names(resultat$table), c("id_patient", "temps", "valeur"))
  expect_equal(resultat$audit$long$n_lignes_avant, 2)
  expect_equal(resultat$audit$long$n_lignes_apres, 6)
})

# Tests : chainage et erreurs generiques ---------------------------------------

test_that("st_reconcile chaine plusieurs operations (empiler puis joindre)", {
  repertoire <- .projet_reconcile()
  a <- data.frame(id = "1", visite = "t0", valeur = "10", stringsAsFactors = FALSE)
  b <- data.frame(id = "2", visite = "t0", valeur = "20", stringsAsFactors = FALSE)
  meta <- data.frame(id = c("1", "2"), groupe = c("X", "Y"), stringsAsFactors = FALSE)

  config <- .fake_config(list(
    .op_empiler(c("a", "b"), "empile"),
    .op_joindre("empile", "meta", "id", "final")
  ))

  resultat <- st_reconcile(list(a = a, b = b, meta = meta), config)
  expect_equal(nrow(resultat$table), 2)
  expect_true("groupe" %in% names(resultat$table))
  expect_named(resultat$audit, c("empile", "final"))
})

test_that("st_reconcile s'arrete si une source referencee est introuvable", {
  repertoire <- .projet_reconcile()
  config <- .fake_config(list(.op_joindre("g", "absent", "id", "joint")))

  expect_error(
    st_reconcile(list(g = data.frame(id = "1", stringsAsFactors = FALSE)), config),
    "introuvable"
  )
})

test_that("st_reconcile s'arrete si aucune operation n'est declaree", {
  repertoire <- .projet_reconcile()
  config <- .fake_config(list())

  expect_error(
    st_reconcile(list(g = data.frame(id = "1", stringsAsFactors = FALSE)), config),
    "[Aa]ucune operation"
  )
})

test_that("st_reconcile journalise chaque operation", {
  repertoire <- .projet_reconcile()
  gauche <- data.frame(id = "1", age = "30", stringsAsFactors = FALSE)
  droite <- data.frame(id = "1", poids = "60", stringsAsFactors = FALSE)
  config <- .fake_config(list(.op_joindre("g", "d", "id", "joint")))

  st_reconcile(list(g = gauche, d = droite), config)
  journal <- st_log_read(repertoire)

  expect_true("reconciliation_joindre" %in% journal$evenement)
})

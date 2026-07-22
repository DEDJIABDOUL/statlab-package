# Helpers -----------------------------------------------------------------

.projet_prepare <- function() {
  repertoire <- tempfile("statlab_prepare_")
  dir.create(repertoire)
  st_log_init(repertoire)
  repertoire
}

.fake_config_prep <- function(preparation, dictionnaire = NULL) {
  structure(
    list(preparation = preparation, dictionnaire = dictionnaire),
    class = "statlab_config",
    valid = TRUE
  )
}

# Tests : variables ------------------------------------------------------------

test_that("st_prepare selectionne et renomme les variables", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", age = "30", sexe = "H", bruit = "x", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    variables = list(selectionner = c("id", "age", "sexe"), renommer = list(sexe = "genre"))
  ))

  resultat <- st_prepare(data, config)
  expect_setequal(names(resultat), c("id", "age", "genre"))
})

test_that("st_prepare s'arrete si une variable a selectionner est introuvable", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(variables = list(selectionner = c("id", "absente"))))

  expect_error(st_prepare(data, config), "introuvable")
})

# Tests : dates ------------------------------------------------------------------

test_that("st_prepare convertit une date selon le format declare", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = c("1", "2"), date_inclusion = c("01/02/2020", "15/06/2021"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    dates = list(date_inclusion = list(format = "%d/%m/%Y")),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_s3_class(resultat$date_inclusion, "Date")
  expect_equal(resultat$date_inclusion[1], as.Date("2020-02-01"))
})

test_that("st_prepare s'arrete si la conversion de date echoue pour une valeur non manquante", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", date_inclusion = "2020-02-01", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(dates = list(date_inclusion = list(format = "%d/%m/%Y"))))

  expect_error(st_prepare(data, config), "[Ee]chec de conversion")
})

test_that("st_prepare conserve le type Date d'une colonne avec des manquants et strategie 'conserver'", {
  repertoire <- .projet_prepare()
  data <- data.frame(
    id = c("1", "2", "3"), date_inclusion = c("01/02/2020", "999", "15/06/2021"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config_prep(list(
    dates = list(date_inclusion = list(format = "%d/%m/%Y")),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_s3_class(resultat$date_inclusion, "Date")
  expect_true(is.na(resultat$date_inclusion[2]))
})

# Tests : recodages --------------------------------------------------------------

test_that("st_prepare fusionne les modalites et applique l'ordre", {
  repertoire <- .projet_prepare()
  data <- data.frame(
    id = c("1", "2", "3", "4"), sexe = c("H", "homme", "F", "femme"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config_prep(list(
    recodages = list(sexe = list(
      fusionner = list(Homme = c("H", "homme"), Femme = c("F", "femme")),
      ordre = c("Homme", "Femme")
    )),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_s3_class(resultat$sexe, "factor")
  expect_equal(levels(resultat$sexe), c("Homme", "Femme"))
  expect_equal(as.character(resultat$sexe), c("Homme", "Homme", "Femme", "Femme"))
})

test_that("st_prepare s'arrete si 'ordre' ne couvre pas toutes les modalites", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = c("1", "2"), sexe = c("H", "Autre"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    recodages = list(sexe = list(ordre = c("H")))
  ))

  expect_error(st_prepare(data, config), "non couverte")
})

# Tests : derivations -------------------------------------------------------------

test_that("st_prepare calcule une variable derivee autorisee", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", poids = "70", taille = "175", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    derivations = list(list(
      nom = "imc", formule = "as.numeric(poids) / (as.numeric(taille) / 100)^2", libelle = "IMC"
    )),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_equal(round(resultat$imc, 2), round(70 / (1.75^2), 2))
})

test_that("st_prepare s'arrete sur une fonction non autorisee dans une derivation", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", poids = "70", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    derivations = list(list(nom = "risque", formule = "system('echo x')", libelle = "x"))
  ))

  expect_error(st_prepare(data, config), "non autorisee")
})

test_that("st_prepare s'arrete si une derivation reference une colonne inexistante", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    derivations = list(list(nom = "x", formule = "as.numeric(absente) * 2", libelle = "x"))
  ))

  expect_error(st_prepare(data, config), "echoue")
})

test_that("st_prepare interdit les appels de fonction masques (namespace ou anonymes)", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    derivations = list(list(nom = "x", formule = "base::system('echo x')", libelle = "x"))
  ))

  expect_error(st_prepare(data, config), "non autorisee")
})

test_that("st_prepare interdit une formule contenant plusieurs instructions", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", poids = "70", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    derivations = list(list(nom = "x", formule = "1; as.numeric(poids)", libelle = "x"))
  ))

  expect_error(st_prepare(data, config), "seule instruction")
})

# Tests : classes -----------------------------------------------------------------

test_that("st_prepare decoupe une variable selon des seuils et libelles", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:5), age = c("20", "45", "60", "70", "90"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    classes = list(age = list(seuils = c(40, 60, 75), libelles = c("<40", "40-59", "60-74", ">=75"))),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_equal(as.character(resultat$age), c("<40", "40-59", "60-74", "60-74", ">=75"))
})

test_that("st_prepare decoupe une variable en quantiles", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:8), imc = as.character(c(18, 19, 20, 21, 22, 23, 24, 25)), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    classes = list(imc = list(methode = "quantiles", n = 4)),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_equal(nlevels(resultat$imc), 4)
  expect_setequal(levels(resultat$imc), c("Q1", "Q2", "Q3", "Q4"))
})

test_that("st_prepare s'arrete si trop peu de valeurs distinctes pour le nombre de quantiles demande", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:6), score = rep("5", 6), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(classes = list(score = list(methode = "quantiles", n = 4))))

  expect_error(st_prepare(data, config), "classe")
})

# Tests : exclusions --------------------------------------------------------------

test_that("st_prepare exclut les lignes selon une condition et journalise", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:4), age = c("10", "20", "30", "40"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    exclusions = list(list(condition = "as.numeric(age) < 18", motif = "Mineurs")),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_equal(nrow(resultat), 3)

  journal <- st_log_read(repertoire)
  expect_true("exclusion_observations" %in% journal$evenement)
  expect_match(journal$details[journal$evenement == "exclusion_observations"][1], "motif=Mineurs")
})

test_that("st_prepare conserve les lignes dont la condition d'exclusion est NA", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:3), age = c("10", NA, "40"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(
    exclusions = list(list(condition = "as.numeric(age) < 18", motif = "Mineurs")),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_equal(nrow(resultat), 2)
})

# Tests : manquants ---------------------------------------------------------------

test_that("st_prepare s'arrete si des manquants existent sans strategie declaree", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", age = "999", stringsAsFactors = FALSE)
  config <- .fake_config_prep(list())

  expect_error(st_prepare(data, config), "sans strategie")
})

test_that("st_prepare 'conserve' normalise les codes de manquant en NA sans supprimer de lignes", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:3), age = c("30", "999", "40"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(manquants = list(list(strategie = "conserver", variables = "age"))))

  resultat <- st_prepare(data, config)
  expect_equal(nrow(resultat), 3)
  expect_true(is.na(resultat$age[2]))
})

test_that("st_prepare 'exclure_ligne' supprime les lignes manquantes et journalise", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:3), age = c("30", "999", "40"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(manquants = list(list(strategie = "exclure_ligne", variables = "age"))))

  resultat <- st_prepare(data, config)
  expect_equal(nrow(resultat), 2)
  expect_false("999" %in% resultat$age)
})

test_that("st_prepare 'imputer' remplace les manquants numeriques par la mediane", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = as.character(1:5), valeur = c("10", "20", "999", "30", "40"), stringsAsFactors = FALSE)
  config <- .fake_config_prep(list(manquants = list(list(strategie = "imputer", variables = "valeur"))))

  resultat <- st_prepare(data, config)
  expect_equal(nrow(resultat), 5)
  expect_equal(as.numeric(resultat$valeur[3]), median(c(10, 20, 30, 40)))
})

test_that("st_prepare 'imputer' remplace les manquants categoriels par le mode", {
  repertoire <- .projet_prepare()
  data <- data.frame(
    id = as.character(1:5), groupe = c("A", "A", "B", "999", "A"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config_prep(list(manquants = list(list(strategie = "imputer", variables = "groupe"))))

  resultat <- st_prepare(data, config)
  expect_equal(resultat$groupe[4], "A")
})

test_that("st_prepare applique une entree catch-all aux variables non listees", {
  repertoire <- .projet_prepare()
  data <- data.frame(
    id = as.character(1:4), age = c("30", "999", "40", "50"),
    commentaire = c("ok", "fine", "999", "done"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config_prep(list(manquants = list(
    list(strategie = "exclure_ligne", variables = "age"),
    list(strategie = "conserver")
  )))

  resultat <- st_prepare(data, config)
  expect_equal(nrow(resultat), 3)
  expect_true(any(is.na(resultat$commentaire)))
})

test_that("st_prepare s'arrete si le config n'a pas ete validee", {
  repertoire <- .projet_prepare()
  data <- data.frame(id = "1", stringsAsFactors = FALSE)
  config <- structure(list(preparation = NULL), class = "statlab_config", valid = FALSE)

  expect_error(st_prepare(data, config), "validee")
})

# Tests : chainage complet ---------------------------------------------------------

test_that("st_prepare applique les etapes dans l'ordre documente", {
  repertoire <- .projet_prepare()
  data <- data.frame(
    id = as.character(1:4),
    poids = c("70", "80", "60", "999"),
    taille = c("175", "180", "165", "170"),
    stringsAsFactors = FALSE
  )
  config <- .fake_config_prep(list(
    derivations = list(list(
      nom = "imc", formule = "as.numeric(poids) / (as.numeric(taille) / 100)^2", libelle = "IMC"
    )),
    exclusions = list(list(condition = "imc > 40", motif = "IMC invraisemblable")),
    manquants = list(list(strategie = "conserver"))
  ))

  resultat <- st_prepare(data, config)
  expect_true("imc" %in% names(resultat))
  expect_equal(nrow(resultat), 3)
  expect_true(all(resultat$imc <= 40))
})

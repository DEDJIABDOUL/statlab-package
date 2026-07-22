# Helpers -----------------------------------------------------------------

.creer_projet_test <- function() {
  repertoire <- tempfile("statlab_projet_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  writeLines("id,age", file.path(repertoire, "donnees_brutes", "inclusion.csv"))
  writeLines("id,valeur", file.path(repertoire, "donnees_brutes", "suivi.csv"))
  repertoire
}

.ecrire_config <- function(repertoire, contenu_yaml) {
  chemin <- file.path(repertoire, "config.yml")
  writeLines(contenu_yaml, chemin)
  chemin
}

# Tests ---------------------------------------------------------------------

test_that("st_read_config lit une config minimale valide", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
")
  config <- st_read_config(chemin)
  expect_s3_class(config, "statlab_config")
  expect_equal(config$projet$nom, "Etude test")

  config_valide <- st_validate_config(config)
  expect_true(attr(config_valide, "valid"))
  expect_equal(config_valide$projet$langue, "fr")
})

test_that("st_validate_config valide une config complete", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude complete
  langue: fr
  client: Client X
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
    onglet: Feuille1
    ligne_entete: 1
  - id: suivi
    fichier: donnees_brutes/suivi.csv
reconciliation:
  - operation: joindre
    gauche: inclusion
    droite: suivi
    cle: id
    resultat: inclusion_suivi
dictionnaire:
  age:
    nature: continue
    libelle: Age
    unite: annees
  groupe:
    nature: nominale
    modalites: [\"a\", \"b\"]
preparation:
  exclusions:
    - condition: 'age < 18'
      motif: Mineur
  derivations:
    - nom: imc
      formule: 'poids / taille^2'
      libelle: IMC
  manquants:
    - strategie: exclure_ligne
      variables: [age]
analyse:
  tableau_1:
    stratification: groupe
    variables: [\"age\"]
  comparaisons:
    - variables: [\"age\"]
      groupe: groupe
      apparie: false
rendu:
  charte: charte_x
  declinaisons: [\"ecran\"]
  decimales: 2
  formats: [\"docx\"]
")
  config <- st_validate_config(st_read_config(chemin))
  expect_true(attr(config, "valid"))
  expect_equal(length(config$sources), 2)
  expect_equal(config$rendu$decimales, 2)
  expect_false(config$analyse$comparaisons[[1]]$apparie)
  expect_true(config$reconciliation[[1]]$normaliser_cle)
  expect_equal(config$reconciliation[[1]]$type, "gauche")
  expect_true(config$reconciliation[[1]]$alerte_explosion)
})

test_that("st_validate_config valide les trois types d'operations de reconciliation", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
  - id: suivi
    fichier: donnees_brutes/suivi.csv
reconciliation:
  - operation: empiler
    sources: [inclusion, suivi]
    resultat: empile
    sur_colonnes_divergentes: union
  - operation: joindre
    gauche: inclusion
    droite: suivi
    cle: [id, visite]
    normaliser_cle: false
    type: complete
    resultat: joint
    alerte_explosion: false
  - operation: pivoter_long
    source: joint
    cles: [id]
    mesures: [t0, t3, t12]
    nom_temps: temps
    nom_valeur: valeur
    resultat: long
")
  config <- st_validate_config(st_read_config(chemin))
  expect_equal(config$reconciliation[[1]]$resultat, "empile")
  expect_equal(config$reconciliation[[2]]$cle, c("id", "visite"))
  expect_false(config$reconciliation[[2]]$normaliser_cle)
  expect_equal(config$reconciliation[[3]]$mesures, c("t0", "t3", "t12"))
})

test_that("st_validate_config s'arrete sur une operation de reconciliation inconnue", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
reconciliation:
  - operation: fusionner_par_magie
    sources: [inclusion]
    resultat: x
")
  expect_error(st_validate_config(st_read_config(chemin)), "reconciliation\\[1\\].operation")
})

test_that("st_validate_config s'arrete si une operation empiler a moins de deux sources", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
reconciliation:
  - operation: empiler
    sources: [inclusion]
    resultat: x
")
  expect_error(st_validate_config(st_read_config(chemin)), "au moins deux")
})

test_that("st_validate_config s'arrete sur un champ inconnu dans une operation joindre", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
  - id: suivi
    fichier: donnees_brutes/suivi.csv
reconciliation:
  - operation: joindre
    gauche: inclusion
    droite: suivi
    cle: id
    resultat: x
    priorite: haute
")
  expect_error(
    st_validate_config(st_read_config(chemin)),
    "reconciliation\\[1\\].priorite"
  )
})

test_that("st_read_config s'arrete si le fichier n'existe pas", {
  expect_error(st_read_config(tempfile(fileext = ".yml")), class = "rlang_error")
})

test_that("st_read_config s'arrete sur un YAML malforme", {
  repertoire <- .creer_projet_test()
  chemin <- file.path(repertoire, "config.yml")
  writeLines("projet:\n  nom: [oubli de fermeture", chemin)
  expect_error(st_read_config(chemin))
})

test_that("st_validate_config s'arrete si un champ requis est manquant (projet.nom)", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  langue: fr
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
")
  expect_error(st_validate_config(st_read_config(chemin)), "projet.nom")
})

test_that("st_validate_config s'arrete si la section sources est absente", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
")
  expect_error(st_validate_config(st_read_config(chemin)), "sources")
})

test_that("st_validate_config s'arrete sur un champ inconnu a un niveau connu", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
  pays: France
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
")
  err <- tryCatch(
    st_validate_config(st_read_config(chemin)),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "projet.pays")
})

test_that("st_validate_config s'arrete sur un identifiant de source duplique", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
  - id: inclusion
    fichier: donnees_brutes/suivi.csv
")
  expect_error(st_validate_config(st_read_config(chemin)), "duplique")
})

test_that("st_validate_config s'arrete si un fichier declare est introuvable", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/absent.csv
")
  expect_error(st_validate_config(st_read_config(chemin)), "introuvable")
})

test_that("st_validate_config s'arrete sur une enumeration invalide (dictionnaire)", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
dictionnaire:
  age:
    nature: flottante
")
  expect_error(st_validate_config(st_read_config(chemin)), "dictionnaire.age.nature")
})

test_that("st_validate_config s'arrete sur une strategie de manquants invalide", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
preparation:
  manquants:
    - strategie: supprimer_tout
      variables: [age]
")
  expect_error(
    st_validate_config(st_read_config(chemin)),
    "preparation.manquants\\[1\\].strategie"
  )
})

test_that("st_validate_config valide les sections variables, dates, recodages et classes de preparation", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
preparation:
  variables:
    selectionner: [id, age, sexe, date_inclusion]
    renommer: {sexe: genre}
  dates:
    date_inclusion:
      format: \"%d/%m/%Y\"
  recodages:
    genre:
      fusionner: {Homme: [H, homme, HOMME], Femme: [F, femme, FEMME]}
      ordre: [Homme, Femme]
  classes:
    age:
      seuils: [40, 60, 75]
      libelles: [\"<40\", \"40-59\", \"60-74\", \">=75\"]
    imc:
      methode: quantiles
      \"n\": 4
")
  config <- st_validate_config(st_read_config(chemin))
  expect_equal(config$preparation$variables$selectionner, c("id", "age", "sexe", "date_inclusion"))
  expect_equal(config$preparation$dates$date_inclusion$format, "%d/%m/%Y")
  expect_equal(config$preparation$recodages$genre$ordre, c("Homme", "Femme"))
  expect_equal(config$preparation$classes$imc$n, 4)
})

test_that("st_validate_config s'arrete si classes declare a la fois seuils et methode", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
preparation:
  classes:
    age:
      seuils: [40, 60]
      libelles: [\"<40\", \"40-59\", \">=60\"]
      methode: quantiles
      \"n\": 4
")
  expect_error(st_validate_config(st_read_config(chemin)), "preparation.classes.age")
})

test_that("st_validate_config s'arrete si le nombre de libelles ne correspond pas aux seuils", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
preparation:
  classes:
    age:
      seuils: [40, 60, 75]
      libelles: [\"<40\", \">=40\"]
")
  expect_error(st_validate_config(st_read_config(chemin)), "libelle de plus")
})

test_that("st_validate_config signale clairement le champ 'n' non protege par des guillemets", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
preparation:
  classes:
    imc:
      methode: quantiles
      n: 4
")
  expect_error(st_validate_config(st_read_config(chemin)), "guillemets")
})

test_that("st_validate_config s'arrete si une variable a deux strategies de manquants", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
preparation:
  manquants:
    - strategie: conserver
      variables: [age]
    - strategie: imputer
      variables: [age, poids]
")
  expect_error(st_validate_config(st_read_config(chemin)), "plusieurs entrees")
})

test_that("st_validate_config s'arrete sur plusieurs entrees manquants sans 'variables' (catch-all)", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
preparation:
  manquants:
    - strategie: conserver
    - strategie: exclure_ligne
")
  expect_error(st_validate_config(st_read_config(chemin)), "catch-all")
})

test_that("st_validate_config s'arrete sur une valeur invalide de rendu.declinaisons", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
rendu:
  declinaisons: [\"word\"]
")
  expect_error(st_validate_config(st_read_config(chemin)), "rendu.declinaisons")
})

test_that("st_validate_config valide forcer_test dans analyse.comparaisons", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
analyse:
  comparaisons:
    - variables: [age]
      groupe: groupe
      forcer_test: student
")
  config <- st_validate_config(st_read_config(chemin))
  expect_equal(config$analyse$comparaisons[[1]]$forcer_test, "student")
})

test_that("st_validate_config applique le denominateur par defaut de tableau_1 et valide l'enum", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
analyse:
  tableau_1:
    variables: [age]
")
  config <- st_validate_config(st_read_config(chemin))
  expect_equal(config$analyse$tableau_1$denominateur, "exclure_manquants")

  chemin2 <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
analyse:
  tableau_1:
    variables: [age]
    denominateur: pourcentage_bizarre
")
  expect_error(st_validate_config(st_read_config(chemin2)), "denominateur")
})

test_that("st_validate_config s'arrete si dictionnaire n'est pas nomme par variable", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
dictionnaire:
  - nature: continue
")
  expect_error(st_validate_config(st_read_config(chemin)), "dictionnaire")
})

test_that("st_validate_config s'arrete sur un champ inconnu dans analyse.comparaisons", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
analyse:
  comparaisons:
    - variables: [\"age\"]
      groupe: bras
      alpha: 0.05
")
  expect_error(
    st_validate_config(st_read_config(chemin)),
    "analyse.comparaisons\\[1\\].alpha"
  )
})

test_that("st_validate_config applique la valeur par defaut de rendu.decimales", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
rendu:
  charte: charte_x
")
  config <- st_validate_config(st_read_config(chemin))
  expect_equal(config$rendu$decimales, 1L)
})

test_that("print.statlab_config affiche un resume lisible", {
  repertoire <- .creer_projet_test()
  chemin <- .ecrire_config(repertoire, "
projet:
  nom: Etude test
sources:
  - id: inclusion
    fichier: donnees_brutes/inclusion.csv
")
  config <- st_validate_config(st_read_config(chemin))
  sortie <- paste(testthat::capture_messages(print(config)), collapse = "")
  expect_match(sortie, "Configuration statlab")
  expect_match(sortie, "Etude test")
})

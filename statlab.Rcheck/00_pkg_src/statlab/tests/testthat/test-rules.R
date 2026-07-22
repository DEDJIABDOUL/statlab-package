# Helpers -----------------------------------------------------------------

.projet_rules <- function() {
  repertoire <- tempfile("statlab_rules_")
  dir.create(repertoire)
  st_log_init(repertoire)
  repertoire
}

.ecrire_referentiel <- function(lignes) {
  chemin <- tempfile(fileext = ".yml")
  writeLines(lignes, chemin)
  chemin
}

.referentiel_minimal <- c(
  "version: \"9.9.9\"",
  "regles:",
  "  - id: TEST-001",
  "    famille: preconditions",
  "    condition: \"valeur > 10\"",
  "    seuils: {valeur: 10}",
  "    severite: avertissement",
  "    message: \"Valeur elevee : {valeur}.\"",
  "    source: \"Regle de test\""
)

# Tests : st_load_rules -------------------------------------------------------

test_that("st_load_rules charge le referentiel embarque avec sa version", {
  rules <- st_load_rules()
  expect_s3_class(rules, "statlab_rules")
  expect_false(is.null(attr(rules, "version")))
  expect_true(length(rules) >= 5)
  expect_true(all(vapply(rules, function(r) r$famille, character(1)) == "preconditions"))
})

test_that("st_load_rules charge un referentiel personnalise via 'path'", {
  chemin <- .ecrire_referentiel(.referentiel_minimal)
  rules <- st_load_rules(chemin)
  expect_equal(attr(rules, "version"), "9.9.9")
  expect_equal(names(rules), "TEST-001")
})

test_that("st_load_rules s'arrete si 'version' est absent", {
  chemin <- .ecrire_referentiel(c(
    "regles:",
    "  - id: TEST-001",
    "    famille: preconditions",
    "    condition: \"valeur > 10\"",
    "    severite: avertissement",
    "    message: \"x\""
  ))
  expect_error(st_load_rules(chemin), "version")
})

test_that("st_load_rules s'arrete si 'regles' est absent ou vide", {
  chemin <- .ecrire_referentiel(c("version: \"1.0.0\"", "regles: []"))
  expect_error(st_load_rules(chemin), "regles")
})

test_that("st_load_rules s'arrete sur des identifiants de regle dupliques", {
  chemin <- .ecrire_referentiel(c(
    "version: \"1.0.0\"",
    "regles:",
    "  - id: DUP-001",
    "    famille: preconditions",
    "    condition: \"x > 1\"",
    "    severite: information",
    "    message: \"m\"",
    "  - id: DUP-001",
    "    famille: preconditions",
    "    condition: \"x > 2\"",
    "    severite: information",
    "    message: \"m\""
  ))
  expect_error(st_load_rules(chemin), "duplique")
})

test_that("st_load_rules s'arrete sur un champ inconnu dans une regle", {
  chemin <- .ecrire_referentiel(c(
    "version: \"1.0.0\"",
    "regles:",
    "  - id: X-001",
    "    famille: preconditions",
    "    condition: \"x > 1\"",
    "    severite: information",
    "    message: \"m\"",
    "    priorite: haute"
  ))
  expect_error(st_load_rules(chemin), "regles\\[1\\].priorite")
})

test_that("st_load_rules s'arrete sur une severite invalide", {
  chemin <- .ecrire_referentiel(c(
    "version: \"1.0.0\"",
    "regles:",
    "  - id: X-001",
    "    famille: preconditions",
    "    condition: \"x > 1\"",
    "    severite: critique",
    "    message: \"m\""
  ))
  expect_error(st_load_rules(chemin), "severite")
})

test_that("st_load_rules exige une justification des qu'une action est declaree", {
  chemin <- .ecrire_referentiel(c(
    "version: \"1.0.0\"",
    "regles:",
    "  - id: X-001",
    "    famille: preconditions",
    "    condition: \"x > 1\"",
    "    severite: bloquant",
    "    message: \"m\"",
    "    action: corriger"
  ))
  expect_error(st_load_rules(chemin), "justification")
})

test_that("st_load_rules s'arrete si 'seuils' n'est pas une liste nommee", {
  chemin <- .ecrire_referentiel(c(
    "version: \"1.0.0\"",
    "regles:",
    "  - id: X-001",
    "    famille: preconditions",
    "    condition: \"x > 1\"",
    "    seuils: [1, 2]",
    "    severite: information",
    "    message: \"m\""
  ))
  expect_error(st_load_rules(chemin), "seuils")
})

# Tests : st_rules_report ------------------------------------------------------

test_that("st_rules_report retourne un tableau lisible du referentiel", {
  rapport <- st_rules_report()
  expect_s3_class(rapport, "data.frame")
  expect_true(all(c("id", "famille", "severite", "condition", "seuils", "message", "action", "source") %in% names(rapport)))
  expect_true("NORM-002" %in% rapport$id)
})

# Tests : st_evaluate_rules -----------------------------------------------------

test_that("st_evaluate_rules declenche NORM-002 (bloquant + action) et interpole la justification", {
  repertoire <- .projet_rules()
  contexte <- list(
    test_type = "comparaison_2_groupes", is_continuous = TRUE,
    min_group_n = 20, shapiro_p = 0.01, levene_p = 0.9
  )
  resultat <- st_evaluate_rules(contexte, family = "preconditions")

  ligne <- resultat[resultat$id == "NORM-002", ]
  expect_equal(nrow(ligne), 1)
  expect_equal(ligne$action, "bascule_mann_whitney")
  expect_match(ligne$justification, "0.01")
  expect_match(ligne$justification, "20")
  expect_false(ligne$derogation)
})

test_that("st_evaluate_rules declenche NORM-001 (information) sans arreter l'execution", {
  contexte <- list(
    test_type = "comparaison_2_groupes", is_continuous = TRUE,
    min_group_n = 50, shapiro_p = 0.01, levene_p = 0.9
  )
  resultat <- st_evaluate_rules(contexte, family = "preconditions")
  expect_true("NORM-001" %in% resultat$id)
  expect_equal(resultat$severite[resultat$id == "NORM-001"], "information")
})

test_that("st_evaluate_rules s'arrete sur une regle bloquante sans action (GROUPE-001)", {
  contexte <- list(min_group_n = 3)
  expect_error(st_evaluate_rules(contexte), "GROUPE-001")
})

test_that("st_evaluate_rules declenche VARIANCE-001 (bloquant + action Welch)", {
  contexte <- list(
    test_type = "comparaison_2_groupes", is_continuous = TRUE, levene_p = 0.001,
    min_group_n = 50, shapiro_p = 0.8
  )
  resultat <- st_evaluate_rules(contexte, family = "preconditions")
  ligne <- resultat[resultat$id == "VARIANCE-001", ]
  expect_equal(ligne$action, "bascule_welch")
})

test_that("st_evaluate_rules declenche CHI2-001 (bloquant + action Fisher)", {
  contexte <- list(test_type = "tableau_croise", effectif_theorique_min = 2)
  resultat <- st_evaluate_rules(contexte, family = "preconditions")
  ligne <- resultat[resultat$id == "CHI2-001", ]
  expect_equal(ligne$action, "bascule_fisher_exact")
  expect_match(ligne$justification, "2")
})

test_that("st_evaluate_rules declenche APPARIEMENT-001 (bloquant sans action) et s'arrete", {
  contexte <- list(apparie = TRUE, n_paires_completes = 8, n_total_declare = 10)
  expect_error(st_evaluate_rules(contexte), "APPARIEMENT-001")
})

test_that("st_evaluate_rules declenche APPARIEMENT-002 (avertissement) sans arreter", {
  contexte <- list(apparie = FALSE, n_paires_completes = 10, n_total_declare = 10)
  resultat <- st_evaluate_rules(contexte, family = "preconditions")
  expect_true("APPARIEMENT-002" %in% resultat$id)
  expect_equal(resultat$severite[resultat$id == "APPARIEMENT-002"], "avertissement")
})

test_that("st_evaluate_rules ignore silencieusement les regles dont le contexte est incomplet", {
  contexte <- list(test_type = "tableau_croise", effectif_theorique_min = 10)
  resultat <- st_evaluate_rules(contexte, family = "preconditions")
  expect_equal(nrow(resultat), 0)
})

test_that("st_evaluate_rules ne retourne aucune regle pour une famille inexistante", {
  repertoire <- .projet_rules()
  contexte <- list(
    test_type = "comparaison_2_groupes", is_continuous = TRUE,
    min_group_n = 50, shapiro_p = 0.01, levene_p = 0.9
  )
  resultat <- st_evaluate_rules(contexte, family = "famille_inexistante")
  expect_equal(nrow(resultat), 0)
})

test_that("st_load_rules charge correctement la famille declaree d'un referentiel personnalise", {
  chemin <- .ecrire_referentiel(c(
    "version: \"1.0.0\"",
    "regles:",
    "  - id: A-001",
    "    famille: autre_famille",
    "    condition: \"valeur > 1\"",
    "    severite: information",
    "    message: \"m\""
  ))
  rules_custom <- st_load_rules(chemin)
  expect_equal(rules_custom[["A-001"]]$famille, "autre_famille")
})

test_that("st_evaluate_rules journalise une derogation quand la regle est forcee via 'override'", {
  repertoire <- .projet_rules()
  contexte <- list(min_group_n = 3)

  resultat <- st_evaluate_rules(contexte, override = "GROUPE-001")
  expect_true(resultat$derogation[resultat$id == "GROUPE-001"])

  journal <- st_log_read(repertoire)
  expect_true("derogation_regle" %in% journal$evenement)
  expect_equal(journal$niveau[journal$evenement == "derogation_regle"], "derogation")
  expect_match(journal$details[journal$evenement == "derogation_regle"], "regle=GROUPE-001")
})

test_that("st_evaluate_rules trie les regles declenchees par severite", {
  chemin <- .ecrire_referentiel(c(
    "version: \"1.0.0\"",
    "regles:",
    "  - id: INFO-001",
    "    famille: preconditions",
    "    condition: \"valeur > 0\"",
    "    severite: information",
    "    message: \"m\"",
    "  - id: WARN-001",
    "    famille: preconditions",
    "    condition: \"valeur > 0\"",
    "    severite: avertissement",
    "    message: \"m\"",
    "  - id: BLOCK-001",
    "    famille: preconditions",
    "    condition: \"valeur > 0\"",
    "    severite: bloquant",
    "    message: \"m\"",
    "    action: corriger",
    "    justification: \"j\""
  ))
  repertoire <- .projet_rules()
  rules <- st_load_rules(chemin)
  expect_equal(names(rules), c("INFO-001", "WARN-001", "BLOCK-001"))
})

test_that("st_evaluate_rules journalise les regles appliquees et signalees", {
  repertoire <- .projet_rules()
  contexte <- list(
    test_type = "comparaison_2_groupes", is_continuous = TRUE,
    min_group_n = 50, shapiro_p = 0.01, levene_p = 0.9
  )
  st_evaluate_rules(contexte, family = "preconditions")

  journal <- st_log_read(repertoire)
  expect_true("regle_signalee" %in% journal$evenement)
})

# Tests : interpolation ---------------------------------------------------------

test_that("l'interpolation formate les valeurs numeriques dans les messages", {
  contexte <- list(test_type = "tableau_croise", effectif_theorique_min = 3.14159)
  resultat <- st_evaluate_rules(contexte, family = "preconditions")
  ligne <- resultat[resultat$id == "CHI2-001", ]
  expect_match(ligne$justification, "3.14")
})

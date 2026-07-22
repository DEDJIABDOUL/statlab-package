# Helpers -----------------------------------------------------------------

.creer_projet_pipeline <- function() {
  repertoire <- tempfile("statlab_pipeline_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  repertoire
}

.ecrire_source_pipeline <- function(repertoire, nom = "inclusion.csv", n_par_groupe = 6) {
  set.seed(1)
  n <- n_par_groupe * 2
  lignes <- c("id;age;groupe;sexe")
  for (i in seq_len(n)) {
    groupe <- if (i <= n_par_groupe) "A" else "B"
    sexe <- if (i %% 2 == 0) "homme" else "femme"
    lignes <- c(lignes, sprintf("P%d;%d;%s;%s", i, 30 + i, groupe, sexe))
  }
  writeLines(lignes, file.path(repertoire, "donnees_brutes", nom))
}

.config_pipeline_simple <- function(repertoire) {
  .ecrire_source_pipeline(repertoire)
  chemin_config <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:",
    "  nom: Etude test pipeline",
    "sources:",
    "  - id: inclusion",
    "    fichier: donnees_brutes/inclusion.csv",
    "dictionnaire:",
    "  age:",
    "    nature: continue",
    "    libelle: Age",
    "  groupe:",
    "    nature: nominale",
    "    libelle: Groupe",
    '    modalites: ["A", "B"]',
    "  sexe:",
    "    nature: binaire",
    "    libelle: Sexe",
    "preparation:",
    "  manquants:",
    "    - strategie: conserver",
    "analyse:",
    "  tableau_1:",
    '    variables: ["age", "sexe"]',
    "  comparaisons:",
    '    - variables: ["age"]',
    "      groupe: groupe",
    "      apparie: false",
    "rendu:",
    '  declinaisons: ["ecran"]'
  ), chemin_config)
  chemin_config
}

.quarto_disponible <- function() nzchar(Sys.which("quarto"))
.latex_disponible <- function() nzchar(Sys.which("pdflatex")) || nzchar(Sys.getenv("TINYTEX_ROOT"))

.dans_repertoire <- function(repertoire, expr) {
  ancien <- getwd()
  setwd(repertoire)
  on.exit(setwd(ancien), add = TRUE)
  eval.parent(substitute(expr))
}

# Tests : helpers purs (.pipeline_*) ---------------------------------------

test_that(".pipeline_mini_config construit un objet statlab_config valide minimal", {
  cfg <- statlab:::.pipeline_mini_config(dictionnaire = list(age = list(nature = "continue")))
  expect_s3_class(cfg, "statlab_config")
  expect_true(attr(cfg, "valid"))
  expect_equal(cfg$dictionnaire$age$nature, "continue")
})

test_that(".pipeline_source_paths resout les chemins absolus des sources", {
  repertoire <- .creer_projet_pipeline()
  .ecrire_source_pipeline(repertoire)
  sources_spec <- list(list(id = "inclusion", fichier = "donnees_brutes/inclusion.csv"))

  chemins <- statlab:::.pipeline_source_paths(sources_spec, repertoire)
  expect_true(file.exists(chemins[1]))
  expect_match(chemins[1], "inclusion.csv$")
})

test_that(".pipeline_run_reconciliation retourne la source unique en l'absence de reconciliation", {
  sources <- list(inclusion = data.frame(id = "1", age = "45"))
  resultat <- statlab:::.pipeline_run_reconciliation(sources, NULL)
  expect_equal(resultat, sources$inclusion)
})

test_that(".pipeline_run_reconciliation s'arrete si plusieurs sources sans reconciliation", {
  sources <- list(a = data.frame(id = "1"), b = data.frame(id = "1"))
  expect_error(statlab:::.pipeline_run_reconciliation(sources, NULL), "reconciliation")
})

test_that(".pipeline_run_reconciliation applique la jointure declaree", {
  st_log_init(.creer_projet_pipeline())
  sources <- list(
    inclusion = data.frame(id = c("1", "2"), age = c("45", "52"), stringsAsFactors = FALSE),
    suivi = data.frame(id = c("1", "2"), visite = c("J8", "J8"), stringsAsFactors = FALSE)
  )
  reconciliation_spec <- list(list(
    operation = "joindre", gauche = "inclusion", droite = "suivi",
    cle = "id", resultat = "donnees_completes",
    type = "gauche", normaliser_cle = TRUE, alerte_explosion = TRUE
  ))
  resultat <- statlab:::.pipeline_run_reconciliation(sources, reconciliation_spec)
  expect_true("visite" %in% names(resultat))
  expect_equal(nrow(resultat), 2)
})

test_that(".pipeline_run_table1 retourne NULL si aucun tableau_1 n'est declare", {
  expect_null(statlab:::.pipeline_run_table1(data.frame(age = 1), list(), NULL, NULL))
})

test_that(".pipeline_run_table1 produit un objet gtsummary quand declare", {
  donnees <- data.frame(age = c("45", "52", "38", "60"), sexe = c("homme", "femme", "femme", "homme"), stringsAsFactors = FALSE)
  dictionnaire <- list(age = list(nature = "continue", libelle = "Age"), sexe = list(nature = "binaire", libelle = "Sexe"))
  tbl <- statlab:::.pipeline_run_table1(donnees, dictionnaire, list(variables = c("age", "sexe")), NULL)
  expect_s3_class(tbl, "gtsummary")
})

test_that(".pipeline_run_comparaisons retourne une liste vide sans comparaisons declarees", {
  expect_equal(statlab:::.pipeline_run_comparaisons(data.frame(age = 1), list(), NULL), list())
})

test_that(".pipeline_run_comparaisons execute st_compare() pour chaque variable declaree", {
  set.seed(2)
  donnees <- data.frame(
    age = c(stats::rnorm(10, 40, 5), stats::rnorm(10, 45, 5)),
    groupe = rep(c("A", "B"), each = 10)
  )
  dictionnaire <- list(age = list(nature = "continue"), groupe = list(nature = "nominale"))
  comparisons_spec <- list(list(variables = "age", groupe = "groupe", apparie = FALSE))

  resultats <- statlab:::.pipeline_run_comparaisons(donnees, dictionnaire, comparisons_spec)
  expect_length(resultats, 1)
  expect_equal(resultats[[1]]$variable, "age")
  expect_s3_class(resultats[[1]]$result, "statlab_comparison")
})

test_that(".pipeline_run_graphiques produit un fichier par comparaison", {
  set.seed(3)
  donnees <- data.frame(
    age = c(stats::rnorm(10, 40, 5), stats::rnorm(10, 45, 5)),
    groupe = rep(c("A", "B"), each = 10)
  )
  dictionnaire <- list(age = list(nature = "continue", libelle = "Age"), groupe = list(nature = "nominale", libelle = "Groupe"))
  comparisons_spec <- list(list(variables = "age", groupe = "groupe", apparie = FALSE))
  comparaisons <- statlab:::.pipeline_run_comparaisons(donnees, dictionnaire, comparisons_spec)

  repertoire <- .creer_projet_pipeline()
  chemins <- statlab:::.pipeline_run_graphiques(donnees, comparaisons, dictionnaire, repertoire, "ecran")

  expect_length(chemins, 1)
  expect_true(file.exists(chemins))
})

test_that(".pipeline_run_graphiques retourne un vecteur vide sans comparaison", {
  expect_equal(statlab:::.pipeline_run_graphiques(data.frame(), list(), list(), tempdir(), "ecran"), character(0))
})

# Tests : preparation du plan -----------------------------------------------

test_that(".pipeline_prepare copie le gabarit avec le chemin de config substitue", {
  repertoire <- .creer_projet_pipeline()
  chemin_config <- .config_pipeline_simple(repertoire)

  project_dir <- statlab:::.pipeline_prepare(chemin_config)
  expect_equal(normalizePath(project_dir, winslash = "/"), normalizePath(repertoire, winslash = "/"))

  chemin_plan <- file.path(project_dir, "_targets.R")
  expect_true(file.exists(chemin_plan))
  contenu <- paste(readLines(chemin_plan), collapse = "\n")
  expect_false(grepl("__CONFIG_PATH__", contenu, fixed = TRUE))
  expect_match(contenu, normalizePath(chemin_config, winslash = "/"), fixed = TRUE)
})

# Tests : erreurs -------------------------------------------------------------

test_that("st_pipeline s'arrete si le fichier config n'existe pas", {
  expect_error(st_pipeline(tempfile(fileext = ".yml")), "introuvable")
})

test_that("st_status s'arrete si le fichier config n'existe pas", {
  expect_error(st_status(tempfile(fileext = ".yml")), "introuvable")
})

# Tests : invalidation fine, sans rendu Quarto (execution partielle du plan) --

.NOMS_SOUS_GRAPHE <- c("comparaisons", "anomalies")

test_that("le sous-graphe de traitement des donnees se calcule correctement et se met en cache", {
  repertoire <- .creer_projet_pipeline()
  chemin_config <- .config_pipeline_simple(repertoire)
  statlab:::.pipeline_prepare(chemin_config)
  st_log_init(repertoire)

  .dans_repertoire(repertoire, {
    targets::tar_make(names = tidyselect::all_of(.NOMS_SOUS_GRAPHE), callr_function = NULL, reporter = "silent")
    progres1 <- targets::tar_progress()
    executes1 <- progres1$name[progres1$progress %in% c("completed", "built")]

    expect_true(all(c("sources", "profil", "anomalies", "donnees_finales", "comparaisons") %in% executes1))

    # Deuxieme appel, rien n'a change : tout doit etre saute.
    targets::tar_make(names = tidyselect::all_of(.NOMS_SOUS_GRAPHE), callr_function = NULL, reporter = "silent")
    progres2 <- targets::tar_progress()
    executes2 <- progres2$name[progres2$progress %in% c("completed", "built")]
    expect_length(executes2, 0)
  })
})

test_that("modifier analyse.comparaisons ne recalcule pas sources/profil/anomalies", {
  repertoire <- .creer_projet_pipeline()
  chemin_config <- .config_pipeline_simple(repertoire)
  statlab:::.pipeline_prepare(chemin_config)
  st_log_init(repertoire)

  .dans_repertoire(repertoire, {
    targets::tar_make(names = tidyselect::all_of(.NOMS_SOUS_GRAPHE), callr_function = NULL, reporter = "silent")

    # Modifier uniquement 'analyse' (ajouter une comparaison sur 'sexe'),
    # inseree avant la section 'rendu:' pour rester dans le bloc 'analyse'.
    lignes <- readLines(chemin_config)
    idx_rendu <- which(lignes == "rendu:")
    nouvelle_comparaison <- c('    - variables: ["sexe"]', "      groupe: groupe", "      apparie: false")
    lignes <- append(lignes, nouvelle_comparaison, after = idx_rendu - 1)
    writeLines(lignes, chemin_config)
    statlab:::.pipeline_prepare(chemin_config)

    targets::tar_make(names = tidyselect::all_of(.NOMS_SOUS_GRAPHE), callr_function = NULL, reporter = "silent")
    progres <- targets::tar_progress()
    executes <- progres$name[progres$progress %in% c("completed", "built")]

    expect_true("comparaisons" %in% executes)
    expect_true("analyse_spec" %in% executes)
    expect_false("sources" %in% executes)
    expect_false("profil" %in% executes)
    expect_false("anomalies" %in% executes)
    expect_false("donnees_finales" %in% executes)
  })
})

test_that("modifier le fichier source recalcule l'ensemble du sous-graphe de donnees", {
  repertoire <- .creer_projet_pipeline()
  chemin_config <- .config_pipeline_simple(repertoire)
  statlab:::.pipeline_prepare(chemin_config)
  st_log_init(repertoire)

  .dans_repertoire(repertoire, {
    targets::tar_make(names = tidyselect::all_of(.NOMS_SOUS_GRAPHE), callr_function = NULL, reporter = "silent")

    Sys.sleep(0.1)
    .ecrire_source_pipeline(repertoire, "inclusion.csv", n_par_groupe = 8)

    targets::tar_make(names = tidyselect::all_of(.NOMS_SOUS_GRAPHE), callr_function = NULL, reporter = "silent")
    progres <- targets::tar_progress()
    executes <- progres$name[progres$progress %in% c("completed", "built")]

    expect_true(all(c("source_files", "sources", "profil", "anomalies", "donnees_finales", "comparaisons") %in% executes))
  })
})

# Test d'integration complet (necessite Quarto + LaTeX) -----------------------

test_that("st_pipeline execute le plan complet et st_status reflete l'etat final", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")
  testthat::skip_if_not(.latex_disponible(), "distribution LaTeX non disponible")

  repertoire <- .creer_projet_pipeline()
  chemin_config <- .config_pipeline_simple(repertoire)

  resultat <- st_pipeline(chemin_config)
  expect_true(nrow(resultat) > 0)
  expect_true(file.exists(file.path(repertoire, "sorties", "audit", "audit.html")))
  expect_true(file.exists(file.path(repertoire, "sorties", "rapport", "rapport.pdf")))
  expect_true(file.exists(file.path(repertoire, "sorties", "rapport", "analyse.R")))
  expect_true(file.exists(file.path(repertoire, "sorties", "rapport", "attestation.txt")))

  statut <- st_status(chemin_config)
  expect_true(all(statut$etat == "a_jour"))

  resultat2 <- st_pipeline(chemin_config)
  expect_equal(nrow(resultat2), 0)
})

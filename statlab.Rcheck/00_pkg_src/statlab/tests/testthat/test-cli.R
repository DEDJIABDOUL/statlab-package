# Helpers -----------------------------------------------------------------

.creer_projet_cli <- function() {
  repertoire <- tempfile("statlab_cli_")
  dir.create(repertoire)
  dir.create(file.path(repertoire, "donnees_brutes"))
  repertoire
}

.ecrire_source_cli <- function(repertoire, nom = "inclusion.csv") {
  writeLines(c(
    "id;age;groupe;sexe",
    "P1;45;A;homme",
    "P2;52;B;femme",
    "P3;38;A;femme",
    "P4;60;B;homme",
    "P5;41;A;homme",
    "P6;55;B;femme",
    "P7;33;A;femme",
    "P8;48;B;homme",
    "P9;39;A;femme",
    "P10;58;B;homme",
    "P11;44;A;homme",
    "P12;51;B;femme"
  ), file.path(repertoire, "donnees_brutes", nom))
}

.config_cli_simple <- function(repertoire, nom_fichier = "config.yml") {
  .ecrire_source_cli(repertoire)
  chemin_config <- file.path(repertoire, nom_fichier)
  writeLines(c(
    "projet:",
    "  nom: Etude test CLI",
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
    "      apparie: false"
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

# cli::cli_alert_*()/cli_bullets()/cli_h1() emettent via message() (pas
# stdout), alors que optparse::print_help() et print() ecrivent sur stdout :
# capturer les deux sources dans UN seul tampon (ordre chronologique
# preserve) est necessaire pour verifier le texte affiche a l'utilisateur.
.capturer_sortie <- function(expr) {
  buffer_lignes <- NULL
  conn <- textConnection("buffer_lignes", open = "w", local = TRUE)
  sink(conn)
  sink(conn, type = "message")
  statut <- tryCatch(
    expr,
    finally = {
      sink(type = "message")
      sink()
    }
  )
  close(conn)
  list(statut = statut, sortie = paste(buffer_lignes, collapse = "\n"))
}

# Tests : dispatch et gestion des erreurs --------------------------------------

test_that(".cli_main affiche l'usage et retourne 0 sans argument", {
  r <- .capturer_sortie(statlab:::.cli_main(character(0)))
  expect_equal(r$statut, 0L)
  expect_match(r$sortie, "statlab")
  expect_match(r$sortie, "auditer")
})

test_that(".cli_main affiche l'usage et retourne 0 avec --help", {
  r <- .capturer_sortie(statlab:::.cli_main(c("--help")))
  expect_equal(r$statut, 0L)
  expect_match(r$sortie, "Commandes")
})

test_that(".cli_main retourne 1 et un message actionnable pour une commande inconnue", {
  r <- .capturer_sortie(statlab:::.cli_main(c("bogus")))
  expect_equal(r$statut, 1L)
  expect_match(r$sortie, "inconnue")
  expect_match(r$sortie, "auditer, config, analyser, rapporter, valider, regles")
})

test_that(".cli_main retourne 1 et un message actionnable (sans pile d'appels) en cas d'erreur", {
  repertoire <- .creer_projet_cli()
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("valider")))
    expect_equal(r$statut, 1L)
    expect_match(r$sortie, "introuvable")
    expect_false(grepl("Backtrace", r$sortie))
    expect_match(r$sortie, "verbose")
  })
})

test_that(".cli_main affiche la trace complete avec --verbose", {
  repertoire <- .creer_projet_cli()
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("valider", "--verbose")))
    expect_equal(r$statut, 1L)
    expect_match(r$sortie, "Trace complete")
  })
})

# Tests : --help sur chaque commande (en francais, sans quitter le processus) --

test_that("chaque commande affiche une aide en francais et retourne 0", {
  for (commande in c("auditer", "config", "analyser", "rapporter", "valider", "regles")) {
    r <- .capturer_sortie(statlab:::.cli_main(c(commande, "--help")))
    expect_equal(r$statut, 0L, info = commande)
    expect_match(r$sortie, "Usage", info = commande)
    expect_false(grepl("Show this help message", r$sortie), info = commande)
  }
})

# Tests : commande 'valider' ---------------------------------------------------

test_that("statlab valider retourne 0 pour une configuration valide", {
  repertoire <- .creer_projet_cli()
  chemin <- .config_cli_simple(repertoire)
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("valider", sprintf("--config=%s", chemin))))
    expect_equal(r$statut, 0L)
    expect_match(r$sortie, "valide")
  })
})

test_that("statlab valider utilise config.yml du repertoire courant par defaut", {
  repertoire <- .creer_projet_cli()
  .config_cli_simple(repertoire)
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("valider", "--quiet")))
    expect_equal(r$statut, 0L)
  })
})

# Tests : commande 'config' ------------------------------------------------------

test_that("statlab config genere un config.yml valide a partir de fichiers sources", {
  repertoire <- .creer_projet_cli()
  .ecrire_source_cli(repertoire)
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("config", "--sources=donnees_brutes/inclusion.csv", "--sortie=genere.yml")))
    expect_equal(r$statut, 0L)
    expect_true(file.exists("genere.yml"))
    expect_s3_class(st_validate_config(st_read_config("genere.yml")), "statlab_config")
  })
})

test_that("statlab config s'arrete si --sources est absent", {
  repertoire <- .creer_projet_cli()
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("config")))
    expect_equal(r$statut, 1L)
    expect_match(r$sortie, "--sources")
  })
})

test_that("statlab config refuse d'ecraser sans --forcer", {
  repertoire <- .creer_projet_cli()
  .ecrire_source_cli(repertoire)
  .dans_repertoire(repertoire, {
    .capturer_sortie(statlab:::.cli_main(c("config", "--sources=donnees_brutes/inclusion.csv", "--sortie=genere.yml", "--quiet")))
    r <- .capturer_sortie(statlab:::.cli_main(c("config", "--sources=donnees_brutes/inclusion.csv", "--sortie=genere.yml")))
    expect_equal(r$statut, 1L)
  })
})

# Tests : commande 'analyser' -----------------------------------------------------

test_that("statlab analyser execute la chaine et affiche un resume", {
  repertoire <- .creer_projet_cli()
  chemin <- .config_cli_simple(repertoire)
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("analyser", sprintf("--config=%s", chemin))))
    expect_equal(r$statut, 0L)
    expect_match(r$sortie, "Resume de l'analyse")
    expect_match(r$sortie, "Tableau 1")
    expect_match(r$sortie, "Comparaisons")
  })
})

test_that("statlab analyser --quiet n'affiche que l'essentiel", {
  repertoire <- .creer_projet_cli()
  chemin <- .config_cli_simple(repertoire)
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("analyser", sprintf("--config=%s", chemin), "--quiet")))
    expect_equal(r$statut, 0L)
    expect_false(grepl("Resume de l'analyse", r$sortie))
  })
})

test_that("statlab analyser s'arrete si plusieurs sources sont declarees sans reconciliation", {
  repertoire <- .creer_projet_cli()
  .ecrire_source_cli(repertoire)
  .ecrire_source_cli(repertoire, "suivi.csv")
  chemin <- file.path(repertoire, "config.yml")
  writeLines(c(
    "projet:", "  nom: Etude test CLI", "sources:",
    "  - id: inclusion", "    fichier: donnees_brutes/inclusion.csv",
    "  - id: suivi", "    fichier: donnees_brutes/suivi.csv"
  ), chemin)
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c("analyser", sprintf("--config=%s", chemin))))
    expect_equal(r$statut, 1L)
    expect_match(r$sortie, "reconciliation")
  })
})

# Tests : commande 'regles' -------------------------------------------------------

test_that("statlab regles affiche le referentiel et retourne 0", {
  r <- .capturer_sortie(statlab:::.cli_main(c("regles")))
  expect_equal(r$statut, 0L)
  expect_match(r$sortie, "Referentiel methodologique")
})

# Tests : commande 'auditer' (necessite Quarto) -----------------------------------

test_that("statlab auditer produit un rapport a partir d'un config.yml", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")

  repertoire <- .creer_projet_cli()
  chemin <- .config_cli_simple(repertoire)
  .dans_repertoire(repertoire, {
    statut <- statlab:::.cli_main(c("auditer", sprintf("--config=%s", chemin), "--sortie=sorties/audit"))
    expect_equal(statut, 0L)
    expect_true(file.exists("sorties/audit/audit.html"))
  })
})

test_that("statlab auditer accepte --sources sans config.yml prealable", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")

  repertoire <- .creer_projet_cli()
  .ecrire_source_cli(repertoire)
  .dans_repertoire(repertoire, {
    statut <- statlab:::.cli_main(c("auditer", "--sources=donnees_brutes/inclusion.csv", "--sortie=sorties/audit"))
    expect_equal(statut, 0L)
    expect_true(file.exists("sorties/audit/audit.html"))
  })
})

test_that("statlab auditer refuse --config et --sources simultanement", {
  repertoire <- .creer_projet_cli()
  chemin <- .config_cli_simple(repertoire)
  .dans_repertoire(repertoire, {
    r <- .capturer_sortie(statlab:::.cli_main(c(
      "auditer", sprintf("--config=%s", chemin), "--sources=donnees_brutes/inclusion.csv"
    )))
    expect_equal(r$statut, 1L)
    expect_match(r$sortie, "mutuellement exclusives")
  })
})

# Tests : commande 'rapporter' (necessite Quarto + LaTeX) --------------------------

test_that("statlab rapporter produit le rapport pdf a partir d'un config.yml", {
  testthat::skip_if_not(.quarto_disponible(), "quarto CLI non disponible")
  testthat::skip_if_not(.latex_disponible(), "distribution LaTeX non disponible")

  repertoire <- .creer_projet_cli()
  chemin <- .config_cli_simple(repertoire)
  .dans_repertoire(repertoire, {
    statut <- statlab:::.cli_main(c("rapporter", sprintf("--config=%s", chemin), "--formats=pdf", "--sortie=sorties/rapport"))
    expect_equal(statut, 0L)
    expect_true(file.exists("sorties/rapport/rapport.pdf"))
  })
})

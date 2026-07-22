# =============================================================================
# Commandes operationnelles : point d'entree de la ligne de commande
# 'statlab' (inst/bin/statlab). Ce fichier ne definit que des fonctions
# INTERNES (non exportees, pas de prefixe st_) : il ne s'agit pas de
# fonctions d'analyse statistique mais d'un habillage CLI autour d'elles.
#
# Convention d'echec : chaque commande retourne un entier (0 = succes, non
# nul = echec) plutot que d'appeler quit() elle-meme, pour rester testable
# depuis testthat. Seul inst/bin/statlab appelle quit(status = ...).
#
# Attention (optparse) : par defaut, optparse::parse_args() imprime l'aide
# ET appelle quit() lui-meme des qu'un argument "--help"/"-h" est present
# (comportement documente du package, et l'option d'aide integree n'est
# qu'en anglais). Chaque parser ci-dessous desactive donc l'aide integree
# (add_help_option = FALSE) au profit d'une option "-h"/"--help" maison (en
# francais), et .cli_parse() appelle parse_args(print_help_and_exit = FALSE)
# pour piloter l'affichage et le code de sortie nous-memes.
# =============================================================================

.cli_default_config_path <- function() "config.yml"

.cli_default <- function(x, default) if (is.null(x)) default else x

.cli_common_options <- function() {
  list(
    optparse::make_option(c("-q", "--quiet"), action = "store_true", default = FALSE, help = "N'affiche que les erreurs et le resultat final"),
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE, help = "Affiche le detail complet de l'execution (journal, trace en cas d'erreur)"),
    optparse::make_option(c("-h", "--help"), action = "store_true", default = FALSE, help = "Affiche ce message d'aide et quitte")
  )
}

.cli_make_parser <- function(usage, description, option_list = list()) {
  optparse::OptionParser(
    usage = usage, description = description, add_help_option = FALSE,
    option_list = c(option_list, .cli_common_options())
  )
}

# Retourne les options analysees, ou NULL si "--help" a ete demande (dans ce
# cas, l'aide a deja ete affichee : l'appelant doit alors retourner 0L).
.cli_parse <- function(parser, rest) {
  opts <- optparse::parse_args(parser, args = rest, print_help_and_exit = FALSE)
  if (isTRUE(opts$help)) {
    optparse::print_help(parser)
    return(NULL)
  }
  opts
}

.cli_configure_verbosity <- function(opts) {
  if (isTRUE(opts$quiet)) {
    options(statlab.quiet = TRUE)
    return("quiet")
  }
  if (isTRUE(opts$verbose)) {
    options(statlab.quiet = FALSE)
    return("verbose")
  }
  options(statlab.quiet = TRUE)
  "normal"
}

.cli_step <- function(mode, text) {
  if (identical(mode, "quiet")) {
    return(invisible(NULL))
  }
  # cli_progress_step() gere sa propre animation terminal (reecriture de
  # ligne), incompatible avec la redirection standard de sortie/erreur :
  # sous testthat (TESTTHAT=true, convention documentee du package), on
  # degrade vers une simple ligne cli_alert_info(), capturable normalement.
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    cli::cli_alert_info(text)
  } else {
    cli::cli_progress_step(text)
  }
}

.cli_assert_config_exists <- function(config_path) {
  if (!file.exists(config_path)) {
    cli::cli_abort(c(
      "Fichier de configuration introuvable : {.path {config_path}}",
      "i" = "Executer {.code statlab config --sources=...} pour en generer un, ou preciser {.arg --config=chemin/vers/config.yml}."
    ))
  }
}

.cli_split_list <- function(x) {
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

# --- Point d'entree principal -------------------------------------------------

#' @keywords internal
.cli_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  checkmate::assert_character(args, any.missing = FALSE)

  if (length(args) == 0 || args[1] %in% c("-h", "--help")) {
    .cli_print_usage()
    return(0L)
  }

  command <- args[1]
  rest <- args[-1]

  handler <- switch(command,
    auditer = .cli_cmd_auditer,
    config = .cli_cmd_config,
    analyser = .cli_cmd_analyser,
    rapporter = .cli_cmd_rapporter,
    valider = .cli_cmd_valider,
    regles = .cli_cmd_regles,
    NULL
  )

  if (is.null(handler)) {
    cli::cli_alert_danger("Commande inconnue : '{command}'")
    cli::cli_bullets(c("i" = "Commandes disponibles : auditer, config, analyser, rapporter, valider, regles", "i" = "Executer 'statlab --help' pour le detail."))
    return(1L)
  }

  tryCatch(
    {
      statut <- handler(rest)
      cli::cli_progress_done()
      statut
    },
    error = function(e) {
      cli::cli_progress_done(result = "failed")
      cli::cli_alert_danger(conditionMessage(e))
      if ("--verbose" %in% rest || "-v" %in% rest) {
        cli::cli_h3("Trace complete (--verbose)")
        print(e)
      } else {
        cli::cli_bullets(c("i" = "Relancer avec --verbose pour la trace complete."))
      }
      1L
    }
  )
}

.cli_print_usage <- function() {
  cli::cli_h1("statlab -- chaine d'analyse statistique locale")
  cli::cli_text("Usage : statlab <commande> [options]")
  cli::cli_h2("Commandes")
  cli::cli_bullets(c(
    "*" = "{.strong auditer}   [--config=chemin | --sources=f1,f2] [--sortie=dir]  — audit des donnees brutes",
    "*" = "{.strong config}    --sources=f1,f2 [--sortie=config.yml] [--forcer]     — genere un config.yml",
    "*" = "{.strong analyser}  [--config=config.yml]                               — execute l'analyse (resume console)",
    "*" = "{.strong rapporter} [--config=config.yml] [--formats=docx,pdf]          — genere le rapport complet",
    "*" = "{.strong valider}   [--config=config.yml]                               — valide la configuration seule",
    "*" = "{.strong regles}                                                       — affiche le referentiel methodologique"
  ))
  cli::cli_text("")
  cli::cli_text("Options communes a toutes les commandes : --quiet/-q, --verbose/-v, --help/-h")
}

# --- Commande : auditer --------------------------------------------------------

.cli_cmd_auditer <- function(rest) {
  parser <- .cli_make_parser(
    usage = "statlab auditer [options]",
    description = "Genere le rapport d'audit des donnees brutes (avant toute preparation).",
    option_list = list(
      optparse::make_option("--config", type = "character", default = NULL, help = "Chemin vers config.yml [defaut : config.yml]"),
      optparse::make_option("--sources", type = "character", default = NULL, help = "Fichiers sources separes par des virgules (alternative a --config : un config.yml minimal est genere automatiquement)"),
      optparse::make_option("--sortie", type = "character", default = "sorties/audit", help = "Repertoire de sortie [defaut : %default]")
    )
  )
  opts <- .cli_parse(parser, rest)
  if (is.null(opts)) return(0L)
  mode <- .cli_configure_verbosity(opts)

  if (!is.null(opts$config) && !is.null(opts$sources)) {
    cli::cli_abort(c(
      "Les options {.arg --config} et {.arg --sources} sont mutuellement exclusives.",
      "i" = "Utiliser {.arg --config} pour un projet existant, ou {.arg --sources} pour auditer des fichiers bruts directement."
    ))
  }

  if (!is.null(opts$sources)) {
    fichiers <- .cli_split_list(opts$sources)
    .cli_step(mode, sprintf("Generation d'une configuration temporaire pour %d fichier(s)", length(fichiers)))
    config_path <- st_scaffold_config(fichiers, output = tempfile(fileext = ".yml"), overwrite = TRUE)
  } else {
    config_path <- .cli_default(opts$config, .cli_default_config_path())
  }
  .cli_assert_config_exists(config_path)

  .cli_step(mode, sprintf("Audit des donnees (%s)", config_path))
  chemins <- st_audit(config_path, output_dir = opts$sortie)
  if (!identical(mode, "quiet")) {
    cli::cli_alert_success("Rapport d'audit genere : {paste(chemins, collapse = ', ')}")
  }
  0L
}

# --- Commande : config ----------------------------------------------------------

.cli_cmd_config <- function(rest) {
  parser <- .cli_make_parser(
    usage = "statlab config --sources=f1,f2 [options]",
    description = "Genere un config.yml pre-rempli a partir de fichiers de donnees bruts (profilage automatique).",
    option_list = list(
      optparse::make_option("--sources", type = "character", default = NULL, help = "Fichiers sources separes par des virgules (requis)"),
      optparse::make_option("--sortie", type = "character", default = "config.yml", help = "Chemin du config.yml a generer [defaut : %default]"),
      optparse::make_option("--forcer", action = "store_true", default = FALSE, help = "Ecrase le fichier de sortie s'il existe deja")
    )
  )
  opts <- .cli_parse(parser, rest)
  if (is.null(opts)) return(0L)
  mode <- .cli_configure_verbosity(opts)

  if (is.null(opts$sources)) {
    cli::cli_abort(c(
      "L'option {.arg --sources} est requise pour la commande 'config'.",
      "i" = "Exemple : statlab config --sources=donnees_brutes/inclusion.xlsx,donnees_brutes/suivi.csv"
    ))
  }
  fichiers <- .cli_split_list(opts$sources)

  .cli_step(mode, sprintf("Profilage de %d fichier(s) source(s)", length(fichiers)))
  chemin <- st_scaffold_config(fichiers, output = opts$sortie, overwrite = isTRUE(opts$forcer))
  if (!identical(mode, "quiet")) {
    cli::cli_alert_success("Configuration generee : {.path {chemin}}")
  }
  0L
}

# --- Commande : analyser ---------------------------------------------------------

.cli_cmd_analyser <- function(rest) {
  parser <- .cli_make_parser(
    usage = "statlab analyser [options]",
    description = "Execute la chaine d'analyse (lecture, reconciliation, preparation, tableau 1, comparaisons) et affiche un resume dans le terminal, sans generer de rapport.",
    option_list = list(
      optparse::make_option("--config", type = "character", default = NULL, help = "Chemin vers config.yml [defaut : config.yml]")
    )
  )
  opts <- .cli_parse(parser, rest)
  if (is.null(opts)) return(0L)
  mode <- .cli_configure_verbosity(opts)
  config_path <- .cli_default(opts$config, .cli_default_config_path())
  .cli_assert_config_exists(config_path)

  .cli_step(mode, "Validation de la configuration")
  config <- st_validate_config(st_read_config(config_path))
  project_dir <- attr(config, "project_dir")
  st_log_init(project_dir)

  .cli_step(mode, "Lecture des sources")
  sources_data <- st_read_all_sources(config)

  if (!is.null(config$reconciliation)) {
    .cli_step(mode, "Reconciliation des sources")
    working_data <- st_reconcile(sources_data, config)$table
  } else {
    if (length(sources_data) != 1) {
      cli::cli_abort(c(
        "Plusieurs sources sont declarees sans operation de reconciliation.",
        "i" = "Declarer une section 'reconciliation' dans config.yml pour indiquer comment les assembler."
      ))
    }
    working_data <- sources_data[[1]]
  }

  .cli_step(mode, "Preparation des donnees")
  prepared_data <- st_prepare(working_data, config)

  if (!identical(mode, "quiet")) {
    cli::cli_h1("Resume de l'analyse")
    cli::cli_bullets(c("*" = "Effectif final analyse : {nrow(prepared_data)}"))
  }

  if (!is.null(config$analyse$tableau_1)) {
    .cli_step(mode, "Construction du tableau 1")
    table1_csv <- st_table1_csv(st_table1(prepared_data, config))
    if (!identical(mode, "quiet")) {
      cli::cli_h2("Tableau 1")
      print(table1_csv)
    }
  }

  if (!is.null(config$analyse$comparaisons)) {
    .cli_step(mode, "Comparaisons statistiques")
    if (!identical(mode, "quiet")) {
      cli::cli_h2("Comparaisons")
    }
    for (entry in config$analyse$comparaisons) {
      for (variable in entry$variables) {
        resultat <- st_compare(prepared_data, variable, entry$groupe, config, paired = isTRUE(entry$apparie))
        if (!identical(mode, "quiet")) {
          cli::cli_bullets(c("*" = "{variable} selon {entry$groupe} : {resultat$test_name}, p = {format(resultat$p_value, digits = 3)}"))
        }
      }
    }
  }

  0L
}

# --- Commande : rapporter ---------------------------------------------------------

.cli_cmd_rapporter <- function(rest) {
  parser <- .cli_make_parser(
    usage = "statlab rapporter [options]",
    description = "Genere le rapport d'analyse complet (docx et/ou pdf), avec ses annexes.",
    option_list = list(
      optparse::make_option("--config", type = "character", default = NULL, help = "Chemin vers config.yml [defaut : config.yml]"),
      optparse::make_option("--formats", type = "character", default = "docx,pdf", help = "Formats separes par des virgules, parmi docx, pdf [defaut : %default]"),
      optparse::make_option("--sortie", type = "character", default = "sorties/rapport", help = "Repertoire de sortie [defaut : %default]")
    )
  )
  opts <- .cli_parse(parser, rest)
  if (is.null(opts)) return(0L)
  mode <- .cli_configure_verbosity(opts)
  config_path <- .cli_default(opts$config, .cli_default_config_path())
  .cli_assert_config_exists(config_path)
  formats <- .cli_split_list(opts$formats)

  .cli_step(mode, sprintf("Generation du rapport (%s)", paste(formats, collapse = ", ")))
  chemins <- st_report(config_path, output_dir = opts$sortie, formats = formats)
  if (!identical(mode, "quiet")) {
    cli::cli_alert_success("Rapport genere : {paste(chemins, collapse = ', ')}")
  }
  0L
}

# --- Commande : valider ---------------------------------------------------------

.cli_cmd_valider <- function(rest) {
  parser <- .cli_make_parser(
    usage = "statlab valider [options]",
    description = "Valide la configuration (config.yml), sans executer l'analyse.",
    option_list = list(
      optparse::make_option("--config", type = "character", default = NULL, help = "Chemin vers config.yml [defaut : config.yml]")
    )
  )
  opts <- .cli_parse(parser, rest)
  if (is.null(opts)) return(0L)
  mode <- .cli_configure_verbosity(opts)
  config_path <- .cli_default(opts$config, .cli_default_config_path())
  .cli_assert_config_exists(config_path)

  .cli_step(mode, "Validation de la configuration")
  config <- st_validate_config(st_read_config(config_path))
  if (!identical(mode, "quiet")) {
    cli::cli_alert_success("Configuration valide : {.path {config_path}}")
    print(config)
  }
  0L
}

# --- Commande : regles ---------------------------------------------------------

.cli_cmd_regles <- function(rest) {
  parser <- .cli_make_parser(
    usage = "statlab regles [options]",
    description = "Affiche le referentiel methodologique (regles, seuils, justifications)."
  )
  opts <- .cli_parse(parser, rest)
  if (is.null(opts)) return(0L)
  .cli_configure_verbosity(opts)

  rules <- st_load_rules()
  table_regles <- st_rules_report()
  cli::cli_h1(sprintf("Referentiel methodologique (version %s)", attr(rules, "version")))
  print(table_regles)
  0L
}

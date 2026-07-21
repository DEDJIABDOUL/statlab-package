# =============================================================================
# Attestation de reproductibilite : document de synthese, a la racine du
# projet (aux cotes de config.yml et journal.log), qui certifie ce qu'il
# faut pour rejouer une analyse a l'identique : le fichier de
# configuration exact (empreinte SHA-256), les fichiers sources exacts
# (empreintes SHA-256), les versions des packages utilises, la version du
# referentiel methodologique, et toute derogation operateur consignee au
# journal (car une derogation est, par definition, une decision humaine
# qui echappe au moteur deterministe et doit donc etre rejouee a
# l'identique pour reproduire le meme resultat).
# =============================================================================

#' Generer l'attestation de reproductibilite d'un projet
#'
#' Produit un document texte certifiant les elements necessaires a la
#' reproduction a l'identique de l'analyse : empreinte du fichier
#' `config.yml`, empreintes des fichiers sources declares, versions des
#' packages R utilises, version du referentiel methodologique
#' ([st_load_rules()]), et toute derogation operateur consignee dans
#' `journal.log` (cf. [st_evaluate_rules()]).
#'
#' @param config_path Chemin (chr) vers le fichier `config.yml`.
#' @param output Chemin (chr) du fichier a produire. Si relatif, resolu
#'   par rapport au repertoire du projet (celui de `config.yml`).
#'   Par defaut, `"attestation.txt"` a la racine du projet.
#'
#' @return Le chemin absolu (chr) du fichier genere, de maniere invisible.
#' @export
st_attestation <- function(config_path, output = "attestation.txt") {
  checkmate::assert_string(config_path, min.chars = 1)
  checkmate::assert_string(output, min.chars = 1)

  config <- st_validate_config(st_read_config(config_path))
  project_dir <- attr(config, "project_dir")
  st_log_init(project_dir)

  output_path <- if (.is_absolute_path(output)) output else file.path(project_dir, output)
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    cli::cli_abort("Le repertoire de destination est introuvable : {.path {output_dir}}")
  }

  sources_info <- lapply(config$sources, function(entry) {
    file_path <- file.path(project_dir, entry$fichier)
    if (!file.exists(file_path)) {
      cli::cli_abort("Fichier source introuvable pour l'attestation : {.path {file_path}}")
    }
    list(id = entry$id, fichier = entry$fichier, hash = digest::digest(file = file_path, algo = "sha256"))
  })

  rules <- st_load_rules()
  derogations <- .attestation_collect_derogations(project_dir)
  event_counts <- .attestation_event_counts(project_dir)

  lines <- .build_attestation_lines(config, sources_info, attr(rules, "version"), derogations, event_counts)
  writeLines(lines, output_path, useBytes = TRUE)

  absolute_output <- normalizePath(output_path, winslash = "/", mustWork = TRUE)
  st_log(
    "attestation",
    module = "attestation", fichier = absolute_output,
    n_sources = length(sources_info), n_derogations = nrow(derogations),
    level = "info"
  )

  cli::cli_alert_success("Attestation de reproductibilite generee : {.path {absolute_output}}")
  invisible(absolute_output)
}

# --- Collecte des informations ------------------------------------------------

.attestation_collect_derogations <- function(project_dir) {
  empty <- data.frame(horodatage = character(0), evenement = character(0), details = character(0), stringsAsFactors = FALSE)
  log_path <- file.path(project_dir, "journal.log")
  if (!file.exists(log_path)) {
    return(empty)
  }
  journal <- st_log_read(project_dir)
  if (nrow(journal) == 0) {
    return(empty)
  }
  journal[journal$niveau == "derogation", c("horodatage", "evenement", "details"), drop = FALSE]
}

.attestation_event_counts <- function(project_dir) {
  log_path <- file.path(project_dir, "journal.log")
  if (!file.exists(log_path)) {
    return(character(0))
  }
  journal <- st_log_read(project_dir)
  if (nrow(journal) == 0) {
    return(character(0))
  }
  counts <- sort(table(journal$evenement), decreasing = TRUE)
  stats::setNames(as.integer(counts), names(counts))
}

.attestation_package_versions <- function() {
  description <- utils::packageDescription("statlab")
  imports_field <- description$Imports
  if (is.null(imports_field) || is.na(imports_field)) {
    return(character(0))
  }
  package_names <- trimws(gsub("\\(.*\\)", "", strsplit(imports_field, ",")[[1]]))
  package_names <- sort(package_names[nzchar(package_names)])
  versions <- vapply(package_names, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "inconnue")
  }, character(1))
  versions
}

.is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

# --- Construction du document --------------------------------------------------

.build_attestation_lines <- function(config, sources_info, rules_version, derogations, event_counts) {
  separator <- strrep("=", 70)
  subseparator <- strrep("-", 70)

  project_name <- if (!is.null(config$projet$nom)) config$projet$nom else "(sans nom)"
  client_name <- config$projet$client

  package_versions <- .attestation_package_versions()

  source_lines <- if (length(sources_info) > 0) {
    vapply(sources_info, function(s) sprintf("  - %s : %s", s$id, s$fichier), character(1))
  } else {
    "  (aucune source declaree)"
  }
  hash_lines <- if (length(sources_info) > 0) {
    vapply(sources_info, function(s) sprintf("      SHA-256 : %s", s$hash), character(1))
  } else {
    character(0)
  }
  sources_block <- if (length(sources_info) > 0) {
    as.vector(rbind(source_lines, hash_lines))
  } else {
    source_lines
  }

  derogation_lines <- if (nrow(derogations) > 0) {
    sprintf("  - %s | %s", derogations$horodatage, derogations$details)
  } else {
    "  Aucune derogation enregistree."
  }

  event_lines <- if (length(event_counts) > 0) {
    sprintf("  - %s : %d", names(event_counts), event_counts)
  } else {
    "  (journal vide ou introuvable)"
  }

  c(
    separator,
    "ATTESTATION DE REPRODUCTIBILITE",
    separator,
    "",
    sprintf("Projet            : %s", project_name),
    if (!is.null(client_name)) sprintf("Client            : %s", client_name),
    sprintf("Genere le         : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    subseparator,
    "ENVIRONNEMENT",
    subseparator,
    sprintf("R                 : %s", R.version.string),
    sprintf("Package statlab   : %s", .statlab_version()),
    sprintf("Referentiel methodologique (methodology.yml) : version %s", rules_version),
    "",
    "Packages R (versions installees) :",
    if (length(package_versions) > 0) {
      sprintf("  - %s : %s", names(package_versions), package_versions)
    } else {
      "  (non determinable)"
    },
    "",
    subseparator,
    "CONFIGURATION",
    subseparator,
    sprintf("Fichier           : %s", attr(config, "path")),
    sprintf("Empreinte SHA-256 : %s", digest::digest(file = attr(config, "path"), algo = "sha256")),
    "",
    subseparator,
    "SOURCES DE DONNEES (chemins relatifs au repertoire du projet)",
    subseparator,
    sources_block,
    "",
    subseparator,
    "DEROGATIONS ENREGISTREES (journal.log, niveau 'derogation')",
    subseparator,
    derogation_lines,
    "",
    subseparator,
    "RESUME DU JOURNAL",
    subseparator,
    event_lines,
    "",
    subseparator,
    "PORTEE",
    subseparator,
    "Cette attestation certifie que l'analyse peut etre reproduite a",
    "l'identique en reutilisant le fichier config.yml et les fichiers",
    "sources cites ci-dessus (verification par empreinte SHA-256), avec",
    "les memes versions de packages R, sous reserve de rejouer les memes",
    "derogations operateur listees ci-dessus. En l'absence de derogation,",
    "le moteur de regles methodologiques est deterministe : memes entrees,",
    "meme referentiel, meme resultat.",
    separator
  )
}

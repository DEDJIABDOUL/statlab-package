# =============================================================================
# Attestation de reproductibilite : document de synthese, piece jointe au
# livrable, qui certifie ce qu'il faut pour rejouer une analyse a l'identique
# : les fichiers sources exacts (empreintes SHA-256), les versions exactes
# de tous les packages utilises, la version du referentiel methodologique,
# les effectifs a chaque etape de la chaine (lecture, reconciliation,
# exclusions, effectif final), et toute derogation operateur consignee au
# journal (car une derogation est, par definition, une decision humaine qui
# echappe au moteur deterministe et doit donc etre rejouee a l'identique
# pour reproduire le meme resultat).
#
# Cette fonction rejoue elle-meme la chaine (lecture, reconciliation,
# preparation) pour obtenir les effectifs reels de cette execution precise :
# ce ne sont jamais des valeurs devinees a partir de la configuration seule.
# =============================================================================

#' Generer l'attestation de reproductibilite d'un projet
#'
#' Rejoue la chaine d'analyse (lecture, reconciliation, preparation) pour
#' produire un document texte certifiant les elements necessaires a la
#' reproduction a l'identique de l'analyse : empreintes des fichiers
#' sources, versions exactes de tous les packages utilises, version du
#' referentiel methodologique ([st_load_rules()]), effectifs a chaque etape
#' (lecture, reconciliation, exclusions avec motifs, effectif final), et
#' toute derogation operateur consignee dans `journal.log`
#' (cf. [st_evaluate_rules()]).
#'
#' @param config Un objet `statlab_config` valide, tel que retourne par
#'   [st_validate_config()].
#' @param output Chemin (chr) du fichier a produire. Si relatif, resolu
#'   par rapport au repertoire du projet. Par defaut,
#'   `"attestation.txt"` a la racine du projet.
#'
#' @return Le chemin absolu (chr) du fichier genere, de maniere invisible.
#' @export
st_attestation <- function(config, output = "attestation.txt") {
  checkmate::assert_class(config, "statlab_config")
  if (!isTRUE(attr(config, "valid"))) {
    cli::cli_abort("La configuration doit etre validee (st_validate_config()) avant de generer l'attestation.")
  }
  checkmate::assert_string(output, min.chars = 1)

  project_dir <- attr(config, "project_dir")
  st_log_init(project_dir)

  sources_data <- st_read_all_sources(config)
  sources_info <- .at_describe_sources(config, sources_data)

  n_apres_reconciliation <- NA_integer_
  if (!is.null(config$reconciliation)) {
    recon <- st_reconcile(sources_data, config)
    working_data <- recon$table
    n_apres_reconciliation <- nrow(working_data)
  } else {
    if (length(sources_data) != 1) {
      cli::cli_abort(c(
        "Plusieurs sources sont declarees sans operation de reconciliation.",
        "i" = "Declarer une section 'reconciliation' dans config.yml pour indiquer comment les assembler."
      ))
    }
    working_data <- sources_data[[1]]
  }

  journal_avant <- .rp_read_journal_safe(project_dir)
  prepared_data <- st_prepare(working_data, config)
  journal_apres <- .rp_read_journal_safe(project_dir)
  exclusions_df <- .rp_extract_exclusions(journal_avant, journal_apres)
  derogations_df <- .at_extract_derogations(project_dir)

  rules <- st_load_rules()

  lines <- .at_build_lines(
    config = config, sources_info = sources_info,
    n_apres_reconciliation = n_apres_reconciliation,
    exclusions_df = exclusions_df, n_final = nrow(prepared_data),
    derogations_df = derogations_df, rules_version = attr(rules, "version")
  )

  output_path <- if (.is_absolute_path(output)) output else file.path(project_dir, output)
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    cli::cli_abort("Le repertoire de destination est introuvable : {.path {output_dir}}")
  }
  writeLines(lines, output_path, useBytes = TRUE)

  absolute_output <- normalizePath(output_path, winslash = "/", mustWork = TRUE)
  st_log(
    "attestation",
    module = "attestation", fichier = absolute_output,
    n_sources = length(sources_info), n_derogations = nrow(derogations_df),
    level = "info"
  )
  cli::cli_alert_success("Attestation de reproductibilite generee : {.path {absolute_output}}")
  invisible(absolute_output)
}

# --- Collecte des informations ------------------------------------------------

.at_describe_sources <- function(config, sources_data) {
  lapply(config$sources, function(entry) {
    data <- sources_data[[entry$id]]
    path <- attr(data, "file_path")
    info <- file.info(path)
    list(
      id = entry$id,
      nom_fichier = basename(path),
      taille_octets = as.numeric(info$size),
      hash = attr(data, "file_hash"),
      date_modification = info$mtime,
      n_lignes_lues = nrow(data)
    )
  })
}

.at_extract_derogations <- function(project_dir) {
  empty <- data.frame(horodatage = character(0), details = character(0), stringsAsFactors = FALSE)
  journal <- .rp_read_journal_safe(project_dir)
  if (nrow(journal) == 0) {
    return(empty)
  }
  journal[journal$niveau == "derogation", c("horodatage", "details"), drop = FALSE]
}

.at_package_versions <- function() {
  description <- utils::packageDescription("statlab")
  imports_field <- description$Imports
  if (is.null(imports_field) || is.na(imports_field)) {
    return(data.frame(package = character(0), version = character(0), stringsAsFactors = FALSE))
  }
  package_names <- trimws(gsub("\\(.*\\)", "", strsplit(imports_field, ",")[[1]]))
  package_names <- sort(package_names[nzchar(package_names)])

  info <- as.data.frame(sessioninfo::package_info(pkgs = package_names, dependencies = FALSE))
  version <- ifelse(is.na(info$loadedversion), info$ondiskversion, info$loadedversion)
  data.frame(package = info$package, version = as.character(version), stringsAsFactors = FALSE)
}

# --- Construction du document --------------------------------------------------

.at_build_lines <- function(config, sources_info, n_apres_reconciliation, exclusions_df, n_final, derogations_df, rules_version) {
  separator <- strrep("=", 70)
  subseparator <- strrep("-", 70)

  project_name <- if (!is.null(config$projet$nom)) config$projet$nom else "(sans nom)"
  client_name <- config$projet$client

  platform <- sessioninfo::platform_info()
  package_versions <- .at_package_versions()

  c(
    separator,
    "ATTESTATION DE REPRODUCTIBILITE",
    separator,
    "",
    sprintf("Projet            : %s", project_name),
    if (!is.null(client_name)) sprintf("Client            : %s", client_name),
    sprintf("Date et heure     : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    subseparator,
    "ENVIRONNEMENT",
    subseparator,
    sprintf("R                 : %s", platform$version),
    sprintf("Systeme           : %s (%s)", platform$os, platform$system),
    sprintf("Package statlab   : %s", .statlab_version()),
    sprintf("Referentiel methodologique (methodology.yml) : version %s", rules_version),
    "",
    "Packages R (versions exactes) :",
    if (nrow(package_versions) > 0) {
      sprintf("  - %s : %s", package_versions$package, package_versions$version)
    } else {
      "  (non determinable)"
    },
    "",
    subseparator,
    "FICHIERS SOURCES",
    subseparator,
    .at_format_sources_block(sources_info),
    "",
    subseparator,
    "EFFECTIFS",
    subseparator,
    "Lignes lues par source :",
    sprintf("  - %s : %d", vapply(sources_info, function(s) s$id, character(1)), vapply(sources_info, function(s) s$n_lignes_lues, integer(1))),
    "",
    if (is.na(n_apres_reconciliation)) {
      "Lignes apres reconciliation : (sans objet - source unique, aucune reconciliation declaree)"
    } else {
      sprintf("Lignes apres reconciliation : %d", n_apres_reconciliation)
    },
    "",
    "Exclusions (dans l'ordre d'application) :",
    .at_format_exclusions_block(exclusions_df),
    "",
    sprintf("Effectif final analyse : %d", n_final),
    "",
    subseparator,
    "DEROGATIONS AUX REGLES METHODOLOGIQUES",
    subseparator,
    .at_format_derogations_block(derogations_df),
    separator
  )
}

.at_format_sources_block <- function(sources_info) {
  unlist(lapply(sources_info, function(s) {
    c(
      sprintf("- %s : %s", s$id, s$nom_fichier),
      sprintf("    Taille           : %s octets", format(s$taille_octets, big.mark = " ", scientific = FALSE)),
      sprintf("    Empreinte SHA-256 : %s", s$hash),
      sprintf("    Derniere modification : %s", format(s$date_modification, "%Y-%m-%d %H:%M:%S"))
    )
  }))
}

.at_format_exclusions_block <- function(exclusions_df) {
  if (nrow(exclusions_df) == 0) {
    return("  Aucune exclusion appliquee.")
  }
  sprintf(
    "  - %s : %d -> %d (%d exclue(s))",
    exclusions_df$motif, exclusions_df$n_avant, exclusions_df$n_apres, exclusions_df$n_exclues
  )
}

.at_format_derogations_block <- function(derogations_df) {
  if (nrow(derogations_df) == 0) {
    return("Aucune derogation enregistree.")
  }
  sprintf("  - %s | %s", derogations_df$horodatage, derogations_df$details)
}

.is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

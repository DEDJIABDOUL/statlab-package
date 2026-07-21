# =============================================================================
# Assemblage du rapport d'analyse : le livrable principal du projet. Le
# rendu est delegue a Quarto (inst/templates/rapport.qmd), avec une mise en
# forme Word par gabarit (inst/templates/reference.docx). Ce fichier ne fait
# que rejouer la chaine d'analyse (ingest -> reconciliation -> preparation
# -> tableau 1 -> comparaisons -> graphiques), preparer les donnees
# necessaires au gabarit, et piloter le rendu.
#
# Le rapport integre deux pieces deja produites par d'autres fonctions du
# package : le script R autonome (st_export_script(), Annexe A) et
# l'attestation de reproductibilite (st_attestation(), Annexe B). Elles sont
# generees dans le meme repertoire de sortie, puis leur CONTENU est
# incorpore tel quel dans le corps du rapport.
# =============================================================================

#' Generer le rapport d'analyse
#'
#' Rejoue la chaine d'analyse declaree dans `config.yml` (lecture,
#' reconciliation, preparation, tableau 1, comparaisons statistiques,
#' graphiques), genere le script R autonome ([st_export_script()]) et
#' l'attestation de reproductibilite ([st_attestation()]), puis assemble le
#' tout dans un rapport Quarto (`inst/templates/rapport.qmd`) : page de
#' garde, population etudiee (effectifs et exclusions issus du journal),
#' tableau 1, resultats des comparaisons (tableau, graphique, phrase de
#' lecture par analyse), methodologie (justifications du moteur de regles),
#' puis les deux annexes.
#'
#' @param config_path Chemin (chr) vers le fichier `config.yml`.
#' @param output_dir Repertoire (chr) de sortie du rapport et de ses
#'   annexes. Cree s'il n'existe pas.
#' @param formats Formats de sortie (chr), parmi `"docx"` et `"pdf"`.
#'
#' @return Les chemins (chr) des rapports generes (un par format), de
#'   maniere invisible.
#' @export
st_report <- function(config_path, output_dir = "sorties/rapport", formats = c("docx", "pdf")) {
  checkmate::assert_string(config_path, min.chars = 1)
  checkmate::assert_string(output_dir, min.chars = 1)
  checkmate::assert_subset(formats, c("docx", "pdf"), empty.ok = FALSE)

  config <- st_validate_config(st_read_config(config_path))
  project_dir <- attr(config, "project_dir")
  st_log_init(project_dir)

  sources_data <- st_read_all_sources(config)
  if (!is.null(config$reconciliation)) {
    recon <- st_reconcile(sources_data, config)
    working_data <- recon$table
  } else {
    if (length(sources_data) != 1) {
      cli::cli_abort(c(
        "Plusieurs sources sont declarees sans operation de reconciliation.",
        "i" = "Declarer une section 'reconciliation' dans config.yml pour indiquer comment les assembler."
      ))
    }
    working_data <- sources_data[[1]]
  }
  n_avant_preparation <- nrow(working_data)

  journal_avant <- .rp_read_journal_safe(project_dir)
  prepared_data <- st_prepare(working_data, config)
  journal_apres <- .rp_read_journal_safe(project_dir)
  exclusions_df <- .rp_extract_exclusions(journal_avant, journal_apres)

  output_dir_resolved <- if (.is_absolute_path(output_dir)) output_dir else file.path(project_dir, output_dir)
  if (!dir.exists(output_dir_resolved)) {
    dir.create(output_dir_resolved, recursive = TRUE)
  }
  output_dir_resolved <- normalizePath(output_dir_resolved, winslash = "/", mustWork = TRUE)

  script_path <- st_export_script(config, output = file.path(output_dir_resolved, "analyse.R"))
  attestation_path <- st_attestation(config_path, output = file.path(output_dir_resolved, "attestation.txt"))

  payload <- .rp_build_payload(config, prepared_data, n_avant_preparation, exclusions_df, script_path, attestation_path)

  template_path <- system.file("templates", "rapport.qmd", package = "statlab")
  if (!nzchar(template_path)) {
    cli::cli_abort("Gabarit de rapport introuvable dans le package (inst/templates/rapport.qmd).")
  }
  reference_docx_path <- system.file("templates", "reference.docx", package = "statlab")

  work_dir <- tempfile("statlab_rapport_render_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  file.copy(template_path, file.path(work_dir, "rapport.qmd"))
  if (nzchar(reference_docx_path)) {
    file.copy(reference_docx_path, file.path(work_dir, "reference.docx"))
  }

  payload_path <- file.path(work_dir, "rapport_data.rds")
  saveRDS(payload, payload_path)

  rendered_files <- character(0)
  for (format in formats) {
    quarto::quarto_render(
      input = file.path(work_dir, "rapport.qmd"),
      output_format = format,
      execute_params = list(data_path = payload_path),
      quiet = TRUE
    )

    produced <- file.path(work_dir, paste0("rapport.", format))
    if (!file.exists(produced)) {
      cli::cli_abort("Le rendu du format '{format}' a echoue : fichier attendu introuvable ({.path {produced}}).")
    }

    destination <- file.path(output_dir_resolved, paste0("rapport.", format))
    file.copy(produced, destination, overwrite = TRUE)
    rendered_files <- c(rendered_files, destination)
  }

  st_log(
    "rapport",
    module = "report", formats = paste(formats, collapse = ","),
    n_comparaisons = length(payload$comparisons), level = "info"
  )
  cli::cli_alert_success("Rapport genere : {.path {paste(rendered_files, collapse = ', ')}}")
  invisible(rendered_files)
}

# --- Journal : effectifs d'exclusion (Population etudiee) -------------------

.rp_read_journal_safe <- function(project_dir) {
  log_path <- file.path(project_dir, "journal.log")
  if (!file.exists(log_path)) {
    return(data.frame(
      horodatage = character(0), niveau = character(0), module = character(0),
      evenement = character(0), details = character(0), stringsAsFactors = FALSE
    ))
  }
  st_log_read(project_dir)
}

.rp_extract_exclusions <- function(journal_avant, journal_apres) {
  empty <- data.frame(motif = character(0), n_avant = integer(0), n_apres = integer(0), n_exclues = integer(0), stringsAsFactors = FALSE)

  n_nouvelles <- nrow(journal_apres) - nrow(journal_avant)
  if (n_nouvelles <= 0) {
    return(empty)
  }

  nouvelles <- journal_apres[seq(nrow(journal_avant) + 1, nrow(journal_apres)), , drop = FALSE]
  exclusions <- nouvelles[nouvelles$evenement == "exclusion_observations", , drop = FALSE]
  if (nrow(exclusions) == 0) {
    return(empty)
  }

  parsed <- lapply(exclusions$details, .rp_parse_exclusion_details)
  data.frame(
    motif = vapply(parsed, function(p) p$motif, character(1)),
    n_avant = vapply(parsed, function(p) p$n_avant, integer(1)),
    n_apres = vapply(parsed, function(p) p$n_apres, integer(1)),
    n_exclues = vapply(parsed, function(p) p$n_exclues, integer(1)),
    stringsAsFactors = FALSE
  )
}

.rp_parse_exclusion_details <- function(details_str) {
  m <- regmatches(details_str, regexec("^n_avant=(-?[0-9]+) n_apres=(-?[0-9]+) n_exclues=(-?[0-9]+) motif=(.*)$", details_str))[[1]]
  if (length(m) != 5) {
    cli::cli_abort("Ligne de journal d'exclusion mal formee : '{details_str}'.")
  }
  list(n_avant = as.integer(m[2]), n_apres = as.integer(m[3]), n_exclues = as.integer(m[4]), motif = m[5])
}

# --- Construction du contenu du rapport --------------------------------------

.rp_build_payload <- function(config, prepared_data, n_avant_preparation, exclusions_df, script_path, attestation_path) {
  project <- list(
    name = config$projet$nom,
    client = config$projet$client,
    auteur = .rp_current_user(),
    generated_at = Sys.time()
  )

  population <- list(
    n_initial = n_avant_preparation,
    n_final = nrow(prepared_data),
    exclusions = exclusions_df
  )

  table1_flex <- NULL
  if (!is.null(config$analyse$tableau_1)) {
    table1_flex <- st_table1_flextable(st_table1(prepared_data, config))
  }

  list(
    project = project,
    population = population,
    table1 = table1_flex,
    comparisons = .rp_build_comparisons(prepared_data, config),
    script_path = script_path,
    attestation_path = attestation_path,
    rules_version = attr(st_load_rules(), "version")
  )
}

.rp_current_user <- function() {
  utilisateur <- Sys.getenv("USERNAME", unset = Sys.getenv("USER", unset = ""))
  if (nzchar(utilisateur)) utilisateur else "(non renseigne)"
}

.rp_build_comparisons <- function(prepared_data, config) {
  entries <- config$analyse$comparaisons
  if (is.null(entries)) {
    return(list())
  }
  dictionary <- config$dictionnaire

  results <- list()
  for (entry in entries) {
    paired <- isTRUE(entry$apparie)
    for (variable in entry$variables) {
      result <- st_compare(prepared_data, variable, entry$groupe, config, paired = paired)
      nature <- .lookup_nature(variable, config)
      label_variable <- .rp_label(dictionary, variable)
      label_group <- .rp_label(dictionary, entry$groupe)

      plot <- tryCatch(
        {
          if (nature %in% c("continue", "entiere")) {
            st_plot_box(prepared_data, variable, entry$groupe, config, test_result = result)
          } else {
            st_plot_bar(prepared_data, variable, entry$groupe, config, test_result = result)
          }
        },
        error = function(e) NULL
      )

      results[[length(results) + 1]] <- list(
        variable = variable, group = entry$groupe,
        label_variable = label_variable, label_group = label_group,
        result = result,
        descriptive_flex = flextable::flextable(result$descriptive_by_group),
        plot = plot,
        reading_sentence = .rp_reading_sentence(result, label_variable, label_group)
      )
    }
  }
  results
}

.rp_label <- function(dictionary, variable) {
  entry <- dictionary[[variable]]
  if (!is.null(entry) && !is.null(entry$libelle)) entry$libelle else variable
}

.rp_reading_sentence <- function(result, label_variable, label_group) {
  p_formatted <- .format_p_value(result$p_value)
  if (!is.na(result$p_value) && result$p_value < 0.05) {
    sprintf(
      "Une difference statistiquement significative de %s est observee selon %s (%s, %s).",
      label_variable, label_group, result$test_name, p_formatted
    )
  } else {
    sprintf(
      "Aucune difference statistiquement significative de %s n'est observee selon %s (%s, %s).",
      label_variable, label_group, result$test_name, p_formatted
    )
  }
}

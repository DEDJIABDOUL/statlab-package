# =============================================================================
# Journal d'execution d'un projet statlab (journal.log).
# Module fondateur : tous les autres modules du package s'appuient sur lui
# pour assurer la tracabilite systematique des operations.
# =============================================================================

.statlab_log_state <- new.env(parent = emptyenv())
.statlab_log_state$path <- NULL

#' Initialiser le journal d'un projet statlab
#'
#' Ouvre (ou cree s'il n'existe pas) le fichier `journal.log` a la racine du
#' repertoire du projet, et y ecrit un en-tete de session (horodatage,
#' version de statlab, version de R). Les appels suivants a [st_log()] dans
#' la session en cours ecrivent dans ce fichier.
#'
#' @param project_dir Chemin (chr) du repertoire racine du projet.
#'
#' @return Le chemin (chr) du fichier `journal.log`, de maniere invisible.
#' @export
st_log_init <- function(project_dir) {
  checkmate::assert_directory_exists(project_dir, access = "r")

  log_path <- file.path(project_dir, "journal.log")
  header <- sprintf(
    "=== Session statlab | demarree le %s | statlab %s | R %s ===",
    .iso_timestamp(),
    .statlab_version(),
    .r_version()
  )
  cat(header, "\n", file = log_path, append = TRUE, sep = "")

  .statlab_log_state$path <- log_path

  if (!isTRUE(getOption("statlab.quiet", FALSE))) {
    cli::cli_alert_info("Journal de session ouvert : {.path {log_path}}")
  }

  invisible(log_path)
}

#' Ecrire une entree dans le journal
#'
#' Ecrit une ligne horodatee et structuree dans le journal ouvert par
#' [st_log_init()]. Chaque ecriture produit aussi un message a l'ecran via
#' `cli`, sauf si `options(statlab.quiet = TRUE)`.
#'
#' @param event Nom (chr) de l'evenement journalise.
#' @param ... Details de l'evenement, sous forme de paires nommees
#'   (`cle = valeur`). Le nom special `module` designe le module a
#'   l'origine de l'evenement et apparait dans une colonne dediee plutot
#'   que dans les details.
#' @param level Niveau de l'evenement : `"info"`, `"warn"`, `"error"` ou
#'   `"derogation"`.
#'
#' @return La ligne ecrite dans le journal (chr), de maniere invisible.
#' @export
st_log <- function(event, ..., level = c("info", "warn", "error", "derogation")) {
  checkmate::assert_string(event, min.chars = 1)
  level <- match.arg(level)

  log_path <- .get_log_path()

  details <- list(...)
  module <- NA_character_
  if (!is.null(details$module)) {
    module <- as.character(details$module)[1]
    details$module <- NULL
  }

  line <- .format_log_line(level, module, event, details)
  cat(line, "\n", file = log_path, append = TRUE, sep = "")

  .emit_cli_message(level, module, event)

  invisible(line)
}

#' Journaliser une exclusion d'observations
#'
#' Entree de journal dediee aux exclusions d'observations, destinee a etre
#' exploitee plus tard par les diagrammes de flux (flow charts).
#'
#' @param n_before Effectif (entier) avant l'exclusion.
#' @param n_after Effectif (entier) apres l'exclusion.
#' @param reason Motif (chr) de l'exclusion.
#'
#' @return La ligne ecrite dans le journal (chr), de maniere invisible.
#' @export
st_log_exclusion <- function(n_before, n_after, reason) {
  checkmate::assert_count(n_before)
  checkmate::assert_count(n_after)
  checkmate::assert_string(reason, min.chars = 1)
  if (n_after > n_before) {
    cli::cli_abort("n_after ({n_after}) ne peut pas etre superieur a n_before ({n_before}).")
  }

  st_log(
    "exclusion_observations",
    module = "exclusions",
    n_avant = n_before,
    n_apres = n_after,
    n_exclues = n_before - n_after,
    motif = reason,
    level = "info"
  )
}

#' Relire le journal d'un projet
#'
#' Relit le fichier `journal.log` d'un projet et le retourne sous forme de
#' data.frame, une ligne par evenement (les lignes d'en-tete de session sont
#' ignorees).
#'
#' @param project_dir Chemin (chr) du repertoire racine du projet.
#'
#' @return Un data.frame avec les colonnes `horodatage`, `niveau`, `module`,
#'   `evenement`, `details`.
#' @export
st_log_read <- function(project_dir) {
  checkmate::assert_directory_exists(project_dir, access = "r")

  log_path <- file.path(project_dir, "journal.log")
  if (!file.exists(log_path)) {
    cli::cli_abort("Journal introuvable : {.path {log_path}}. Appeler st_log_init() au prealable.")
  }

  lines <- readLines(log_path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  event_lines <- lines[!startsWith(lines, "===")]

  columns <- c("horodatage", "niveau", "module", "evenement", "details")
  if (length(event_lines) == 0) {
    empty_df <- as.data.frame(
      stats::setNames(replicate(length(columns), character(0), simplify = FALSE), columns),
      stringsAsFactors = FALSE
    )
    return(empty_df)
  }

  parts <- strsplit(event_lines, " \\| ")
  extract <- function(x, i) if (length(x) >= i) x[i] else ""

  data.frame(
    horodatage = vapply(parts, extract, character(1), 1),
    niveau = vapply(parts, extract, character(1), 2),
    module = vapply(parts, extract, character(1), 3),
    evenement = vapply(parts, extract, character(1), 4),
    details = vapply(parts, extract, character(1), 5),
    stringsAsFactors = FALSE
  )
}

# --- Helpers internes --------------------------------------------------------

.get_log_path <- function() {
  path <- .statlab_log_state$path
  if (is.null(path)) {
    cli::cli_abort("Le journal n'a pas ete initialise : appeler st_log_init() avant st_log().")
  }
  path
}

.format_log_line <- function(level, module, event, details) {
  paste(
    .iso_timestamp(),
    level,
    if (is.na(module)) "" else .sanitize_field(module),
    .sanitize_field(event),
    .format_details(details),
    sep = " | "
  )
}

.format_details <- function(details) {
  if (length(details) == 0) return("")
  field_names <- names(details)
  if (is.null(field_names) || any(field_names == "")) {
    cli::cli_abort("Tous les elements transmis a st_log() via ... doivent etre nommes.")
  }
  pairs <- vapply(seq_along(details), function(i) {
    value <- paste(as.character(details[[i]]), collapse = ",")
    sprintf("%s=%s", field_names[i], .sanitize_field(value))
  }, character(1))
  paste(pairs, collapse = " ")
}

.sanitize_field <- function(x) {
  x <- gsub("[\r\n]+", " ", x)
  gsub("\\|", "/", x)
}

.iso_timestamp <- function() {
  strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}

.statlab_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("statlab")),
    error = function(e) "dev"
  )
}

.r_version <- function() {
  R.version.string
}

.emit_cli_message <- function(level, module, event) {
  if (isTRUE(getOption("statlab.quiet", FALSE))) {
    return(invisible(NULL))
  }
  text <- if (!is.na(module) && nzchar(module)) sprintf("[%s] %s", module, event) else event
  switch(level,
    info = cli::cli_alert_info(text),
    warn = cli::cli_alert_warning(text),
    error = cli::cli_alert_danger(text),
    derogation = cli::cli_alert(c("!" = text))
  )
  invisible(NULL)
}

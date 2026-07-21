# =============================================================================
# Generation du rapport d'audit (premier livrable client).
# Le rendu est delegue a Quarto (inst/templates/audit.qmd) ; ce fichier ne
# fait que preparer les donnees necessaires au gabarit et piloter le rendu.
# =============================================================================

#' Generer le rapport d'audit d'un projet
#'
#' Lit et valide la configuration, lit toutes les sources declarees, les
#' profile, detecte leurs anomalies, puis rend le rapport d'audit
#' (`inst/templates/audit.qmd`) dans les formats demandes.
#'
#' @param config_path Chemin (chr) vers le fichier `config.yml`.
#' @param output_dir Repertoire (chr) de sortie des rapports. Cree s'il
#'   n'existe pas.
#' @param formats Formats de sortie (chr), parmi `"html"` et `"pdf"`.
#'
#' @return Les chemins (chr) des rapports generes, de maniere invisible.
#' @export
st_audit <- function(config_path, output_dir = "sorties/audit", formats = c("html", "pdf")) {
  checkmate::assert_string(config_path, min.chars = 1)
  checkmate::assert_string(output_dir, min.chars = 1)
  checkmate::assert_subset(formats, c("html", "pdf"), empty.ok = FALSE)

  config <- st_validate_config(st_read_config(config_path))
  project_dir <- attr(config, "project_dir")
  st_log_init(project_dir)

  sources_data <- st_read_all_sources(config)
  payload <- .build_audit_payload(config, sources_data)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  template_path <- system.file("templates", "audit.qmd", package = "statlab")
  if (!nzchar(template_path)) {
    cli::cli_abort("Gabarit de rapport introuvable dans le package (inst/templates/audit.qmd).")
  }

  work_dir <- tempfile("statlab_audit_render_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  file.copy(template_path, file.path(work_dir, "audit.qmd"))

  payload_path <- file.path(work_dir, "audit_data.rds")
  saveRDS(payload, payload_path)

  rendered_files <- character(0)
  for (format in formats) {
    quarto::quarto_render(
      input = file.path(work_dir, "audit.qmd"),
      output_format = format,
      execute_params = list(data_path = payload_path),
      quiet = TRUE
    )

    extension <- if (format == "html") "html" else "pdf"
    produced <- file.path(work_dir, paste0("audit.", extension))
    if (!file.exists(produced)) {
      cli::cli_abort("Le rendu du format '{format}' a echoue : fichier attendu introuvable ({.path {produced}}).")
    }

    destination <- file.path(output_dir, paste0("audit.", extension))
    file.copy(produced, destination, overwrite = TRUE)
    rendered_files <- c(rendered_files, destination)
  }

  st_log(
    "audit",
    module = "audit_report",
    config = normalizePath(config_path, winslash = "/", mustWork = TRUE),
    formats = paste(formats, collapse = ","),
    n_sources = length(sources_data),
    n_anomalies = nrow(payload$anomalies),
    level = "info"
  )

  cli::cli_alert_success("Rapport d'audit genere : {.path {paste(rendered_files, collapse = ', ')}}")
  invisible(rendered_files)
}

# --- Preparation des donnees consommees par le gabarit Quarto ---------------

.build_audit_payload <- function(config, sources_data) {
  project <- list(
    name = config$projet$nom,
    client = config$projet$client,
    generated_at = Sys.time()
  )

  sources_info <- list()
  anomalies_list <- list()
  numeric_list <- list()

  for (source_id in names(sources_data)) {
    data <- sources_data[[source_id]]
    profile <- st_profile(data)
    anomalies <- st_detect_anomalies(data, profile, config)

    if (nrow(anomalies) > 0) {
      anomalies$excerpt <- lapply(seq_len(nrow(anomalies)), function(i) {
        .build_anomaly_excerpt(data, anomalies[i, ], profile)
      })
      anomalies$source <- rep(source_id, nrow(anomalies))
      anomalies_list[[source_id]] <- anomalies
    }

    sources_info[[source_id]] <- list(
      file_name = basename(attr(data, "file_path")),
      file_hash = attr(data, "file_hash"),
      sheet = attr(data, "sheet"),
      header_row = attr(data, "header_row"),
      n_raw_rows = attr(data, "n_raw_rows"),
      n_rows = nrow(data),
      n_cols = ncol(data),
      profile = profile
    )

    numeric_vars <- profile$name[profile$inferred_nature %in% c("continue", "entiere")]
    if (length(numeric_vars) > 0) {
      long_rows <- Filter(Negate(is.null), lapply(numeric_vars, function(variable) {
        values <- .to_numeric_permissive(normalize_missing(as.character(data[[variable]]))$x)
        values <- values[!is.na(values)]
        if (length(values) == 0) {
          return(NULL)
        }
        data.frame(source = source_id, variable = variable, value = values, stringsAsFactors = FALSE)
      }))
      if (length(long_rows) > 0) {
        numeric_list[[source_id]] <- do.call(rbind, long_rows)
      }
    }
  }

  anomalies_df <- do.call(rbind, anomalies_list)
  if (is.null(anomalies_df)) {
    anomalies_df <- .empty_anomaly_df()
    anomalies_df$source <- character(0)
    anomalies_df$excerpt <- list()
  }
  numeric_df <- do.call(rbind, numeric_list)

  severity_levels <- c("bloquant", "avertissement", "information")
  anomaly_counts <- stats::setNames(
    as.list(vapply(severity_levels, function(s) sum(anomalies_df$severity == s), integer(1))),
    severity_levels
  )

  list(
    project = project,
    sources = sources_info,
    anomalies = anomalies_df,
    numeric_long = numeric_df,
    totals = list(
      n_sources = length(sources_data),
      n_rows = sum(vapply(sources_info, function(s) s$n_rows, integer(1))),
      n_cols = length(unique(unlist(lapply(sources_info, function(s) s$profile$name)))),
      anomaly_counts = anomaly_counts
    ),
    recommendations = .build_recommendations(anomalies_df)
  )
}

.build_anomaly_excerpt <- function(data, anomaly_row, profile) {
  rows_idx <- utils::head(anomaly_row$rows_affected[[1]], 10)
  if (length(rows_idx) == 0) {
    return(data.frame(ligne = integer(0)))
  }

  variable <- anomaly_row$variable
  if (is.na(variable)) {
    columns <- names(data)
  } else if (grepl(" vs ", variable, fixed = TRUE)) {
    columns <- trimws(strsplit(variable, " vs ", fixed = TRUE)[[1]])
  } else {
    columns <- variable
  }

  identifier_vars <- profile$name[profile$inferred_nature == "identifiant"]
  if (length(identifier_vars) > 0 && !(identifier_vars[1] %in% columns)) {
    columns <- c(identifier_vars[1], columns)
  }
  columns <- intersect(columns, names(data))

  excerpt <- data[rows_idx, columns, drop = FALSE]
  excerpt <- cbind(ligne = rows_idx, excerpt)
  rownames(excerpt) <- NULL
  excerpt
}

.build_recommendations <- function(anomalies_df) {
  if (nrow(anomalies_df) == 0) {
    return("Aucune anomalie n'a ete detectee : le dictionnaire de config.yml peut etre valide en l'etat.")
  }

  actions <- c(
    missing_codes = "harmoniser les codes utilises pour les valeurs manquantes",
    missing_rate = "decider d'une strategie de traitement des valeurs manquantes",
    duplicate_rows = "trancher le traitement des lignes dupliquees",
    duplicate_ids = "resoudre les identifiants dupliques",
    constant_columns = "confirmer l'utilite d'une variable sans variation",
    empty_columns = "decider de conserver ou non une variable vide ou presque vide",
    level_variants = "fusionner les modalites qui designent la meme chose",
    high_cardinality = "revoir la nature d'une variable a tres nombreuses categories",
    impossible_values = "corriger ou exclure des valeurs physiologiquement impossibles",
    outliers = "examiner des valeurs statistiquement extremes",
    date_coherence = "verifier l'ordre chronologique de deux dates",
    numeric_in_text = "reconsiderer la nature d'une variable actuellement en texte"
  )

  order_idx <- order(factor(anomalies_df$severity, levels = c("bloquant", "avertissement", "information")))
  ordered_df <- anomalies_df[order_idx, , drop = FALSE]

  items <- vapply(seq_len(nrow(ordered_df)), function(i) {
    check_id <- ordered_df$check_id[i]
    action <- if (check_id %in% names(actions)) actions[[check_id]] else check_id
    target <- if (is.na(ordered_df$variable[i])) {
      "l'ensemble des donnees"
    } else {
      sprintf("'%s'", ordered_df$variable[i])
    }
    sprintf("%s pour %s (%s).", action, target, ordered_df$severity[i])
  }, character(1))

  items <- unique(items)
  if (length(items) > 15) {
    items <- c(items[seq_len(15)], sprintf(
      "... et %d autre(s) point(s) : voir le detail des anomalies ci-dessus.", length(items) - 15
    ))
  }
  items
}

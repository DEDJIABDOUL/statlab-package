# =============================================================================
# Generation d'un config.yml pre-rempli a partir de la lecture, du
# profilage et de la detection d'anomalies des sources declarees.
# Le fichier produit doit passer st_validate_config() sans modification.
# =============================================================================

#' Generer un config.yml pre-rempli a partir des sources
#'
#' Lit les sources, les profile, detecte leurs anomalies, puis ecrit un
#' fichier `config.yml` complet et valide (au sens de
#' [st_validate_config()]). Le dictionnaire est rempli avec la nature
#' inferee et un libelle propose pour chaque variable ; les points qui
#' necessitent une decision humaine sont signales par des commentaires
#' "# A VERIFIER". Les sections `analyse` et `preparation` sont laissees
#' en gabarit commente, personnalise avec les noms de variables reels.
#'
#' @param sources Vecteur de chemins (chr) vers les fichiers sources.
#'   S'il est nomme, les noms servent d'identifiants de source ; sinon,
#'   l'identifiant est deduit du nom de fichier (sans extension).
#' @param output Chemin (chr) du fichier `config.yml` a creer.
#' @param overwrite Si `FALSE` (par defaut), refuse d'ecraser `output`
#'   s'il existe deja.
#'
#' @return Le chemin absolu (chr) du fichier genere, de maniere invisible.
#' @export
st_scaffold_config <- function(sources, output = "config.yml", overwrite = FALSE) {
  checkmate::assert_character(sources, min.len = 1, any.missing = FALSE)
  checkmate::assert_string(output, min.chars = 1)
  checkmate::assert_flag(overwrite)

  if (file.exists(output) && !overwrite) {
    cli::cli_abort(c(
      "Le fichier {.path {output}} existe deja.",
      "i" = "Utiliser overwrite = TRUE pour l'ecraser."
    ))
  }

  output_dir <- dirname(output)
  if (!dir.exists(output_dir)) {
    cli::cli_abort("Le repertoire de destination est introuvable : {.path {output_dir}}")
  }
  project_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  ids <- names(sources)
  if (is.null(ids)) {
    ids <- rep(NA_character_, length(sources))
  }
  a_deduire <- is.na(ids) | !nzchar(ids)
  ids[a_deduire] <- tools::file_path_sans_ext(basename(sources[a_deduire]))
  if (any(duplicated(ids))) {
    cli::cli_abort(c(
      "Les identifiants de sources deduits contiennent des doublons : {.val {unique(ids[duplicated(ids)])}}.",
      "i" = "Nommer explicitement le vecteur 'sources' (ex : c(inclusion = \"...\", suivi = \"...\"))."
    ))
  }

  st_log_init(project_dir)

  read_sources <- vector("list", length(sources))
  names(read_sources) <- ids
  for (i in seq_along(sources)) {
    read_sources[[i]] <- st_read_source(list(id = ids[i], fichier = sources[i]))
  }

  profiles <- lapply(read_sources, st_profile)
  anomalies <- Map(
    function(data, profile) st_detect_anomalies(data, profile, config = NULL),
    read_sources, profiles
  )

  lines <- .build_config_lines(
    project_dir = project_dir, sources = sources, ids = ids,
    read_sources = read_sources, profiles = profiles, anomalies = anomalies
  )
  writeLines(lines, output, useBytes = TRUE)

  absolute_output <- normalizePath(output, winslash = "/", mustWork = TRUE)
  n_variables <- length(unique(unlist(lapply(profiles, function(p) p$name))))

  st_log(
    "generation_config",
    module = "scaffold",
    fichier = absolute_output,
    n_sources = length(sources),
    n_variables = n_variables,
    level = "info"
  )

  cli::cli_alert_success("config.yml genere : {.path {absolute_output}}")
  invisible(absolute_output)
}

# --- Construction du contenu YAML (avec commentaires) -----------------------

.build_config_lines <- function(project_dir, sources, ids, read_sources, profiles, anomalies) {
  hints <- .load_label_hints()
  variable_info <- .collect_variable_info(ids, read_sources, profiles, anomalies)

  project_name_guess <- basename(project_dir)
  if (!nzchar(project_name_guess) || project_name_guess %in% c(".", "/")) {
    project_name_guess <- "Projet sans nom"
  }

  n_flags <- sum(vapply(variable_info, function(v) v$conflict || length(v$flags) > 0, logical(1)))
  n_dataset_flags <- sum(vapply(anomalies, function(a) sum(is.na(a$variable)), integer(1)))

  lines <- c(
    "# =============================================================================",
    "# config.yml genere automatiquement par st_scaffold_config().",
    sprintf("# Genere le %s.", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "#",
    "# Ce fichier est VALIDE tel quel (st_validate_config() passe sans",
    "# modification) : l'analyse peut etre lancee immediatement. Les points",
    sprintf(
      "# signales par \"# A VERIFIER\" (%d variable(s) et %d source(s) concernee(s)) valent",
      n_flags, n_dataset_flags
    ),
    "# une relecture avant de considerer la configuration definitive.",
    "# =============================================================================",
    "",
    "# A VERIFIER : nom du projet devine a partir du nom du repertoire.",
    "projet:",
    sprintf("  nom: %s", .yaml_quote(project_name_guess)),
    "  langue: \"fr\"",
    "",
    .build_sources_lines(sources, ids, read_sources, project_dir, anomalies),
    "",
    .build_dictionary_lines(variable_info, hints),
    "",
    .build_template_lines(variable_info)
  )
  lines
}

.build_sources_lines <- function(sources, ids, read_sources, project_dir, anomalies) {
  lines <- "sources:"
  for (i in seq_along(sources)) {
    data <- read_sources[[i]]
    relative_path <- .relative_path(sources[i], project_dir)
    sheet <- attr(data, "sheet")
    header_row <- attr(data, "header_row")

    dataset_level <- anomalies[[i]][is.na(anomalies[[i]]$variable), , drop = FALSE]
    for (flag in .anomaly_row_flags(dataset_level)) {
      lines <- c(lines, sprintf('  # A VERIFIER : %s (source "%s").', flag, ids[i]))
    }

    lines <- c(
      lines,
      sprintf("  - id: %s", .yaml_quote(ids[i])),
      sprintf("    fichier: %s", .yaml_quote(relative_path))
    )
    if (!is.na(sheet)) {
      lines <- c(lines, sprintf("    onglet: %s", .yaml_quote(sheet)))
    }
    lines <- c(lines, sprintf("    ligne_entete: %d", header_row))
  }
  lines
}

.build_dictionary_lines <- function(variable_info, hints) {
  lines <- "dictionnaire:"
  for (variable in names(variable_info)) {
    entry <- variable_info[[variable]]
    label_info <- .guess_label(variable, hints)

    comments <- character(0)
    if (entry$conflict) {
      comments <- c(comments, sprintf(
        "  # A VERIFIER : nature detectee differemment selon la source (%s).",
        paste(unique(entry$sources), collapse = ", ")
      ))
    }
    if (label_info$guessed) {
      comments <- c(comments, "  # A VERIFIER : libelle devine, aucune correspondance dans label_hints.yml.")
    }
    for (flag in unique(entry$flags)) {
      comments <- c(comments, sprintf("  # A VERIFIER : %s.", flag))
    }

    lines <- c(
      lines, comments,
      sprintf("  %s:", .yaml_quote(variable)),
      sprintf("    nature: %s", .yaml_quote(entry$nature)),
      sprintf("    libelle: %s", .yaml_quote(label_info$label))
    )

    if (entry$nature %in% c("nominale", "binaire", "ordinale") && length(entry$modalites) > 0) {
      modalites <- entry$modalites
      if (length(modalites) > 50) {
        lines <- c(lines, sprintf(
          "    # A VERIFIER : %d modalites au total, liste tronquee aux 50 premieres.",
          length(modalites)
        ))
        modalites <- modalites[seq_len(50)]
      }
      lines <- c(lines, "    modalites:")
      for (modalite in modalites) {
        lines <- c(lines, sprintf("      - %s", .yaml_quote(modalite)))
      }
    }
  }
  lines
}

.build_template_lines <- function(variable_info) {
  variables <- names(variable_info)
  numeric_vars <- names(Filter(function(v) v$nature %in% c("continue", "entiere"), variable_info))
  group_vars <- names(Filter(function(v) v$nature %in% c("binaire", "nominale"), variable_info))

  first_numeric <- if (length(numeric_vars) > 0) numeric_vars[1] else "variable_numerique"
  first_group <- if (length(group_vars) > 0) group_vars[1] else "variable_de_groupe"
  flagged_vars <- names(Filter(function(v) length(v$flags) > 0, variable_info))
  missing_targets <- if (length(flagged_vars) > 0) flagged_vars else first_numeric

  lines <- c(
    "# Les deux sections suivantes sont laissees en gabarit commente : decommenter",
    "# et adapter les lignes utiles, supprimer le reste.",
    "#",
    "# preparation:",
    "#   exclusions:",
    sprintf('#     - condition: "%s < 0"', first_numeric),
    '#       motif: "Valeur invraisemblable"',
    "#   derivations:",
    '#     - nom: "nouvelle_variable"',
    sprintf('#       formule: "%s * 2"', first_numeric),
    '#       libelle: "Libelle de la variable derivee"',
    "#   manquants:"
  )
  for (variable in missing_targets) {
    lines <- c(
      lines,
      sprintf('#     - variable: "%s"', variable),
      "#       strategie: \"conserver\"  # ou 'exclure_ligne' / 'imputer'"
    )
  }

  lines <- c(
    lines,
    "#",
    "# analyse:",
    "#   tableau_1:",
    sprintf('#     stratification: "%s"', first_group),
    sprintf("#     variables: [%s]", paste(sprintf('"%s"', utils::head(variables, 5)), collapse = ", ")),
    "#   comparaisons:",
    sprintf('#     - variables: ["%s"]', first_numeric),
    sprintf('#       groupe: "%s"', first_group),
    "#       apparie: false"
  )
  lines
}

# --- Collecte des informations par variable ---------------------------------

.collect_variable_info <- function(ids, read_sources, profiles, anomalies) {
  info <- list()

  for (i in seq_along(ids)) {
    source_id <- ids[i]
    profile <- profiles[[i]]
    data <- read_sources[[i]]

    for (row in seq_len(nrow(profile))) {
      variable <- profile$name[row]
      nature <- profile$inferred_nature[row]

      if (is.null(info[[variable]])) {
        info[[variable]] <- list(
          nature = nature, sources = source_id, conflict = FALSE,
          modalites = character(0), flags = character(0)
        )
      } else {
        if (!identical(info[[variable]]$nature, nature)) {
          info[[variable]]$conflict <- TRUE
        }
        info[[variable]]$sources <- c(info[[variable]]$sources, source_id)
      }

      if (nature %in% c("nominale", "binaire", "ordinale")) {
        values <- normalize_missing(as.character(data[[variable]]))$x
        info[[variable]]$modalites <- sort(unique(c(
          info[[variable]]$modalites, unique(values[!is.na(values)])
        )))
      }
    }

    anomaly_df <- anomalies[[i]]
    if (nrow(anomaly_df) > 0) {
      for (row in seq_len(nrow(anomaly_df))) {
        variable <- anomaly_df$variable[row]
        if (is.na(variable) || grepl(" vs ", variable, fixed = TRUE)) {
          next
        }
        if (!is.null(info[[variable]])) {
          info[[variable]]$flags <- c(info[[variable]]$flags, .anomaly_flag_text(anomaly_df[row, ]))
        }
      }
    }
  }

  info
}

.anomaly_row_flags <- function(anomaly_df) {
  if (nrow(anomaly_df) == 0) {
    return(character(0))
  }
  vapply(seq_len(nrow(anomaly_df)), function(i) .anomaly_flag_text(anomaly_df[i, ]), character(1))
}

.anomaly_flag_text <- function(anomaly_row) {
  switch(anomaly_row$check_id,
    missing_rate = sprintf("taux de manquants eleve (%.0f%% des lignes)", anomaly_row$pct_affected * 100),
    duplicate_ids = "valeurs d'identifiant dupliquees",
    constant_columns = "variable constante ou quasi-constante",
    empty_columns = "variable vide ou presque vide",
    high_cardinality = "beaucoup de modalites pour une variable nominale : nature a reconsiderer",
    impossible_values = "valeurs hors des bornes physiologiques plausibles",
    outliers = "valeurs statistiquement extremes",
    numeric_in_text = "valeurs majoritairement numeriques : nature 'texte' a reconsiderer",
    anomaly_row$detail
  )
}

# --- Libelles -----------------------------------------------------------------

.guess_label <- function(variable, hints) {
  tokens <- .tokenize(variable)
  best_label <- NULL
  best_len <- 0L

  for (motif in names(hints)) {
    motif_tokens <- .tokenize(motif)
    if (length(motif_tokens) > best_len && .contains_subsequence(tokens, motif_tokens)) {
      best_label <- hints[[motif]]
      best_len <- length(motif_tokens)
    }
  }

  if (!is.null(best_label)) {
    return(list(label = best_label, guessed = FALSE))
  }

  naive <- gsub("[^[:alnum:]]+", " ", variable)
  naive <- trimws(naive)
  if (nzchar(naive)) {
    naive <- paste0(toupper(substr(naive, 1, 1)), substr(naive, 2, nchar(naive)))
  } else {
    naive <- variable
  }
  list(label = naive, guessed = TRUE)
}

.tokenize <- function(x) {
  strsplit(tolower(x), "[^a-z0-9]+")[[1]]
}

.contains_subsequence <- function(tokens, motif_tokens) {
  n <- length(tokens)
  m <- length(motif_tokens)
  if (m == 0 || m > n) {
    return(FALSE)
  }
  for (start in seq_len(n - m + 1)) {
    if (identical(tokens[start:(start + m - 1)], motif_tokens)) {
      return(TRUE)
    }
  }
  FALSE
}

.load_label_hints <- function() {
  path <- system.file("rules", "label_hints.yml", package = "statlab")
  if (!nzchar(path) || !file.exists(path)) {
    return(list())
  }
  yaml::read_yaml(path)
}

# --- Chemins -------------------------------------------------------------------

.relative_path <- function(path, base_dir) {
  absolute_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  absolute_base <- normalizePath(base_dir, winslash = "/", mustWork = TRUE)

  path_parts <- strsplit(absolute_path, "/")[[1]]
  base_parts <- strsplit(absolute_base, "/")[[1]]

  common <- 0L
  n <- min(length(path_parts), length(base_parts))
  for (i in seq_len(n)) {
    if (!identical(path_parts[i], base_parts[i])) {
      break
    }
    common <- i
  }

  up <- if (length(base_parts) > common) rep("..", length(base_parts) - common) else character(0)
  down <- if (length(path_parts) > common) path_parts[(common + 1):length(path_parts)] else character(0)
  paste(c(up, down), collapse = "/")
}

.yaml_quote <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  paste0('"', x, '"')
}

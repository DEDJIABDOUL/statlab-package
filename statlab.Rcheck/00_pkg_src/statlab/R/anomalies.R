# =============================================================================
# Detection d'anomalies. Chaque detecteur est une fonction independante,
# de signature uniforme (data, profile = NULL, config = NULL), qui retourne
# un data.frame normalise :
#   check_id, severity, variable, n_affected, pct_affected, detail,
#   rows_affected
# avec severity dans {"bloquant", "avertissement", "information"}.
#
# Aucun detecteur ne corrige quoi que ce soit : ils constatent. La decision
# (exclure, imputer, fusionner des modalites...) reste a l'operateur, via
# config.yml.
#
# Grille de severite retenue :
#   bloquant      : corrompt directement les effectifs ou les statistiques
#                   (doublons, identifiants dupliques, taux de manquants
#                   majoritaire, valeurs physiologiquement impossibles).
#   avertissement : merite un examen mais n'est pas necessairement une
#                   erreur (taux de manquants modere, codages heterogenes,
#                   modalites a fusionner, cardinalite elevee, valeurs
#                   extremes, incoherence chronologique).
#   information    : signal utile mais sans consequence directe sur la
#                   validite des donnees (colonne constante, colonne texte
#                   probablement numerique).
# =============================================================================

#' Detecter les anomalies d'un jeu de donnees
#'
#' Execute l'ensemble des detecteurs internes du module et retourne un
#' data.frame consolide, trie par severite (bloquant, puis avertissement,
#' puis information).
#'
#' @param data Un data.frame (typiquement le resultat de
#'   [st_read_source()]).
#' @param profile Le profilage de `data`, tel que retourne par
#'   [st_profile()]. Utilise pour connaitre la nature inferee de chaque
#'   variable.
#' @param config Un objet `statlab_config` optionnel. Si son
#'   `dictionnaire` declare une nature pour une variable, cette nature
#'   prevaut sur `profile$inferred_nature` pour les detecteurs qui en ont
#'   besoin.
#'
#' @return Un data.frame avec les colonnes `check_id`, `severity`,
#'   `variable`, `n_affected`, `pct_affected`, `detail`, `rows_affected`
#'   (colonne-liste : indices de lignes concernees).
#' @export
st_detect_anomalies <- function(data, profile, config = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_data_frame(profile)
  checkmate::assert_class(config, "statlab_config", null.ok = TRUE)

  detectors <- list(
    check_missing_codes, check_missing_rate, check_duplicate_rows, check_duplicate_ids,
    check_constant_columns, check_empty_columns, check_level_variants, check_high_cardinality,
    check_impossible_values, check_outliers, check_date_coherence, check_numeric_in_text
  )

  results <- lapply(detectors, function(detector) detector(data, profile, config))
  consolidated <- do.call(rbind, results)

  if (is.null(consolidated) || nrow(consolidated) == 0) {
    return(.empty_anomaly_df())
  }

  severity_order <- c(bloquant = 1L, avertissement = 2L, information = 3L)
  consolidated <- consolidated[order(severity_order[consolidated$severity]), , drop = FALSE]
  rownames(consolidated) <- NULL
  consolidated
}

# --- Detecteurs ---------------------------------------------------------------

#' Codages heterogenes de valeurs manquantes
#'
#' Signale les variables ou plusieurs codes de manquant differents (parmi
#' ceux reconnus par [normalize_missing()]) sont utilises simultanement,
#' ce qui suggere une saisie non harmonisee.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_missing_codes <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in names(data)) {
    x <- as.character(data[[variable]])
    normalized <- normalize_missing(x)
    if (length(normalized$codes) < 2) {
      next
    }

    affected <- which(!is.na(x) & toupper(trimws(x)) %in% toupper(normalized$codes))
    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(affected))
    details <- c(details, sprintf(
      "Codages de manquant heterogenes : %s.",
      paste(normalized$codes, collapse = ", ")
    ))
    rows[[length(rows) + 1]] <- affected
  }

  .build_anomaly_rows("missing_codes", "avertissement", variables, n_affected, total, details, rows)
}

#' Taux de valeurs manquantes
#'
#' Signale les variables dont le taux de manquants depasse 20 %
#' (avertissement) ou 50 % (bloquant).
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_missing_rate <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)

  variables <- character(0)
  severities <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in names(data)) {
    x <- as.character(data[[variable]])
    missing_idx <- which(is.na(normalize_missing(x)$x))
    rate <- .safe_pct(length(missing_idx), total)

    if (rate > 0.5) {
      severity <- "bloquant"
    } else if (rate > 0.2) {
      severity <- "avertissement"
    } else {
      next
    }

    variables <- c(variables, variable)
    severities <- c(severities, severity)
    n_affected <- c(n_affected, length(missing_idx))
    details <- c(details, sprintf("Taux de valeurs manquantes : %.1f%%.", rate * 100))
    rows[[length(rows) + 1]] <- missing_idx
  }

  .build_anomaly_rows("missing_rate", severities, variables, n_affected, total, details, rows)
}

#' Doublons stricts de lignes
#'
#' Signale les lignes strictement identiques sur toutes les colonnes.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_duplicate_rows <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)

  is_duplicate <- duplicated(data) | duplicated(data, fromLast = TRUE)
  affected <- which(is_duplicate)
  if (length(affected) == 0) {
    return(.empty_anomaly_df())
  }

  n_excess <- sum(duplicated(data))
  detail <- sprintf(
    "%d lignes impliquees dans des doublons stricts (%d ligne(s) en exces par rapport a des observations uniques).",
    length(affected), n_excess
  )

  .build_anomaly_rows("duplicate_rows", "bloquant", NA_character_, length(affected), total, detail, list(affected))
}

#' Doublons d'identifiant
#'
#' Signale les valeurs dupliquees d'une variable de nature `identifiant`
#' (au sens de `profile$inferred_nature`, ou de la declaration dans
#' `config$dictionnaire` si elle prevaut).
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_duplicate_ids <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)
  id_vars <- .variables_with_nature(names(data), profile, config, "identifiant")

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in id_vars) {
    normalized <- normalize_missing(as.character(data[[variable]]))$x
    is_duplicate <- !is.na(normalized) & (duplicated(normalized) | duplicated(normalized, fromLast = TRUE))
    affected <- which(is_duplicate)
    if (length(affected) == 0) {
      next
    }

    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(affected))
    details <- c(details, sprintf(
      "%d lignes portent une valeur d'identifiant dupliquee (%d valeurs distinctes concernees).",
      length(affected), length(unique(normalized[is_duplicate]))
    ))
    rows[[length(rows) + 1]] <- affected
  }

  .build_anomaly_rows("duplicate_ids", "bloquant", variables, n_affected, total, details, rows)
}

#' Colonnes constantes ou quasi-constantes
#'
#' Signale les variables dont une seule modalite couvre toutes les valeurs
#' non manquantes (constante), ou au moins 95 % d'entre elles
#' (quasi-constante).
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_constant_columns <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in names(data)) {
    normalized <- normalize_missing(as.character(data[[variable]]))$x
    non_missing_idx <- which(!is.na(normalized))
    values <- normalized[non_missing_idx]
    if (length(values) == 0) {
      next
    }

    frequencies <- sort(table(values), decreasing = TRUE)
    top_share <- as.numeric(frequencies[1]) / length(values)

    if (length(frequencies) == 1) {
      variables <- c(variables, variable)
      n_affected <- c(n_affected, length(values))
      details <- c(details, sprintf("Variable constante : une seule valeur ('%s').", names(frequencies)[1]))
      rows[[length(rows) + 1]] <- non_missing_idx
    } else if (top_share >= 0.95) {
      affected <- non_missing_idx[values == names(frequencies)[1]]
      variables <- c(variables, variable)
      n_affected <- c(n_affected, length(affected))
      details <- c(details, sprintf(
        "Variable quasi-constante : '%s' represente %.1f%% des valeurs non manquantes.",
        names(frequencies)[1], top_share * 100
      ))
      rows[[length(rows) + 1]] <- affected
    }
  }

  .build_anomaly_rows("constant_columns", "information", variables, n_affected, total, details, rows)
}

#' Colonnes vides ou presque vides
#'
#' Signale les variables sans aucune valeur, ou dont le taux de manquants
#' depasse 95 %.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_empty_columns <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in names(data)) {
    missing_idx <- which(is.na(normalize_missing(as.character(data[[variable]]))$x))
    rate <- .safe_pct(length(missing_idx), total)
    if (rate < 0.95) {
      next
    }

    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(missing_idx))
    details <- c(details, if (rate >= 1) {
      "Variable entierement vide."
    } else {
      sprintf("Variable presque vide : %.1f%% de manquants.", rate * 100)
    })
    rows[[length(rows) + 1]] <- missing_idx
  }

  .build_anomaly_rows("empty_columns", "avertissement", variables, n_affected, total, details, rows)
}

#' Modalites probablement equivalentes
#'
#' Signale, pour les variables categorielles (nominale, binaire, ordinale),
#' les groupes de modalites qui deviennent identiques apres normalisation
#' (casse, espaces, accents) et sont donc candidates a une fusion manuelle.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_level_variants <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)
  categorical_vars <- .variables_with_nature(names(data), profile, config, c("nominale", "binaire", "ordinale"))

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in categorical_vars) {
    x <- as.character(data[[variable]])
    groups <- normalize_levels(x)
    if (length(groups) == 0) {
      next
    }

    variants <- unlist(groups)
    affected <- which(!is.na(x) & x %in% variants)

    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(affected))
    summary_txt <- paste(
      vapply(groups, function(g) paste0("{", paste(g, collapse = " / "), "}"), character(1)),
      collapse = ", "
    )
    details <- c(details, sprintf("Modalites probablement equivalentes : %s.", summary_txt))
    rows[[length(rows) + 1]] <- affected
  }

  .build_anomaly_rows("level_variants", "avertissement", variables, n_affected, total, details, rows)
}

#' Cardinalite elevee pour une variable nominale
#'
#' Signale les variables declarees (ou inferees) nominales qui comptent
#' plus de 30 modalites non manquantes.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_high_cardinality <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)
  nominal_vars <- .variables_with_nature(names(data), profile, config, "nominale")

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in nominal_vars) {
    normalized <- normalize_missing(as.character(data[[variable]]))$x
    non_missing_idx <- which(!is.na(normalized))
    n_distinct <- length(unique(normalized[non_missing_idx]))
    if (n_distinct <= 30) {
      next
    }

    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(non_missing_idx))
    details <- c(details, sprintf("%d modalites pour une variable nominale (seuil : 30).", n_distinct))
    rows[[length(rows) + 1]] <- non_missing_idx
  }

  .build_anomaly_rows("high_cardinality", "avertissement", variables, n_affected, total, details, rows)
}

#' Valeurs hors des bornes physiologiques plausibles
#'
#' Compare les variables numeriques (continue, entiere) aux bornes
#' declarees dans `inst/rules/plausible_ranges.yml`, d'apres une
#' correspondance sur les jetons du nom de variable. Une variable dont le
#' nom ne correspond a aucune regle n'est pas verifiee.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_impossible_values <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)
  rules <- .load_plausible_ranges()
  numeric_vars <- .variables_with_nature(names(data), profile, config, c("continue", "entiere"))

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in numeric_vars) {
    range <- .match_plausible_range(variable, rules)
    if (is.null(range)) {
      next
    }

    normalized <- normalize_missing(as.character(data[[variable]]))$x
    values <- .to_numeric_permissive(normalized)
    out_of_range <- which(!is.na(values) & (values < range$min | values > range$max))
    if (length(out_of_range) == 0) {
      next
    }

    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(out_of_range))
    unit_txt <- if (!is.null(range$unite)) sprintf(" %s", range$unite) else ""
    details <- c(details, sprintf(
      "%d valeurs hors des bornes plausibles [%s, %s]%s.",
      length(out_of_range), range$min, range$max, unit_txt
    ))
    rows[[length(rows) + 1]] <- out_of_range
  }

  .build_anomaly_rows("impossible_values", "bloquant", variables, n_affected, total, details, rows)
}

#' Valeurs extremes (regle de Tukey)
#'
#' Signale les valeurs situees a plus de 3 ecarts interquartiles des
#' quartiles (regle de Tukey, k = 3). Les valeurs deja signalees par
#' [check_impossible_values()] pour la meme variable sont exclues : les
#' deux detecteurs restent distincts.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_outliers <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)
  rules <- .load_plausible_ranges()
  numeric_vars <- .variables_with_nature(names(data), profile, config, c("continue", "entiere"))

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in numeric_vars) {
    normalized <- normalize_missing(as.character(data[[variable]]))$x
    values <- .to_numeric_permissive(normalized)
    valid_idx <- which(!is.na(values))
    if (length(valid_idx) < 4) {
      next
    }

    quartiles <- stats::quantile(values[valid_idx], probs = c(0.25, 0.75), names = FALSE)
    iqr <- quartiles[2] - quartiles[1]
    if (iqr == 0) {
      next
    }

    lower <- quartiles[1] - 3 * iqr
    upper <- quartiles[2] + 3 * iqr
    outlier_idx <- valid_idx[values[valid_idx] < lower | values[valid_idx] > upper]

    range <- .match_plausible_range(variable, rules)
    if (!is.null(range)) {
      impossible_idx <- valid_idx[values[valid_idx] < range$min | values[valid_idx] > range$max]
      outlier_idx <- setdiff(outlier_idx, impossible_idx)
    }
    if (length(outlier_idx) == 0) {
      next
    }

    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(outlier_idx))
    details <- c(details, sprintf(
      "%d valeurs extremes (regle de Tukey, k=3, bornes [%.2f, %.2f]).",
      length(outlier_idx), lower, upper
    ))
    rows[[length(rows) + 1]] <- sort(outlier_idx)
  }

  .build_anomaly_rows("outliers", "avertissement", variables, n_affected, total, details, rows)
}

#' Coherence chronologique entre paires de dates
#'
#' Pour chaque paire de variables de nature `date`, determine si les
#' lignes completes (les deux dates renseignees) suivent majoritairement
#' (>= 90 %) un ordre chronologique donne, et signale les lignes qui vont
#' a l'encontre de cette majorite. Une paire de dates sans relation
#' chronologique dominante (mélange proche de 50/50) n'est pas signalee :
#' l'absence de majorite claire ne permet pas de conclure a une inversion.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_date_coherence <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)
  date_vars <- .variables_with_nature(names(data), profile, config, "date")

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  if (length(date_vars) >= 2) {
    parsed <- lapply(date_vars, function(v) parse_dates_robust(as.character(data[[v]]))$dates)
    names(parsed) <- date_vars

    pairs <- utils::combn(date_vars, 2, simplify = FALSE)
    for (pair in pairs) {
      first <- parsed[[pair[1]]]
      second <- parsed[[pair[2]]]
      both_present <- which(!is.na(first) & !is.na(second))
      if (length(both_present) < 4) {
        next
      }

      diff_days <- as.numeric(second[both_present] - first[both_present])
      non_null <- diff_days[diff_days != 0]
      if (length(non_null) < 4) {
        next
      }

      majority_positive <- mean(non_null > 0) >= 0.9
      majority_negative <- mean(non_null < 0) >= 0.9
      if (!majority_positive && !majority_negative) {
        next
      }

      reversed <- if (majority_positive) {
        both_present[diff_days < 0]
      } else {
        both_present[diff_days > 0]
      }
      if (length(reversed) == 0) {
        next
      }

      variables <- c(variables, paste(pair[1], "vs", pair[2]))
      n_affected <- c(n_affected, length(reversed))
      details <- c(details, sprintf(
        "%d lignes ou l'ordre chronologique majoritaire entre '%s' et '%s' est inverse.",
        length(reversed), pair[1], pair[2]
      ))
      rows[[length(rows) + 1]] <- reversed
    }
  }

  .build_anomaly_rows("date_coherence", "avertissement", variables, n_affected, total, details, rows)
}

#' Colonne texte majoritairement numerique
#'
#' Signale les variables de nature `texte` dont au moins 80 % des valeurs
#' non manquantes sont convertibles en nombre, ce qui suggere une nature
#' mal declaree plutot qu'un veritable champ libre.
#'
#' @inheritParams st_detect_anomalies
#' @return Un data.frame au format commun des detecteurs.
#' @keywords internal
check_numeric_in_text <- function(data, profile = NULL, config = NULL) {
  checkmate::assert_data_frame(data)
  total <- nrow(data)
  text_vars <- .variables_with_nature(names(data), profile, config, "texte")

  variables <- character(0)
  n_affected <- integer(0)
  details <- character(0)
  rows <- list()

  for (variable in text_vars) {
    normalized <- normalize_missing(as.character(data[[variable]]))$x
    non_missing_idx <- which(!is.na(normalized))
    if (length(non_missing_idx) == 0) {
      next
    }

    values <- .to_numeric_permissive(normalized[non_missing_idx])
    numeric_idx <- non_missing_idx[!is.na(values)]
    rate <- length(numeric_idx) / length(non_missing_idx)
    if (rate < 0.8) {
      next
    }

    variables <- c(variables, variable)
    n_affected <- c(n_affected, length(numeric_idx))
    details <- c(details, sprintf(
      "%.1f%% des valeurs non manquantes sont numeriques ; nature 'texte' probablement a revoir.",
      rate * 100
    ))
    rows[[length(rows) + 1]] <- numeric_idx
  }

  .build_anomaly_rows("numeric_in_text", "information", variables, n_affected, total, details, rows)
}

# --- Helpers internes ---------------------------------------------------------

.empty_anomaly_df <- function() {
  df <- data.frame(
    check_id = character(0),
    severity = character(0),
    variable = character(0),
    n_affected = integer(0),
    pct_affected = numeric(0),
    detail = character(0),
    stringsAsFactors = FALSE
  )
  df$rows_affected <- list()
  df
}

.build_anomaly_rows <- function(check_id, severity, variable, n_affected, total, detail, rows_affected) {
  n <- length(variable)
  if (n == 0) {
    return(.empty_anomaly_df())
  }

  df <- data.frame(
    check_id = rep(check_id, n),
    severity = rep(severity, length.out = n),
    variable = variable,
    n_affected = as.integer(n_affected),
    pct_affected = .safe_pct(n_affected, total),
    detail = detail,
    stringsAsFactors = FALSE
  )
  df$rows_affected <- rows_affected
  df
}

.safe_pct <- function(n, total) {
  if (total <= 0) {
    return(rep(0, length(n)))
  }
  n / total
}

.effective_nature <- function(variable, profile, config) {
  if (!is.null(config) && !is.null(config$dictionnaire) && !is.null(config$dictionnaire[[variable]]$nature)) {
    return(config$dictionnaire[[variable]]$nature)
  }
  if (!is.null(profile)) {
    idx <- which(profile$name == variable)
    if (length(idx) == 1) {
      return(profile$inferred_nature[idx])
    }
  }
  NA_character_
}

.variables_with_nature <- function(variable_names, profile, config, natures) {
  Filter(function(v) .effective_nature(v, profile, config) %in% natures, variable_names)
}

.load_plausible_ranges <- function() {
  path <- system.file("rules", "plausible_ranges.yml", package = "statlab")
  if (!nzchar(path) || !file.exists(path)) {
    return(list())
  }
  yaml::read_yaml(path)
}

.match_plausible_range <- function(variable, rules) {
  if (length(rules) == 0) {
    return(NULL)
  }
  tokens <- strsplit(tolower(variable), "[^a-z0-9]+")[[1]]

  for (rule in rules) {
    motifs <- tolower(rule$motifs)
    if (any(tokens %in% motifs)) {
      return(rule)
    }
  }
  NULL
}

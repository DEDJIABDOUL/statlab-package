# =============================================================================
# Assemblage de plusieurs sources : empilement, jointure, passage au format
# long. Module critique : une jointure qui multiplie ou perd des lignes en
# silence est la principale source d'erreurs dans ce type d'outil. Chaque
# operation produit donc un audit obligatoire (comptages, orphelins,
# ratio d'expansion) qui est a la fois retourne et journalise. Aucune
# fusion approximative n'est appliquee automatiquement : les suggestions
# issues de stringdist sont uniquement signalees.
# =============================================================================

.KEY_SEPARATOR <- ""

#' Assembler plusieurs sources selon la configuration
#'
#' Execute, dans l'ordre declare, les operations de la section
#' `reconciliation` de `config.yml` (empiler, joindre, pivoter_long). Le
#' `resultat` de chaque operation devient disponible comme source pour les
#' operations suivantes, au meme titre que les identifiants de `sources`.
#'
#' @param sources Liste nommee de data.frame (typiquement le resultat de
#'   [st_read_all_sources()]).
#' @param config Un objet `statlab_config` valide, tel que retourne par
#'   [st_validate_config()], dont la section `reconciliation` declare les
#'   operations a executer.
#'
#' @return Une liste avec `table` (le data.frame produit par la derniere
#'   operation) et `audit` (une liste d'audits, un par operation, nommee
#'   par le `resultat` de chaque operation).
#' @export
st_reconcile <- function(sources, config) {
  checkmate::assert_list(sources, types = "data.frame", min.len = 1, names = "unique")
  checkmate::assert_class(config, "statlab_config")
  if (!isTRUE(attr(config, "valid"))) {
    cli::cli_abort("La configuration doit etre validee (st_validate_config()) avant la reconciliation.")
  }

  operations <- config$reconciliation
  if (is.null(operations) || length(operations) == 0) {
    cli::cli_abort("Aucune operation de reconciliation n'est declaree dans config.yml (section 'reconciliation').")
  }

  tables <- sources
  audits <- list()

  for (operation in operations) {
    resultat <- switch(operation$operation,
      empiler = .apply_stack(tables, operation),
      joindre = .apply_join(tables, operation),
      pivoter_long = .apply_pivot(tables, operation)
    )

    tables[[operation$resultat]] <- resultat$table
    audits[[operation$resultat]] <- resultat$audit

    st_log(
      sprintf("reconciliation_%s", operation$operation),
      module = "reconcile",
      resultat = operation$resultat,
      n_lignes = nrow(resultat$table),
      n_colonnes = ncol(resultat$table),
      level = "info"
    )
  }

  last_result_name <- operations[[length(operations)]]$resultat
  list(table = tables[[last_result_name]], audit = audits)
}

#' Normaliser une cle d'appariement
#'
#' Supprime les espaces (de bord et internes), met en majuscules, supprime
#' les caracteres non alphanumeriques, puis harmonise les zeros initiaux
#' des cles entierement numeriques (`"007"` et `"7"` deviennent `"7"`). La
#' cle originale n'est jamais modifiee dans les donnees : seule la
#' correspondance s'appuie sur la forme normalisee.
#'
#' @param x Un vecteur (converti en caractere).
#' @return Un vecteur de caracteres normalise, meme longueur que `x`.
#' @keywords internal
normalize_key <- function(x) {
  cleaned <- toupper(as.character(x))
  cleaned <- gsub("[^A-Z0-9]+", "", cleaned)
  is_numeric <- !is.na(cleaned) & grepl("^[0-9]+$", cleaned)
  cleaned[is_numeric] <- sub("^0+(?=[0-9])", "", cleaned[is_numeric], perl = TRUE)
  cleaned
}

# --- Operations -----------------------------------------------------------

.apply_stack <- function(tables, operation) {
  ids <- operation$sources
  missing_ids <- setdiff(ids, names(tables))
  if (length(missing_ids) > 0) {
    cli::cli_abort("Source(s) introuvable(s) pour l'operation 'empiler' : {paste(missing_ids, collapse = ', ')}.")
  }

  data_frames <- tables[ids]
  columns_per_source <- lapply(data_frames, names)
  all_columns <- Reduce(union, columns_per_source)
  common_columns <- Reduce(intersect, columns_per_source)
  divergent_columns <- setdiff(all_columns, common_columns)

  mode <- operation$sur_colonnes_divergentes
  if (length(divergent_columns) > 0) {
    if (mode == "erreur") {
      cli::cli_abort(c(
        "Colonnes divergentes entre les sources a empiler pour {.field {operation$resultat}} : {paste(divergent_columns, collapse = ', ')}.",
        "i" = "Declarer 'sur_colonnes_divergentes: intersection' ou 'union' dans config.yml pour resoudre automatiquement."
      ))
    } else if (mode == "intersection") {
      data_frames <- lapply(data_frames, function(df) df[, common_columns, drop = FALSE])
    } else {
      data_frames <- lapply(data_frames, function(df) {
        missing_columns <- setdiff(all_columns, names(df))
        for (column in missing_columns) {
          df[[column]] <- NA_character_
        }
        df[, all_columns, drop = FALSE]
      })
    }
  }

  result <- dplyr::bind_rows(data_frames, .id = ".source_empilee")

  audit <- list(
    operation = "empiler",
    resultat = operation$resultat,
    sources = ids,
    n_par_source = stats::setNames(vapply(data_frames, nrow, integer(1)), ids),
    n_resultat = nrow(result),
    colonnes_divergentes = divergent_columns,
    resolution = if (length(divergent_columns) > 0) mode else "aucune_divergence"
  )

  list(table = result, audit = audit)
}

.apply_join <- function(tables, operation) {
  left_id <- operation$gauche
  right_id <- operation$droite
  if (!left_id %in% names(tables)) {
    cli::cli_abort("Source introuvable pour l'operation 'joindre' (gauche) : '{left_id}'.")
  }
  if (!right_id %in% names(tables)) {
    cli::cli_abort("Source introuvable pour l'operation 'joindre' (droite) : '{right_id}'.")
  }

  left <- tables[[left_id]]
  right <- tables[[right_id]]
  keys <- operation$cle
  normalize <- isTRUE(operation$normaliser_cle)

  missing_left <- setdiff(keys, names(left))
  missing_right <- setdiff(keys, names(right))
  if (length(missing_left) > 0) {
    cli::cli_abort("Cle(s) absente(s) de la source '{left_id}' : {paste(missing_left, collapse = ', ')}.")
  }
  if (length(missing_right) > 0) {
    cli::cli_abort("Cle(s) absente(s) de la source '{right_id}' : {paste(missing_right, collapse = ', ')}.")
  }

  by_columns <- keys
  if (normalize) {
    normalized_columns <- paste0(".cle_normalisee_", seq_along(keys))
    for (i in seq_along(keys)) {
      left[[normalized_columns[i]]] <- normalize_key(left[[keys[i]]])
      right[[normalized_columns[i]]] <- normalize_key(right[[keys[i]]])
    }
    by_columns <- normalized_columns
  }

  n_left <- nrow(left)
  n_right <- nrow(right)

  left_key <- .paste_key(left, by_columns)
  right_key <- .paste_key(right, by_columns)

  n_matched <- sum(left_key %in% right_key)
  left_orphan_idx <- which(!(left_key %in% right_key))
  right_orphan_idx <- which(!(right_key %in% left_key))

  right_key_counts <- table(right_key)
  duplicated_right_keys <- right_key_counts[right_key_counts > 1]

  suffix <- c("_gauche", "_droite")
  column_conflicts <- intersect(setdiff(names(left), by_columns), setdiff(names(right), by_columns))

  join_fn <- switch(operation$type,
    gauche = powerjoin::power_left_join,
    interieure = powerjoin::power_inner_join,
    complete = powerjoin::power_full_join
  )

  result <- join_fn(
    left, right,
    by = by_columns, suffix = suffix,
    check = powerjoin::check_specs(
      implicit_keys = "ignore", column_conflict = "ignore",
      duplicate_keys_left = "ignore", duplicate_keys_right = "ignore",
      unmatched_keys_left = "ignore", unmatched_keys_right = "ignore"
    )
  )

  if (normalize) {
    result[normalized_columns] <- NULL
  }

  n_result <- nrow(result)
  expansion_ratio <- if (n_left > 0) n_result / n_left else NA_real_

  # L'explosion pertinente est celle causee par une cle dupliquee cote droit qui
  # correspond effectivement a une ligne gauche (fan-out reel). Un simple ajout
  # de lignes orphelines de droite (jointure "complete") augmente aussi le
  # ratio sans etre une explosion : ce n'est pas ce que ce garde-fou surveille.
  matched_duplicated_right_keys <- duplicated_right_keys[names(duplicated_right_keys) %in% unique(left_key)]
  explosion_detected <- length(matched_duplicated_right_keys) > 0

  if (explosion_detected && isTRUE(operation$alerte_explosion)) {
    cli::cli_abort(c(
      "Explosion de jointure pour {.field {operation$resultat}} : le resultat ({n_result} lignes) depasse la table gauche ({n_left} lignes).",
      "i" = "Cles dupliquees cote droit ('{right_id}') : {paste(sprintf('%s (x%d)', names(matched_duplicated_right_keys), as.integer(matched_duplicated_right_keys)), collapse = ', ')}",
      "i" = "Declarer 'alerte_explosion: false' dans config.yml si cette multiplication est attendue."
    ))
  }

  normalization_only_matches <- if (normalize) {
    .detect_normalization_only_matches(left, right, keys, by_columns)
  } else {
    data.frame()
  }

  approximate_suggestions <- .suggest_approximate_matches(
    left, right, keys, left_orphan_idx, right_orphan_idx
  )

  audit <- list(
    operation = "joindre",
    resultat = operation$resultat,
    n_gauche = n_left,
    n_droite = n_right,
    n_resultat = n_result,
    n_apparies = n_matched,
    n_orphelins_gauche = length(left_orphan_idx),
    n_orphelins_droite = length(right_orphan_idx),
    orphelins_gauche = .key_excerpt(left, keys, left_orphan_idx),
    orphelins_droite = .key_excerpt(right, keys, right_orphan_idx),
    ratio_expansion = expansion_ratio,
    cles_dupliquees_droite = as.list(duplicated_right_keys),
    conflits_colonnes = column_conflicts,
    suffixes = suffix,
    appariements_apres_normalisation = normalization_only_matches,
    suggestions_appariement_approximatif = approximate_suggestions
  )

  list(table = result, audit = audit)
}

.apply_pivot <- function(tables, operation) {
  source_id <- operation$source
  if (!source_id %in% names(tables)) {
    cli::cli_abort("Source introuvable pour l'operation 'pivoter_long' : '{source_id}'.")
  }
  data <- tables[[source_id]]

  missing_keys <- setdiff(operation$cles, names(data))
  missing_measures <- setdiff(operation$mesures, names(data))
  if (length(missing_keys) > 0) {
    cli::cli_abort("Cle(s) de regroupement absente(s) de '{source_id}' : {paste(missing_keys, collapse = ', ')}.")
  }
  if (length(missing_measures) > 0) {
    cli::cli_abort("Colonne(s) de mesure absente(s) de '{source_id}' : {paste(missing_measures, collapse = ', ')}.")
  }

  result <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(operation$mesures),
    names_to = operation$nom_temps,
    values_to = operation$nom_valeur
  )

  audit <- list(
    operation = "pivoter_long",
    resultat = operation$resultat,
    source = source_id,
    n_lignes_avant = nrow(data),
    n_colonnes_avant = ncol(data),
    n_lignes_apres = nrow(result),
    n_colonnes_apres = ncol(result),
    mesures_pivotees = operation$mesures
  )

  list(table = result, audit = audit)
}

# --- Helpers internes -------------------------------------------------------

.paste_key <- function(data, columns) {
  do.call(paste, c(lapply(columns, function(column) data[[column]]), sep = .KEY_SEPARATOR))
}

.key_excerpt <- function(data, keys, idx, limit = 100) {
  idx <- utils::head(idx, limit)
  if (length(idx) == 0) {
    return(data[0, keys, drop = FALSE])
  }
  data[idx, keys, drop = FALSE]
}

.detect_normalization_only_matches <- function(left, right, keys, normalized_columns) {
  left_normalized <- .paste_key(left, normalized_columns)
  right_normalized <- .paste_key(right, normalized_columns)
  left_raw <- .paste_key(left, keys)
  right_raw <- .paste_key(right, keys)

  common_keys <- intersect(unique(left_normalized), unique(right_normalized))
  matches <- list()

  for (key in common_keys) {
    left_forms <- unique(left_raw[left_normalized == key])
    right_forms <- unique(right_raw[right_normalized == key])
    observed_forms <- union(left_forms, right_forms)

    if (length(observed_forms) > 1) {
      matches[[length(matches) + 1]] <- data.frame(
        cle_normalisee = gsub(.KEY_SEPARATOR, " / ", key, fixed = TRUE),
        formes_observees = paste(gsub(.KEY_SEPARATOR, " / ", observed_forms, fixed = TRUE), collapse = " ; "),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(matches) == 0) {
    return(data.frame(cle_normalisee = character(0), formes_observees = character(0)))
  }
  do.call(rbind, matches)
}

.suggest_approximate_matches <- function(left, right, keys, left_orphan_idx, right_orphan_idx, max_distance = 2, max_pairs = 500) {
  empty <- data.frame(cle_gauche = character(0), cle_droite = character(0), distance = numeric(0))
  if (length(left_orphan_idx) == 0 || length(right_orphan_idx) == 0) {
    return(empty)
  }
  if (length(left_orphan_idx) > max_pairs || length(right_orphan_idx) > max_pairs) {
    return(empty)
  }

  left_keys <- .paste_key(left[left_orphan_idx, , drop = FALSE], keys)
  right_keys <- .paste_key(right[right_orphan_idx, , drop = FALSE], keys)
  left_keys <- gsub(.KEY_SEPARATOR, " / ", left_keys, fixed = TRUE)
  right_keys <- gsub(.KEY_SEPARATOR, " / ", right_keys, fixed = TRUE)

  matched <- stringdist::amatch(left_keys, right_keys, maxDist = max_distance, method = "osa")
  found <- which(!is.na(matched))
  if (length(found) == 0) {
    return(empty)
  }

  data.frame(
    cle_gauche = left_keys[found],
    cle_droite = right_keys[matched[found]],
    distance = stringdist::stringdist(left_keys[found], right_keys[matched[found]], method = "osa"),
    stringsAsFactors = FALSE
  )
}

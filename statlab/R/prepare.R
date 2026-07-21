# =============================================================================
# Transformations declarees en config, section `preparation`. Ordre fixe
# d'execution : variables, dates, recodages, derivations, classes,
# exclusions, manquants. Chaque verbe est declaratif : aucune decision
# n'existe en dehors de config.yml, et la strategie de manquants n'est
# jamais implicite (cf. .apply_missing_strategy()).
# =============================================================================

.ALLOWED_FORMULA_FUNCTIONS <- c(
  "+", "-", "*", "/", "^", "%%", "%/%",
  "==", "!=", "<", ">", "<=", ">=",
  "&", "|", "!", "(",
  "as.numeric", "as.Date", "ifelse", "round", "log", "sqrt", "abs",
  "difftime", "pmin", "pmax"
)

#' Appliquer les transformations de preparation declarees en config
#'
#' Applique, dans cet ordre fixe, les transformations de la section
#' `preparation` de `config.yml` : selection/renommage de variables,
#' conversion de dates, recodages, variables derivees, decoupage en
#' classes, exclusions de lignes, puis traitement des valeurs manquantes.
#'
#' La strategie de manquants n'est jamais implicite : si, apres toutes les
#' autres etapes, une variable comporte des valeurs manquantes sans
#' strategie declaree (ni specifique, ni "catch-all"), l'execution
#' s'arrete avec un message explicite.
#'
#' @param data Un data.frame (typiquement le resultat de
#'   [st_read_source()] ou de [st_reconcile()]).
#' @param config Un objet `statlab_config` valide, tel que retourne par
#'   [st_validate_config()].
#'
#' @return Le data.frame transforme.
#' @export
st_prepare <- function(data, config) {
  checkmate::assert_data_frame(data)
  checkmate::assert_class(config, "statlab_config")
  if (!isTRUE(attr(config, "valid"))) {
    cli::cli_abort("La configuration doit etre validee (st_validate_config()) avant la preparation.")
  }

  preparation <- config$preparation

  if (!is.null(preparation$variables)) {
    data <- .apply_variables_section(data, preparation$variables)
  }
  if (!is.null(preparation$dates)) {
    data <- .apply_dates_section(data, preparation$dates)
  }
  if (!is.null(preparation$recodages)) {
    data <- .apply_recodages_section(data, preparation$recodages)
  }
  if (!is.null(preparation$derivations)) {
    data <- .apply_derivations_section(data, preparation$derivations)
  }
  if (!is.null(preparation$classes)) {
    data <- .apply_classes_section(data, preparation$classes)
  }
  if (!is.null(preparation$exclusions)) {
    data <- .apply_exclusions_section(data, preparation$exclusions)
  }

  data <- .apply_missing_strategy(data, preparation$manquants)

  data
}

# --- variables ---------------------------------------------------------------

.apply_variables_section <- function(data, spec) {
  if (!is.null(spec$selectionner)) {
    missing_vars <- setdiff(spec$selectionner, names(data))
    if (length(missing_vars) > 0) {
      cli::cli_abort("Variable(s) a selectionner introuvable(s) : {paste(missing_vars, collapse = ', ')}.")
    }
    data <- data[, spec$selectionner, drop = FALSE]
  }

  if (!is.null(spec$renommer)) {
    for (ancien in names(spec$renommer)) {
      if (!ancien %in% names(data)) {
        cli::cli_abort("Variable a renommer introuvable : '{ancien}'.")
      }
      names(data)[names(data) == ancien] <- spec$renommer[[ancien]]
    }
  }

  data
}

# --- dates ---------------------------------------------------------------------

.apply_dates_section <- function(data, spec) {
  for (variable in names(spec)) {
    if (!variable %in% names(data)) {
      cli::cli_abort("Variable de date introuvable : '{variable}'.")
    }
    expected_format <- spec[[variable]]$format
    raw_values <- as.character(data[[variable]])
    n_missing_before <- sum(is.na(normalize_missing(raw_values)$x))

    converted <- as.Date(raw_values, format = expected_format)
    n_missing_after <- sum(is.na(converted))

    if (n_missing_after > n_missing_before) {
      cli::cli_abort(c(
        "Echec de conversion de date pour '{variable}' avec le format '{expected_format}'.",
        "i" = "{n_missing_after - n_missing_before} valeur(s) non convertie(s) alors qu'elles n'etaient pas manquantes a l'origine."
      ))
    }
    data[[variable]] <- converted
  }
  data
}

# --- recodages -----------------------------------------------------------------

.apply_recodages_section <- function(data, spec) {
  for (variable in names(spec)) {
    if (!variable %in% names(data)) {
      cli::cli_abort("Variable a recoder introuvable : '{variable}'.")
    }
    entry <- spec[[variable]]
    x <- as.character(data[[variable]])

    if (!is.null(entry$fusionner)) {
      x_factor <- factor(x)
      x_factor <- do.call(forcats::fct_collapse, c(list(.f = x_factor), entry$fusionner))
      x <- as.character(x_factor)
    }

    if (!is.null(entry$ordre)) {
      uncovered <- setdiff(unique(x[!is.na(x)]), entry$ordre)
      if (length(uncovered) > 0) {
        cli::cli_abort(c(
          "Valeur(s) non couverte(s) par 'ordre' pour '{variable}' : {paste(uncovered, collapse = ', ')}.",
          "i" = "Completer 'ordre' (ou 'fusionner') dans config.yml pour couvrir toutes les modalites observees."
        ))
      }
      x <- factor(x, levels = entry$ordre)
    }

    data[[variable]] <- x
  }
  data
}

# --- derivations -----------------------------------------------------------------

.apply_derivations_section <- function(data, derivations) {
  for (entry in derivations) {
    value <- tryCatch(
      .safe_eval(entry$formule, data, .ALLOWED_FORMULA_FUNCTIONS),
      error = function(e) {
        cli::cli_abort("Le calcul de la variable derivee '{entry$nom}' a echoue : {conditionMessage(e)}")
      }
    )
    data[[entry$nom]] <- value
  }
  data
}

# --- classes ---------------------------------------------------------------------

.apply_classes_section <- function(data, spec) {
  for (variable in names(spec)) {
    if (!variable %in% names(data)) {
      cli::cli_abort("Variable a classer introuvable : '{variable}'.")
    }
    entry <- spec[[variable]]
    numeric_values <- .to_numeric_permissive(as.character(data[[variable]]))

    if (!is.null(entry$seuils)) {
      breaks <- c(-Inf, entry$seuils, Inf)
      data[[variable]] <- cut(numeric_values, breaks = breaks, labels = entry$libelles, right = FALSE)
    } else {
      probs <- seq(0, 1, length.out = entry$n + 1)
      breaks <- stats::quantile(numeric_values, probs = probs, na.rm = TRUE, names = FALSE)
      unique_breaks <- unique(breaks)
      if (length(unique_breaks) - 1 < entry$n) {
        cli::cli_abort(c(
          "Impossible de former {entry$n} classes de quantiles pour '{variable}'.",
          "i" = "Trop de valeurs identiques : seulement {length(unique_breaks) - 1} classe(s) distincte(s) possible(s)."
        ))
      }
      labels <- paste0("Q", seq_len(entry$n))
      data[[variable]] <- cut(numeric_values, breaks = unique_breaks, labels = labels, include.lowest = TRUE)
    }
  }
  data
}

# --- exclusions -----------------------------------------------------------------

.apply_exclusions_section <- function(data, exclusions) {
  for (entry in exclusions) {
    result <- tryCatch(
      .safe_eval(entry$condition, data, .ALLOWED_FORMULA_FUNCTIONS),
      error = function(e) {
        cli::cli_abort("L'evaluation de la condition d'exclusion '{entry$condition}' a echoue : {conditionMessage(e)}")
      }
    )
    if (!is.logical(result)) {
      cli::cli_abort("La condition d'exclusion '{entry$condition}' ne produit pas un resultat logique (TRUE/FALSE).")
    }
    exclude <- result
    exclude[is.na(exclude)] <- FALSE

    n_before <- nrow(data)
    data <- data[!exclude, , drop = FALSE]
    n_after <- nrow(data)

    st_log_exclusion(n_before, n_after, entry$motif)
  }
  data
}

# --- manquants -----------------------------------------------------------------

.apply_missing_strategy <- function(data, manquants_specs) {
  if (is.null(manquants_specs)) {
    manquants_specs <- list()
  }

  explicit_map <- list()
  catch_all_strategy <- NULL
  for (spec in manquants_specs) {
    if (is.null(spec$variables)) {
      catch_all_strategy <- spec$strategie
    } else {
      for (variable in spec$variables) {
        explicit_map[[variable]] <- spec$strategie
      }
    }
  }

  missing_counts <- stats::setNames(
    vapply(names(data), function(variable) {
      sum(is.na(normalize_missing(as.character(data[[variable]]))$x))
    }, integer(1)),
    names(data)
  )

  uncovered <- Filter(function(variable) {
    missing_counts[[variable]] > 0 && is.null(explicit_map[[variable]]) && is.null(catch_all_strategy)
  }, names(data))

  if (length(uncovered) > 0) {
    cli::cli_abort(c(
      "Des valeurs manquantes existent sans strategie declaree : {paste(uncovered, collapse = ', ')}.",
      "i" = "Declarer une entree 'preparation.manquants' (par variable, ou sans 'variables' pour un catch-all) dans config.yml."
    ))
  }

  for (variable in names(data)) {
    strategy <- if (!is.null(explicit_map[[variable]])) explicit_map[[variable]] else catch_all_strategy
    if (is.null(strategy) || missing_counts[[variable]] == 0) {
      next
    }
    # Seules les colonnes encore en caractere portent des codes de manquant
    # bruts ("999", "NR", ...). Une colonne deja typee (Date, facteur) par
    # une etape anterieure (dates, recodages) a deja un NA canonique : la
    # reconvertir en caractere ici ecraserait silencieusement son type.
    if (is.character(data[[variable]])) {
      data[[variable]] <- normalize_missing(data[[variable]])$x
    }
  }

  for (spec in manquants_specs) {
    target_vars <- if (is.null(spec$variables)) {
      setdiff(names(data), names(explicit_map))
    } else {
      spec$variables
    }
    target_vars <- intersect(target_vars, names(data))
    target_vars <- target_vars[missing_counts[target_vars] > 0]
    if (length(target_vars) == 0) {
      next
    }

    if (spec$strategie == "exclure_ligne") {
      n_before <- nrow(data)
      missing_mask <- Reduce(`|`, lapply(target_vars, function(variable) is.na(data[[variable]])))
      data <- data[!missing_mask, , drop = FALSE]
      n_after <- nrow(data)
      st_log_exclusion(n_before, n_after, sprintf("Valeurs manquantes : %s", paste(target_vars, collapse = ", ")))
    } else if (spec$strategie == "imputer") {
      for (variable in target_vars) {
        data[[variable]] <- .impute_variable(data[[variable]], variable)
      }
    }
  }

  data
}

.impute_variable <- function(x, variable_name) {
  non_missing <- x[!is.na(x)]
  if (length(non_missing) == 0) {
    cli::cli_abort("Imputation impossible pour '{variable_name}' : aucune valeur non manquante disponible.")
  }

  if (is.numeric(x) || inherits(x, "Date")) {
    imputed_value <- stats::median(non_missing)
    x[is.na(x)] <- imputed_value
    method <- "mediane"
  } else if (is.factor(x)) {
    frequencies <- sort(table(non_missing), decreasing = TRUE)
    imputed_value <- names(frequencies)[1]
    x[is.na(x)] <- imputed_value
    method <- "mode"
  } else {
    numeric_values <- suppressWarnings(as.numeric(non_missing))
    if (!anyNA(numeric_values)) {
      imputed_value <- stats::median(numeric_values)
      x[is.na(x)] <- as.character(imputed_value)
      method <- "mediane"
    } else {
      frequencies <- sort(table(non_missing), decreasing = TRUE)
      imputed_value <- names(frequencies)[1]
      x[is.na(x)] <- imputed_value
      method <- "mode"
    }
  }

  st_log(
    "imputation",
    module = "prepare", variable = variable_name, methode = method,
    valeur = as.character(imputed_value), level = "info"
  )
  x
}

# --- evaluation securisee d'expressions (derivations, exclusions) ---------------

.collect_function_calls <- function(expr) {
  if (is.call(expr)) {
    fn <- expr[[1]]
    fn_name <- if (is.symbol(fn)) as.character(fn) else "<expression_non_supportee>"
    rest <- unlist(lapply(as.list(expr)[-1], .collect_function_calls), use.names = FALSE)
    c(fn_name, rest)
  } else {
    character(0)
  }
}

.safe_eval <- function(formula_text, data, allowed_functions) {
  parsed <- tryCatch(
    parse(text = formula_text),
    error = function(e) cli::cli_abort("Expression invalide : '{formula_text}' ({conditionMessage(e)}).")
  )
  if (length(parsed) != 1) {
    cli::cli_abort("L'expression '{formula_text}' doit contenir une seule instruction R (aucun ';' ou saut de ligne separant plusieurs instructions).")
  }
  expr <- parsed[[1]]

  called_functions <- unique(.collect_function_calls(expr))
  disallowed <- setdiff(called_functions, allowed_functions)
  if (length(disallowed) > 0) {
    cli::cli_abort(c(
      "Fonction(s) non autorisee(s) dans l'expression '{formula_text}' : {paste(disallowed, collapse = ', ')}.",
      "i" = "Fonctions autorisees : {paste(allowed_functions, collapse = ', ')}."
    ))
  }

  eval_env <- new.env(parent = emptyenv())
  for (fn_name in allowed_functions) {
    if (exists(fn_name, envir = baseenv(), mode = "function", inherits = FALSE)) {
      assign(fn_name, get(fn_name, envir = baseenv(), mode = "function"), envir = eval_env)
    }
  }

  eval(expr, envir = data, enclos = eval_env)
}

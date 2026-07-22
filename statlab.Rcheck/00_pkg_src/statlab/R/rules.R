# =============================================================================
# Moteur de regles methodologiques. Actif differenciant du projet : il doit
# rester DETERMINISTE (aucun modele probabiliste, aucune heuristique floue,
# aucun appel externe). Chaque regle est une condition booleenne evaluee sur
# un contexte de valeurs deja calculees ; l'evaluation reutilise le meme
# evaluateur restreint (liste blanche de fonctions) que R/prepare.R.
# =============================================================================

.ALLOWED_RULE_FUNCTIONS <- c(
  "==", "!=", "<", ">", "<=", ">=",
  "&&", "||", "&", "|", "!", "(",
  "is.na", "is.null", "isTRUE", "isFALSE"
)

#' Charger le referentiel methodologique
#'
#' Lit et valide `inst/rules/methodology.yml` (ou le chemin fourni). Le
#' referentiel est verse et chaque regle y est validee : champs connus,
#' severite dans l'enumeration attendue, justification presente des que
#' 'action' est declaree.
#'
#' @param path Chemin (chr) vers un fichier de referentiel alternatif. Si
#'   `NULL` (par defaut), le referentiel embarque dans le package est
#'   utilise.
#'
#' @return Un objet de classe `statlab_rules` (une liste de regles), avec
#'   l'attribut `version`.
#' @export
st_load_rules <- function(path = NULL) {
  checkmate::assert_string(path, null.ok = TRUE)

  if (is.null(path)) {
    path <- system.file("rules", "methodology.yml", package = "statlab")
  }
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Referentiel methodologique introuvable : {.path {path}}")
  }

  raw <- yaml::read_yaml(path)
  if (is.null(raw$version) || !is.character(raw$version) || length(raw$version) != 1) {
    cli::cli_abort("Le referentiel methodologique doit declarer un champ {.field version} (chaine) en tete de fichier.")
  }
  if (is.null(raw$regles) || !is.list(raw$regles) || length(raw$regles) == 0) {
    cli::cli_abort("Le referentiel methodologique doit declarer un champ {.field regles} non vide.")
  }

  rules <- lapply(seq_along(raw$regles), function(i) {
    .validate_rule(raw$regles[[i]], sprintf("regles[%d]", i))
  })

  ids <- vapply(rules, function(r) r$id, character(1))
  if (any(duplicated(ids))) {
    cli::cli_abort("Identifiant(s) de regle duplique(s) dans le referentiel : {paste(unique(ids[duplicated(ids)]), collapse = ', ')}.")
  }
  names(rules) <- ids

  structure(rules, class = "statlab_rules", version = raw$version, path = path)
}

#' Evaluer le referentiel methodologique sur un contexte
#'
#' Evalue chaque regle applicable (eventuellement filtree par `family`)
#' sur `context`. Comportements obligatoires :
#' - une regle bloquante SANS action qui se declenche arrete l'execution ;
#' - une regle bloquante AVEC action qui se declenche applique l'action et
#'   consigne la justification (journal, niveau "info"), en interpolant
#'   les valeurs du contexte ;
#' - un identifiant de regle present dans `override` est traite comme une
#'   derogation : la regle n'arrete pas l'execution et n'applique pas son
#'   action, mais la derogation est journalisee (niveau "derogation") avec
#'   l'identifiant de la regle contournee ;
#' - les regles non bloquantes (avertissement, information) sont
#'   journalisees mais n'arretent jamais l'execution.
#'
#' @param context Liste nommee de valeurs (numeriques, logiques ou
#'   caracteres) decrivant la situation a evaluer (ex : `test_type`,
#'   `is_continuous`, `min_group_n`, `shapiro_p`, ...).
#' @param family Famille de regles (chr) a evaluer. Si `NULL`, toutes les
#'   regles du referentiel sont considerees.
#' @param override Vecteur (chr) d'identifiants de regles a contourner
#'   (derogation operateur, ex : `analyse.comparaisons[].forcer_test`).
#'
#' @return Un data.frame des regles declenchees, trie par severite
#'   (bloquant, puis avertissement, puis information), avec les colonnes
#'   `id`, `famille`, `severite`, `message`, `action`, `justification`,
#'   `source`, `derogation`.
#' @export
st_evaluate_rules <- function(context, family = NULL, override = character(0)) {
  checkmate::assert_list(context, names = "unique")
  checkmate::assert_string(family, null.ok = TRUE)
  checkmate::assert_character(override, any.missing = FALSE)

  rules <- st_load_rules()
  if (!is.null(family)) {
    rules <- Filter(function(r) identical(r$famille, family), rules)
  }

  triggered <- list()
  for (rule in rules) {
    condition_expr <- parse(text = rule$condition)[[1]]
    required_vars <- unique(.collect_referenced_symbols(condition_expr))
    if (length(setdiff(required_vars, names(context))) > 0) {
      # Regle non pertinente pour ce contexte (variable absente) : ignoree
      # silencieusement, plutot qu'une erreur - un contexte partiel est
      # attendu lorsque plusieurs familles/formes de test coexistent dans
      # le meme referentiel.
      next
    }

    is_triggered <- tryCatch(
      isTRUE(.safe_eval(rule$condition, context, .ALLOWED_RULE_FUNCTIONS)),
      error = function(e) {
        cli::cli_abort("L'evaluation de la regle '{rule$id}' a echoue : {conditionMessage(e)}")
      }
    )
    if (!is_triggered) {
      next
    }

    message_text <- .interpolate(rule$message, context)
    is_overridden <- rule$id %in% override

    if (is_overridden) {
      st_log(
        "derogation_regle",
        module = "rules", regle = rule$id, severite = rule$severite,
        level = "derogation"
      )
      triggered[[rule$id]] <- .rule_result(rule, message_text, resolved = "derogation")
      next
    }

    if (identical(rule$severite, "bloquant")) {
      if (is.null(rule$action)) {
        cli::cli_abort(c(
          "Regle bloquante '{rule$id}' declenchee sans action de remediation : {message_text}",
          "i" = "Forcer un choix operateur (config 'forcer_test') si cette regle doit etre contournee."
        ))
      }
      justification_text <- .interpolate(rule$justification, context)
      st_log(
        "regle_appliquee",
        module = "rules", regle = rule$id, action = rule$action,
        justification = justification_text, level = "info"
      )
      triggered[[rule$id]] <- .rule_result(rule, message_text, resolved = "action", justification_text)
    } else {
      level <- if (identical(rule$severite, "avertissement")) "warn" else "info"
      st_log("regle_signalee", module = "rules", regle = rule$id, level = level)
      triggered[[rule$id]] <- .rule_result(rule, message_text, resolved = "aucune")
    }
  }

  if (length(triggered) == 0) {
    return(.empty_triggered_rules_df())
  }

  result <- do.call(rbind, triggered)
  severity_order <- c(bloquant = 1L, avertissement = 2L, information = 3L)
  result <- result[order(severity_order[result$severite]), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Lister le referentiel methodologique en tableau lisible
#'
#' Produit un data.frame du referentiel, destine a l'audit externe par un
#' biostatisticien (revue methodologique hors du code R).
#'
#' @inheritParams st_load_rules
#'
#' @return Un data.frame avec les colonnes `id`, `famille`, `severite`,
#'   `condition`, `seuils`, `message`, `action`, `source`.
#' @export
st_rules_report <- function(path = NULL) {
  rules <- st_load_rules(path)

  data.frame(
    id = vapply(rules, function(r) r$id, character(1)),
    famille = vapply(rules, function(r) r$famille, character(1)),
    severite = vapply(rules, function(r) r$severite, character(1)),
    condition = vapply(rules, function(r) r$condition, character(1)),
    seuils = vapply(rules, function(r) .format_seuils(r$seuils), character(1)),
    message = vapply(rules, function(r) r$message, character(1)),
    action = vapply(rules, function(r) if (is.null(r$action)) NA_character_ else r$action, character(1)),
    source = vapply(rules, function(r) if (is.null(r$source)) NA_character_ else r$source, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# --- Validation des regles -----------------------------------------------------

.validate_rule <- function(rule, path) {
  if (!is.list(rule)) {
    cli::cli_abort("L'entree {.field {path}} doit etre une liste de champs.")
  }
  known_fields <- c("id", "famille", "condition", "seuils", "severite", "message", "action", "justification", "source")
  field_names <- names(rule)
  if (is.null(field_names)) field_names <- character(0)
  unknown <- setdiff(field_names[field_names != ""], known_fields)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Champ inconnu dans la regle : {.field {path}.{unknown[1]}}",
      "i" = "Champs valides : {paste(known_fields, collapse = ', ')}"
    ))
  }

  .assert_rule_string(rule$id, paste0(path, ".id"))
  .assert_rule_string(rule$famille, paste0(path, ".famille"))
  .assert_rule_string(rule$condition, paste0(path, ".condition"))
  .assert_rule_string(rule$message, paste0(path, ".message"))

  if (!rule$severite %in% c("bloquant", "avertissement", "information")) {
    cli::cli_abort(c(
      "Valeur invalide pour {.field {path}.severite} : '{rule$severite}'",
      "i" = "Valeurs valides : bloquant, avertissement, information"
    ))
  }

  if (!is.null(rule$seuils)) {
    if (!is.list(rule$seuils)) {
      cli::cli_abort("Le champ {.field {path}.seuils} doit etre une liste nommee de seuils numeriques.")
    }
    if (length(rule$seuils) > 0) {
      seuil_names <- names(rule$seuils)
      if (is.null(seuil_names) || any(seuil_names == "")) {
        cli::cli_abort("Le champ {.field {path}.seuils} doit etre une liste nommee : chaque seuil doit avoir un nom.")
      }
    }
  }

  if (!is.null(rule$action)) {
    .assert_rule_string(rule$action, paste0(path, ".action"))
    if (is.null(rule$justification)) {
      cli::cli_abort("Le champ {.field {path}.justification} est requis des lors que {.field {path}.action} est declare.")
    }
    .assert_rule_string(rule$justification, paste0(path, ".justification"))
  }

  if (!is.null(rule$source)) {
    .assert_rule_string(rule$source, paste0(path, ".source"))
  }

  rule
}

.assert_rule_string <- function(value, path) {
  if (is.null(value)) {
    cli::cli_abort("Champ requis manquant : {.field {path}}")
  }
  if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
    cli::cli_abort("Le champ {.field {path}} doit etre une chaine de caracteres non vide.")
  }
  invisible(value)
}

# --- Helpers internes -----------------------------------------------------------

.rule_result <- function(rule, message_text, resolved, justification_text = NA_character_) {
  data.frame(
    id = rule$id,
    famille = rule$famille,
    severite = rule$severite,
    message = message_text,
    action = if (is.null(rule$action)) NA_character_ else rule$action,
    justification = justification_text,
    source = if (is.null(rule$source)) NA_character_ else rule$source,
    derogation = identical(resolved, "derogation"),
    stringsAsFactors = FALSE
  )
}

.empty_triggered_rules_df <- function() {
  data.frame(
    id = character(0), famille = character(0), severite = character(0),
    message = character(0), action = character(0), justification = character(0),
    source = character(0), derogation = logical(0),
    stringsAsFactors = FALSE
  )
}

.collect_referenced_symbols <- function(expr) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  if (is.call(expr)) {
    args <- as.list(expr)[-1]
    return(unlist(lapply(args, .collect_referenced_symbols), use.names = FALSE))
  }
  character(0)
}

.format_seuils <- function(seuils) {
  if (is.null(seuils) || length(seuils) == 0) {
    return(NA_character_)
  }
  paste(sprintf("%s=%s", names(seuils), unlist(seuils)), collapse = ", ")
}

.interpolate <- function(template, context) {
  placeholders <- unique(regmatches(template, gregexpr("\\{[a-zA-Z_][a-zA-Z0-9_.]*\\}", template))[[1]])
  if (length(placeholders) == 0) {
    return(template)
  }

  for (placeholder in placeholders) {
    variable_name <- substr(placeholder, 2, nchar(placeholder) - 1)
    value <- context[[variable_name]]
    if (is.null(value)) {
      cli::cli_abort("Variable '{variable_name}' referencee dans le message/justification mais absente du contexte.")
    }
    formatted <- if (is.numeric(value)) format(value, digits = 3) else as.character(value)
    template <- gsub(placeholder, formatted, template, fixed = TRUE)
  }
  template
}

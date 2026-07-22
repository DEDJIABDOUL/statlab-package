# =============================================================================
# Lecture et validation du fichier de configuration YAML d'un projet statlab.
# Schema de reference : inst/schema/config_schema.yml
# =============================================================================

#' Lire un fichier de configuration statlab
#'
#' Lit un fichier YAML de configuration de projet et retourne un objet
#' `statlab_config` non valide. La structure n'est pas verifiee a ce stade :
#' utiliser [st_validate_config()] pour cela.
#'
#' @param path Chemin (chr) vers le fichier `config.yml`.
#'
#' @return Un objet de classe `statlab_config`.
#' @export
st_read_config <- function(path) {
  checkmate::assert_string(path, min.chars = 1)

  if (!file.exists(path)) {
    cli::cli_abort("Fichier de configuration introuvable : {.path {path}}")
  }

  content <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      cli::cli_abort(c(
        "Le fichier de configuration n'est pas un YAML valide : {.path {path}}",
        "x" = conditionMessage(e)
      ))
    }
  )

  if (is.null(content) || !is.list(content)) {
    cli::cli_abort("Le fichier de configuration est vide ou mal forme : {.path {path}}")
  }

  absolute_path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  structure(
    content,
    class = "statlab_config",
    path = absolute_path,
    project_dir = dirname(absolute_path),
    valid = FALSE
  )
}

#' Valider un objet de configuration statlab
#'
#' Verifie la structure d'un objet `statlab_config` produit par
#' [st_read_config()] : sections et champs connus, champs requis presents,
#' enumerations respectees, unicite des identifiants de sources, existence
#' des fichiers declares. Applique aussi les valeurs par defaut documentees
#' dans le schema.
#'
#' En cas d'anomalie, la fonction s'arrete avec un message citant le chemin
#' YAML fautif (ex : `sources[2].fichier`) plutot que de choisir une
#' valeur par defaut a la place de l'utilisateur.
#'
#' @param config Un objet `statlab_config`, tel que retourne par
#'   [st_read_config()].
#'
#' @return L'objet `config`, enrichi des valeurs par defaut, avec l'attribut
#'   `valid` a `TRUE`.
#' @export
st_validate_config <- function(config) {
  checkmate::assert_class(config, "statlab_config")

  project_dir <- attr(config, "project_dir")

  root_fields <- c(
    "projet", "sources", "reconciliation", "dictionnaire",
    "preparation", "analyse", "rendu"
  )
  .check_known_fields(config, root_fields, "")

  config$projet <- .validate_project(config$projet)
  config$sources <- .validate_sources(config$sources, project_dir)
  config$reconciliation <- .validate_reconciliation(config$reconciliation)
  config$dictionnaire <- .validate_dictionary(config$dictionnaire)
  config$preparation <- .validate_preparation(config$preparation)
  config$analyse <- .validate_analysis(config$analyse)
  config$rendu <- .validate_output(config$rendu)

  attr(config, "valid") <- TRUE
  attr(config, "validated_at") <- Sys.time()
  config
}

#' Afficher un objet de configuration statlab
#'
#' @param x Un objet `statlab_config`.
#' @param ... Ignore.
#'
#' @return `x`, de maniere invisible.
#' @export
print.statlab_config <- function(x, ...) {
  checkmate::assert_class(x, "statlab_config")

  valid <- isTRUE(attr(x, "valid"))
  project_name <- if (!is.null(x$projet$nom)) x$projet$nom else "(sans nom)"

  cli::cli_h1("Configuration statlab")
  cli::cli_bullets(c(
    "*" = "Fichier : {.path {attr(x, 'path')}}",
    "*" = "Etat : {if (valid) 'validee' else 'non validee'}",
    "*" = "Projet : {project_name}"
  ))
  if (!is.null(x$sources)) {
    cli::cli_bullets(c("*" = "Sources : {length(x$sources)}"))
  }
  if (!is.null(x$dictionnaire)) {
    cli::cli_bullets(c("*" = "Variables au dictionnaire : {length(x$dictionnaire)}"))
  }
  if (!is.null(x$preparation)) {
    cli::cli_bullets(c("*" = "Regles de preparation : presentes"))
  }
  if (!is.null(x$analyse)) {
    cli::cli_bullets(c("*" = "Plan d'analyse : present"))
  }

  invisible(x)
}

# --- Helpers internes de validation -----------------------------------------

.check_known_fields <- function(obj, valid_fields, path) {
  field_names <- names(obj)
  if (is.null(field_names)) field_names <- character(0)
  unknown <- setdiff(field_names[field_names != ""], valid_fields)
  if (length(unknown) > 0) {
    full_field <- if (nzchar(path)) paste0(path, ".", unknown[1]) else unknown[1]
    cli::cli_abort(c(
      "Champ inconnu dans la configuration : {.field {full_field}}",
      "i" = "Champs valides a ce niveau : {paste(valid_fields, collapse = ', ')}"
    ))
  }
}

.assert_scalar_chr <- function(value, path, required = TRUE) {
  if (is.null(value)) {
    if (required) cli::cli_abort("Champ requis manquant : {.field {path}}")
    return(invisible(NULL))
  }
  if (!is.character(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort("Le champ {.field {path}} doit etre une chaine de caracteres unique.")
  }
  invisible(value)
}

.assert_chr_vector <- function(value, path, required = TRUE) {
  if (is.null(value)) {
    if (required) cli::cli_abort("Champ requis manquant : {.field {path}}")
    return(invisible(NULL))
  }
  if (!is.character(value) || length(value) == 0 || anyNA(value)) {
    cli::cli_abort("Le champ {.field {path}} doit etre un vecteur non vide de chaines de caracteres.")
  }
  invisible(value)
}

.assert_scalar_int <- function(value, path, required = TRUE) {
  if (is.null(value)) {
    if (required) cli::cli_abort("Champ requis manquant : {.field {path}}")
    return(invisible(NULL))
  }
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || value != as.integer(value)) {
    cli::cli_abort("Le champ {.field {path}} doit etre un entier.")
  }
  invisible(as.integer(value))
}

.assert_scalar_bool <- function(value, path, required = TRUE) {
  if (is.null(value)) {
    if (required) cli::cli_abort("Champ requis manquant : {.field {path}}")
    return(invisible(NULL))
  }
  if (!is.logical(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort("Le champ {.field {path}} doit etre un booleen (true/false).")
  }
  invisible(value)
}

.assert_enum <- function(value, valid_values, path, required = TRUE) {
  .assert_scalar_chr(value, path, required = required)
  if (!is.null(value) && !value %in% valid_values) {
    cli::cli_abort(c(
      "Valeur invalide pour {.field {path}} : '{value}'",
      "i" = "Valeurs valides : {paste(valid_values, collapse = ', ')}"
    ))
  }
}

.validate_entry_list <- function(entries, base_path, valid_fields, required_fields) {
  if (!is.list(entries) || length(entries) == 0) {
    cli::cli_abort("Le champ {.field {base_path}} doit etre une liste non vide.")
  }
  for (i in seq_along(entries)) {
    entry_path <- sprintf("%s[%d]", base_path, i)
    entry <- entries[[i]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {entry_path}} doit etre une liste de champs.")
    }
    .check_known_fields(entry, valid_fields, entry_path)
    for (field in required_fields) {
      .assert_scalar_chr(entry[[field]], paste0(entry_path, ".", field))
    }
  }
  entries
}

.validate_project <- function(project) {
  if (is.null(project)) cli::cli_abort("Champ requis manquant : {.field projet}")
  if (!is.list(project)) cli::cli_abort("Le champ {.field projet} doit etre une liste de champs.")
  .check_known_fields(project, c("nom", "langue", "client"), "projet")

  .assert_scalar_chr(project$nom, "projet.nom")
  if (is.null(project$langue)) {
    project$langue <- "fr"
  } else {
    .assert_scalar_chr(project$langue, "projet.langue", required = FALSE)
  }
  if (!is.null(project$client)) {
    .assert_scalar_chr(project$client, "projet.client", required = FALSE)
  }
  project
}

.validate_sources <- function(sources, project_dir) {
  if (is.null(sources)) cli::cli_abort("Champ requis manquant : {.field sources}")
  if (!is.list(sources) || length(sources) == 0) {
    cli::cli_abort("Le champ {.field sources} doit etre une liste non vide de sources.")
  }

  ids <- character(0)
  for (i in seq_along(sources)) {
    entry_path <- sprintf("sources[%d]", i)
    entry <- sources[[i]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {entry_path}} doit etre une liste de champs.")
    }
    .check_known_fields(entry, c("id", "fichier", "onglet", "ligne_entete"), entry_path)

    .assert_scalar_chr(entry$id, paste0(entry_path, ".id"))
    if (entry$id %in% ids) {
      cli::cli_abort("Identifiant de source duplique pour {.field {entry_path}.id} : '{entry$id}'.")
    }
    ids <- c(ids, entry$id)

    .assert_scalar_chr(entry$fichier, paste0(entry_path, ".fichier"))
    file_path <- file.path(project_dir, entry$fichier)
    if (!file.exists(file_path)) {
      cli::cli_abort("Fichier declare introuvable pour {.field {entry_path}.fichier} : {.path {file_path}}")
    }

    if (!is.null(entry$onglet)) {
      .assert_scalar_chr(entry$onglet, paste0(entry_path, ".onglet"), required = FALSE)
    }
    if (!is.null(entry$ligne_entete)) {
      .assert_scalar_int(entry$ligne_entete, paste0(entry_path, ".ligne_entete"), required = FALSE)
    }
  }
  sources
}

.validate_reconciliation <- function(reconciliation) {
  if (is.null(reconciliation)) return(NULL)
  if (!is.list(reconciliation) || length(reconciliation) == 0) {
    cli::cli_abort("Le champ {.field reconciliation} doit etre une liste non vide d'operations.")
  }

  valid_operations <- c("empiler", "joindre", "pivoter_long")
  for (i in seq_along(reconciliation)) {
    entry_path <- sprintf("reconciliation[%d]", i)
    entry <- reconciliation[[i]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {entry_path}} doit etre une liste de champs.")
    }
    .assert_enum(entry$operation, valid_operations, paste0(entry_path, ".operation"))

    reconciliation[[i]] <- switch(entry$operation,
      empiler = .validate_stack_operation(entry, entry_path),
      joindre = .validate_join_operation(entry, entry_path),
      pivoter_long = .validate_pivot_operation(entry, entry_path)
    )
  }
  reconciliation
}

.validate_stack_operation <- function(entry, path) {
  .check_known_fields(entry, c("operation", "sources", "resultat", "sur_colonnes_divergentes"), path)
  .assert_chr_vector(entry$sources, paste0(path, ".sources"))
  if (length(entry$sources) < 2) {
    cli::cli_abort("Le champ {.field {path}.sources} doit contenir au moins deux identifiants de source.")
  }
  .assert_scalar_chr(entry$resultat, paste0(path, ".resultat"))

  if (is.null(entry$sur_colonnes_divergentes)) {
    entry$sur_colonnes_divergentes <- "erreur"
  } else {
    .assert_enum(
      entry$sur_colonnes_divergentes, c("erreur", "intersection", "union"),
      paste0(path, ".sur_colonnes_divergentes"), required = FALSE
    )
  }
  entry
}

.validate_join_operation <- function(entry, path) {
  .check_known_fields(
    entry,
    c("operation", "gauche", "droite", "cle", "normaliser_cle", "type", "resultat", "alerte_explosion"),
    path
  )
  .assert_scalar_chr(entry$gauche, paste0(path, ".gauche"))
  .assert_scalar_chr(entry$droite, paste0(path, ".droite"))
  .assert_chr_vector(entry$cle, paste0(path, ".cle"))
  .assert_scalar_chr(entry$resultat, paste0(path, ".resultat"))

  if (is.null(entry$normaliser_cle)) {
    entry$normaliser_cle <- TRUE
  } else {
    .assert_scalar_bool(entry$normaliser_cle, paste0(path, ".normaliser_cle"), required = FALSE)
  }

  if (is.null(entry$type)) {
    entry$type <- "gauche"
  } else {
    .assert_enum(entry$type, c("gauche", "interieure", "complete"), paste0(path, ".type"), required = FALSE)
  }

  if (is.null(entry$alerte_explosion)) {
    entry$alerte_explosion <- TRUE
  } else {
    .assert_scalar_bool(entry$alerte_explosion, paste0(path, ".alerte_explosion"), required = FALSE)
  }

  entry
}

.validate_pivot_operation <- function(entry, path) {
  .check_known_fields(entry, c("operation", "source", "cles", "mesures", "nom_temps", "nom_valeur", "resultat"), path)
  .assert_scalar_chr(entry$source, paste0(path, ".source"))
  .assert_chr_vector(entry$cles, paste0(path, ".cles"))
  .assert_chr_vector(entry$mesures, paste0(path, ".mesures"))
  .assert_scalar_chr(entry$nom_temps, paste0(path, ".nom_temps"))
  .assert_scalar_chr(entry$nom_valeur, paste0(path, ".nom_valeur"))
  .assert_scalar_chr(entry$resultat, paste0(path, ".resultat"))
  entry
}

.validate_dictionary <- function(dictionary) {
  if (is.null(dictionary)) return(NULL)
  field_names <- names(dictionary)
  if (!is.list(dictionary) || is.null(field_names) || any(field_names == "")) {
    cli::cli_abort("Le champ {.field dictionnaire} doit etre une liste nommee par variable.")
  }

  valid_natures <- c(
    "identifiant", "continue", "entiere", "binaire",
    "nominale", "ordinale", "date", "texte"
  )

  for (variable in field_names) {
    var_path <- sprintf("dictionnaire.%s", variable)
    entry <- dictionary[[variable]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {var_path}} doit etre une liste de champs.")
    }
    .check_known_fields(entry, c("nature", "libelle", "unite", "modalites"), var_path)
    .assert_enum(entry$nature, valid_natures, paste0(var_path, ".nature"))
    if (!is.null(entry$libelle)) {
      .assert_scalar_chr(entry$libelle, paste0(var_path, ".libelle"), required = FALSE)
    }
    if (!is.null(entry$unite)) {
      .assert_scalar_chr(entry$unite, paste0(var_path, ".unite"), required = FALSE)
    }
    if (!is.null(entry$modalites)) {
      .assert_chr_vector(entry$modalites, paste0(var_path, ".modalites"), required = FALSE)
    }
  }
  dictionary
}

.validate_preparation <- function(preparation) {
  if (is.null(preparation)) return(NULL)
  if (!is.list(preparation)) {
    cli::cli_abort("Le champ {.field preparation} doit etre une liste de champs.")
  }
  .check_known_fields(
    preparation,
    c("variables", "dates", "recodages", "derivations", "classes", "exclusions", "manquants"),
    "preparation"
  )

  if (!is.null(preparation$variables)) {
    preparation$variables <- .validate_variables_section(preparation$variables)
  }

  if (!is.null(preparation$dates)) {
    preparation$dates <- .validate_dates_section(preparation$dates)
  }

  if (!is.null(preparation$recodages)) {
    preparation$recodages <- .validate_recodages_section(preparation$recodages)
  }

  if (!is.null(preparation$derivations)) {
    preparation$derivations <- .validate_entry_list(
      preparation$derivations, "preparation.derivations",
      valid_fields = c("nom", "formule", "libelle"),
      required_fields = c("nom", "formule")
    )
  }

  if (!is.null(preparation$classes)) {
    preparation$classes <- .validate_classes_section(preparation$classes)
  }

  if (!is.null(preparation$exclusions)) {
    preparation$exclusions <- .validate_entry_list(
      preparation$exclusions, "preparation.exclusions",
      valid_fields = c("condition", "motif"),
      required_fields = c("condition", "motif")
    )
  }

  if (!is.null(preparation$manquants)) {
    preparation$manquants <- .validate_manquants_section(preparation$manquants)
  }

  preparation
}

.validate_variables_section <- function(variables) {
  if (!is.list(variables)) {
    cli::cli_abort("Le champ {.field preparation.variables} doit etre une liste de champs.")
  }
  .check_known_fields(variables, c("selectionner", "renommer"), "preparation.variables")

  if (!is.null(variables$selectionner)) {
    .assert_chr_vector(variables$selectionner, "preparation.variables.selectionner", required = FALSE)
  }
  if (!is.null(variables$renommer)) {
    renommer <- variables$renommer
    noms <- names(renommer)
    if (!is.list(renommer) || is.null(noms) || any(noms == "")) {
      cli::cli_abort("Le champ {.field preparation.variables.renommer} doit etre une liste nommee (ancien: nouveau).")
    }
    for (ancien in noms) {
      .assert_scalar_chr(renommer[[ancien]], sprintf("preparation.variables.renommer.%s", ancien))
    }
  }
  variables
}

.validate_dates_section <- function(dates) {
  noms <- names(dates)
  if (!is.list(dates) || is.null(noms) || any(noms == "")) {
    cli::cli_abort("Le champ {.field preparation.dates} doit etre une liste nommee par variable.")
  }
  for (variable in noms) {
    var_path <- sprintf("preparation.dates.%s", variable)
    entry <- dates[[variable]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {var_path}} doit etre une liste de champs.")
    }
    .check_known_fields(entry, "format", var_path)
    .assert_scalar_chr(entry$format, paste0(var_path, ".format"))
  }
  dates
}

.validate_recodages_section <- function(recodages) {
  noms <- names(recodages)
  if (!is.list(recodages) || is.null(noms) || any(noms == "")) {
    cli::cli_abort("Le champ {.field preparation.recodages} doit etre une liste nommee par variable.")
  }
  for (variable in noms) {
    var_path <- sprintf("preparation.recodages.%s", variable)
    entry <- recodages[[variable]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {var_path}} doit etre une liste de champs.")
    }
    .check_known_fields(entry, c("fusionner", "ordre"), var_path)

    if (!is.null(entry$fusionner)) {
      fusionner <- entry$fusionner
      noms_fusion <- names(fusionner)
      if (!is.list(fusionner) || is.null(noms_fusion) || any(noms_fusion == "")) {
        cli::cli_abort("Le champ {.field {var_path}.fusionner} doit etre une liste nommee (nouvelle_modalite: [anciennes valeurs]).")
      }
      for (nouvelle_modalite in noms_fusion) {
        .assert_chr_vector(
          fusionner[[nouvelle_modalite]],
          sprintf("%s.fusionner.%s", var_path, nouvelle_modalite)
        )
      }
    }
    if (!is.null(entry$ordre)) {
      .assert_chr_vector(entry$ordre, paste0(var_path, ".ordre"), required = FALSE)
    }
  }
  recodages
}

.validate_classes_section <- function(classes) {
  noms <- names(classes)
  if (!is.list(classes) || is.null(noms) || any(noms == "")) {
    cli::cli_abort("Le champ {.field preparation.classes} doit etre une liste nommee par variable.")
  }
  for (variable in noms) {
    var_path <- sprintf("preparation.classes.%s", variable)
    entry <- classes[[variable]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {var_path}} doit etre une liste de champs.")
    }
    if ("FALSE" %in% names(entry) || "TRUE" %in% names(entry)) {
      cli::cli_abort(c(
        "Champ {.field {var_path}.n} mal interprete par l'analyseur YAML.",
        "i" = "Ecrire la clef entre guillemets (ex : \"n\": 4) : sans guillemets, le mot 'n' est lu comme un booleen (YAML 1.1) plutot que comme un nom de champ."
      ))
    }
    .check_known_fields(entry, c("seuils", "libelles", "methode", "n"), var_path)

    a_seuils <- !is.null(entry$seuils)
    a_methode <- !is.null(entry$methode)
    if (a_seuils == a_methode) {
      cli::cli_abort(c(
        "L'entree {.field {var_path}} doit declarer soit 'seuils' (+ 'libelles'), soit 'methode' (+ 'n'), mais pas les deux.",
        "i" = "Champs actuellement presents : {paste(names(entry), collapse = ', ')}"
      ))
    }

    if (a_seuils) {
      if (!is.numeric(entry$seuils) || length(entry$seuils) == 0 || anyNA(entry$seuils)) {
        cli::cli_abort("Le champ {.field {var_path}.seuils} doit etre un vecteur numerique non vide.")
      }
      .assert_chr_vector(entry$libelles, paste0(var_path, ".libelles"))
      if (length(entry$libelles) != length(entry$seuils) + 1) {
        cli::cli_abort(c(
          "Le champ {.field {var_path}.libelles} doit contenir un libelle de plus que le nombre de seuils.",
          "i" = "{length(entry$seuils)} seuil(s) declare(s), {length(entry$libelles)} libelle(s) fourni(s) (attendu : {length(entry$seuils) + 1})."
        ))
      }
    } else {
      .assert_enum(entry$methode, c("quantiles"), paste0(var_path, ".methode"))
      .assert_scalar_int(entry$n, paste0(var_path, ".n"))
      if (entry$n < 2) {
        cli::cli_abort("Le champ {.field {var_path}.n} doit etre superieur ou egal a 2.")
      }
    }
  }
  classes
}

.validate_manquants_section <- function(manquants) {
  if (!is.list(manquants) || length(manquants) == 0) {
    cli::cli_abort("Le champ {.field preparation.manquants} doit etre une liste non vide.")
  }

  n_catch_all <- 0L
  seen_variables <- character(0)
  for (i in seq_along(manquants)) {
    entry_path <- sprintf("preparation.manquants[%d]", i)
    entry <- manquants[[i]]
    if (!is.list(entry)) {
      cli::cli_abort("L'entree {.field {entry_path}} doit etre une liste de champs.")
    }
    .check_known_fields(entry, c("strategie", "variables"), entry_path)
    .assert_enum(entry$strategie, c("conserver", "exclure_ligne", "imputer"), paste0(entry_path, ".strategie"))

    if (is.null(entry$variables)) {
      n_catch_all <- n_catch_all + 1L
    } else {
      .assert_chr_vector(entry$variables, paste0(entry_path, ".variables"), required = FALSE)
      duplicates <- intersect(entry$variables, seen_variables)
      if (length(duplicates) > 0) {
        cli::cli_abort(c(
          "Variable(s) declaree(s) dans plusieurs entrees de {.field preparation.manquants} : {paste(duplicates, collapse = ', ')}.",
          "i" = "Chaque variable ne peut avoir qu'une seule strategie de manquants."
        ))
      }
      seen_variables <- c(seen_variables, entry$variables)
    }
  }

  if (n_catch_all > 1) {
    cli::cli_abort(c(
      "Le champ {.field preparation.manquants} contient plusieurs entrees sans 'variables' (catch-all).",
      "i" = "Au plus une entree peut s'appliquer a toutes les variables non couvertes par ailleurs."
    ))
  }

  manquants
}

.validate_analysis <- function(analysis) {
  if (is.null(analysis)) return(NULL)
  if (!is.list(analysis)) {
    cli::cli_abort("Le champ {.field analyse} doit etre une liste de champs.")
  }
  .check_known_fields(analysis, c("tableau_1", "comparaisons"), "analyse")

  if (!is.null(analysis$tableau_1)) {
    table1 <- analysis$tableau_1
    if (!is.list(table1)) {
      cli::cli_abort("Le champ {.field analyse.tableau_1} doit etre une liste de champs.")
    }
    .check_known_fields(table1, c("stratification", "variables", "denominateur"), "analyse.tableau_1")
    .assert_chr_vector(table1$variables, "analyse.tableau_1.variables")
    if (!is.null(table1$stratification)) {
      .assert_scalar_chr(table1$stratification, "analyse.tableau_1.stratification", required = FALSE)
    }
    if (is.null(table1$denominateur)) {
      table1$denominateur <- "exclure_manquants"
    } else {
      .assert_enum(
        table1$denominateur, c("inclure_manquants", "exclure_manquants"),
        "analyse.tableau_1.denominateur", required = FALSE
      )
    }
    analysis$tableau_1 <- table1
  }

  if (!is.null(analysis$comparaisons)) {
    entries <- analysis$comparaisons
    if (!is.list(entries) || length(entries) == 0) {
      cli::cli_abort("Le champ {.field analyse.comparaisons} doit etre une liste non vide.")
    }
    for (i in seq_along(entries)) {
      entry_path <- sprintf("analyse.comparaisons[%d]", i)
      entry <- entries[[i]]
      if (!is.list(entry)) {
        cli::cli_abort("L'entree {.field {entry_path}} doit etre une liste de champs.")
      }
      .check_known_fields(entry, c("variables", "groupe", "apparie", "forcer_test"), entry_path)
      .assert_chr_vector(entry$variables, paste0(entry_path, ".variables"))
      .assert_scalar_chr(entry$groupe, paste0(entry_path, ".groupe"))
      if (is.null(entry$apparie)) {
        entry$apparie <- FALSE
      } else {
        .assert_scalar_bool(entry$apparie, paste0(entry_path, ".apparie"), required = FALSE)
      }
      if (!is.null(entry$forcer_test)) {
        .assert_scalar_chr(entry$forcer_test, paste0(entry_path, ".forcer_test"), required = FALSE)
      }
      entries[[i]] <- entry
    }
    analysis$comparaisons <- entries
  }

  analysis
}

.validate_output <- function(output) {
  if (is.null(output)) return(NULL)
  if (!is.list(output)) {
    cli::cli_abort("Le champ {.field rendu} doit etre une liste de champs.")
  }
  .check_known_fields(output, c("charte", "declinaisons", "decimales", "formats"), "rendu")

  if (!is.null(output$charte)) {
    .assert_scalar_chr(output$charte, "rendu.charte", required = FALSE)
  }
  if (!is.null(output$declinaisons)) {
    .assert_chr_vector(output$declinaisons, "rendu.declinaisons", required = FALSE)
    invalid_variants <- setdiff(output$declinaisons, c("ecran", "impression", "projection"))
    if (length(invalid_variants) > 0) {
      cli::cli_abort(c(
        "Valeur(s) invalide(s) pour {.field rendu.declinaisons} : {paste(invalid_variants, collapse = ', ')}",
        "i" = "Valeurs valides : ecran, impression, projection"
      ))
    }
  }
  if (is.null(output$decimales)) {
    output$decimales <- 1L
  } else {
    .assert_scalar_int(output$decimales, "rendu.decimales", required = FALSE)
  }
  if (!is.null(output$formats)) {
    .assert_chr_vector(output$formats, "rendu.formats", required = FALSE)
  }

  output
}

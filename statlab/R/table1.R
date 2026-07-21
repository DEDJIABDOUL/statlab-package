# =============================================================================
# Tableau 1 (statistiques descriptives, avec ou sans stratification).
# La nature statistique de chaque decision (moyenne/mediane, test employe
# pour la colonne de p-value) est prise par le moteur de regles
# (R/rules.R, via st_compare()), jamais codee en dur : ce fichier
# construit le contexte necessaire (libelles, unites, ordres de
# modalites, statistique retenue par variable) et delegue a gtsummary.
# =============================================================================

#' Construire le Tableau 1
#'
#' Construit le tableau descriptif declare dans `config$analyse$tableau_1`,
#' en utilisant les libelles, unites et ordres de modalites du
#' dictionnaire (jamais les noms techniques). Pour chaque variable
#' continue, la statistique (moyenne/ecart-type ou mediane/\[Q1-Q3\]) est
#' choisie selon la normalite (Shapiro-Wilk), evaluee par le meme moteur
#' de regles que [st_compare()]. Si une stratification est declaree, une
#' colonne de p-value est ajoutee ; le test employe pour chaque variable
#' est lui aussi choisi par [st_compare()] (jamais un choix par defaut de
#' gtsummary), et consigne dans une note de bas de tableau.
#'
#' @param data Un data.frame (typiquement le resultat de [st_prepare()]).
#' @param config Un objet `statlab_config` valide, tel que retourne par
#'   [st_validate_config()], dont `analyse$tableau_1` declare les
#'   variables (et, en option, la stratification et le denominateur des
#'   pourcentages).
#'
#' @return Un objet `gtsummary` (`tbl_summary`), utilisable pour
#'   composition ulterieure (ex : [gtsummary::tbl_merge()]) ou converti
#'   via [st_table1_flextable()] / [st_table1_csv()].
#' @export
st_table1 <- function(data, config) {
  checkmate::assert_data_frame(data)
  checkmate::assert_class(config, "statlab_config")
  if (!isTRUE(attr(config, "valid"))) {
    cli::cli_abort("La configuration doit etre validee (st_validate_config()) avant de construire le tableau 1.")
  }

  spec <- config$analyse$tableau_1
  if (is.null(spec)) {
    cli::cli_abort("La section 'analyse.tableau_1' n'est pas declaree dans config.yml.")
  }

  variables <- spec$variables
  missing_vars <- setdiff(variables, names(data))
  if (length(missing_vars) > 0) {
    cli::cli_abort("Variable(s) de 'analyse.tableau_1.variables' introuvable(s) : {paste(missing_vars, collapse = ', ')}.")
  }

  stratification <- spec$stratification
  if (!is.null(stratification) && !stratification %in% names(data)) {
    cli::cli_abort("Variable de stratification introuvable : '{stratification}'.")
  }

  dictionary <- config$dictionnaire
  decimals <- if (!is.null(config$rendu$decimales)) config$rendu$decimales else 1L
  denominator <- if (!is.null(spec$denominateur)) spec$denominateur else "exclure_manquants"

  columns_needed <- unique(c(variables, if (!is.null(stratification)) stratification))
  working_data <- .table1_apply_dictionary_levels(data[, columns_needed, drop = FALSE], columns_needed, dictionary)

  statistics <- .table1_statistics_spec(working_data, variables, dictionary)
  labels <- .table1_labels_spec(variables, dictionary)

  # "missing" chez gtsummary ne controle que l'AFFICHAGE d'une ligne de
  # comptage des manquants : cela n'inclut jamais les manquants dans le
  # denominateur des pourcentages des autres modalites. Pour honorer
  # "inclure_manquants", les NA des variables categorielles sont donc
  # transformes en modalite explicite avant de construire le tableau ; la
  # ligne "Manquant" separee de gtsummary devient alors superflue.
  if (identical(denominator, "inclure_manquants")) {
    working_data <- .table1_explicit_missing_level(working_data, variables, dictionary)
    missing_mode <- "no"
  } else {
    missing_mode <- "ifany"
  }

  table_summary <- gtsummary::tbl_summary(
    working_data,
    by = stratification,
    include = dplyr::all_of(variables),
    statistic = statistics,
    label = labels,
    digits = gtsummary::all_continuous() ~ decimals,
    missing = missing_mode,
    missing_text = "Manquant"
  )
  table_summary <- gtsummary::modify_footnote(
    table_summary,
    gtsummary::all_stat_cols() ~ "Moyenne (ecart-type) ; Mediane [Q1-Q3] ; Effectif (pourcentage)"
  )

  if (!is.null(stratification)) {
    test_specs <- .table1_test_functions(variables, dictionary, config)
    if (!is.null(test_specs)) {
      table_summary <- gtsummary::add_p(table_summary, test = test_specs)
    }
    table_summary <- gtsummary::add_overall(table_summary, col_label = "**Ensemble**  \nN = {N}")
  }

  table_summary
}

#' Convertir le Tableau 1 en flextable
#'
#' Conversion NATIVE (jamais une image) pour insertion editable dans un
#' document Word.
#'
#' @param table1 L'objet retourne par [st_table1()].
#'
#' @return Un objet `flextable`.
#' @export
st_table1_flextable <- function(table1) {
  checkmate::assert_class(table1, "gtsummary")
  gtsummary::as_flex_table(table1)
}

#' Exporter le Tableau 1 en tableau brut
#'
#' @param table1 L'objet retourne par [st_table1()].
#'
#' @return Un data.frame (une ligne par caracteristique/modalite affichee).
#' @export
st_table1_csv <- function(table1) {
  checkmate::assert_class(table1, "gtsummary")
  as.data.frame(table1$table_body, stringsAsFactors = FALSE)
}

# --- Libelles, unites, modalites (dictionnaire) -----------------------------

.table1_labels_spec <- function(variables, dictionary) {
  specs <- list()
  for (variable in variables) {
    entry <- dictionary[[variable]]
    if (is.null(entry) || is.null(entry$libelle)) {
      next
    }
    label_text <- entry$libelle
    if (!is.null(entry$unite)) {
      label_text <- sprintf("%s (%s)", label_text, entry$unite)
    }
    specs[[variable]] <- stats::as.formula(sprintf("`%s` ~ %s", variable, deparse(label_text)))
  }
  if (length(specs) == 0) {
    return(NULL)
  }
  unname(specs)
}

.table1_explicit_missing_level <- function(data, variables, dictionary) {
  for (variable in variables) {
    entry <- dictionary[[variable]]
    if (is.null(entry) || !entry$nature %in% c("binaire", "nominale", "ordinale")) {
      next
    }
    if (anyNA(data[[variable]])) {
      data[[variable]] <- forcats::fct_na_value_to_level(factor(data[[variable]]), level = "Manquant")
    }
  }
  data
}

.table1_apply_dictionary_levels <- function(data, variables, dictionary) {
  for (variable in variables) {
    entry <- dictionary[[variable]]
    if (is.null(entry) || identical(entry$nature, "date")) {
      next
    }
    if (entry$nature %in% c("continue", "entiere")) {
      # gtsummary determine categorielle/continue d'apres le TYPE R de la
      # colonne, pas d'apres 'nature' : une variable continue encore en
      # caractere (cas courant, st_read_source() ne type jamais les
      # colonnes) serait sinon traitee comme categorielle et rejetterait
      # moyenne/ecart-type ou mediane/IQR.
      if (!is.numeric(data[[variable]])) {
        data[[variable]] <- .to_numeric_permissive(as.character(data[[variable]]))
      }
    } else if (!is.null(entry$modalites)) {
      data[[variable]] <- factor(as.character(data[[variable]]), levels = entry$modalites)
    } else if (entry$nature %in% c("binaire", "nominale", "ordinale") && !is.factor(data[[variable]])) {
      data[[variable]] <- factor(as.character(data[[variable]]))
    }
  }
  data
}

# --- Choix de la statistique (moyenne/ecart-type vs mediane/IQR) -----------

.table1_statistics_spec <- function(data, variables, dictionary) {
  specs <- list()
  for (variable in variables) {
    nature <- if (!is.null(dictionary[[variable]])) dictionary[[variable]]$nature else NA_character_
    if (!identical(nature, "continue") && !identical(nature, "entiere")) {
      next
    }
    values <- .clean_numeric(data[[variable]])
    shapiro_p <- .safe_shapiro(values)
    is_normal <- !is.na(shapiro_p) && shapiro_p >= 0.05

    formula_text <- if (is_normal) "{mean} ({sd})" else "{median} [{p25}-{p75}]"
    specs[[variable]] <- stats::as.formula(sprintf("`%s` ~ %s", variable, deparse(formula_text)))
  }
  if (length(specs) == 0) {
    return(NULL)
  }
  unname(specs)
}

# --- Choix du test (colonne p-value), pilote par st_compare() --------------

.make_table1_test_function <- function(target_variable, config) {
  force(target_variable)
  force(config)
  function(data, variable, by, ...) {
    resultat <- st_compare(data, target_variable, by, config)
    data.frame(p.value = resultat$p_value, method = resultat$test_name, stringsAsFactors = FALSE)
  }
}

.table1_test_functions <- function(variables, dictionary, config) {
  specs <- list()
  for (variable in variables) {
    nature <- if (!is.null(dictionary[[variable]])) dictionary[[variable]]$nature else NA_character_
    if (identical(nature, "identifiant")) {
      next
    }

    test_fn <- .make_table1_test_function(variable, config)
    formula_obj <- stats::as.formula(paste0("`", variable, "` ~ test_fn"))
    # Chaque formule recoit son propre environnement isole ne contenant que
    # sa fermeture : sans cela, une seule variable partagee entre toutes les
    # iterations ferait pointer chaque formule vers la DERNIERE fermeture
    # creee, plutot que celle qui lui correspond.
    environment(formula_obj) <- list2env(list(test_fn = test_fn), parent = emptyenv())
    specs[[variable]] <- formula_obj
  }
  if (length(specs) == 0) {
    return(NULL)
  }
  unname(specs)
}

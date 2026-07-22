# =============================================================================
# Generation du script R "annexe" du rapport : une reproduction litterale,
# section par section, de la chaine d'analyse (lecture, reconciliation,
# preparation, tests, graphiques), executable par un tiers SANS le package
# statlab (uniquement des packages publics du CRAN). C'est la caution
# scientifique du livrable : le jury doit pouvoir rejouer chaque resultat
# numerique a partir de ce seul fichier et des sources brutes.
#
# Principe de construction : cette fonction fait tourner elle-meme la vraie
# chaine statlab (ingest -> reconciliation -> preparation -> comparaisons),
# pour obtenir les faits REELS et RESOLUS de cette execution precise
# (onglet retenu, ligne d'en-tete, test statistique choisi, etc.), puis
# transpose chaque etape en code R litteral n'appelant que des fonctions de
# packages publics. Ce n'est pas une devinette a partir de la configuration
# seule : chaque valeur inseree dans le script a ete effectivement calculee.
#
# Simplification assumee (documentee en en-tete du script genere) : les
# graphiques utilisent un theme ggplot2 neutre plutot que la charte
# graphique proprietaire de statlab (R/theme.R), afin de ne dependre que du
# CRAN. Les VALEURS representees restent, elles, strictement identiques.
# =============================================================================

#' Generer le script R autonome de reproduction de l'analyse
#'
#' Execute la chaine d'analyse declaree dans `config` (lecture,
#' reconciliation, preparation, comparaisons) pour en capturer les faits
#' resolus (parametres de lecture detectes, tests statistiques retenus par
#' le moteur de regles), puis genere un script R autonome qui reproduit
#' litteralement chaque etape avec des packages publics du CRAN
#' uniquement : aucun appel a `statlab` n'apparait dans le fichier produit.
#'
#' Chaque test statistique est accompagne, en commentaire, de la
#' justification methodologique produite par [st_evaluate_rules()] (via
#' [st_compare()]). Les graphiques utilisent un theme neutre (`ggplot2`) :
#' la charte graphique du rapport (`R/theme.R`) n'est pas reproduite, afin
#' de ne pas introduire de dependance au package `statlab`.
#'
#' @param config Un objet `statlab_config` valide, tel que retourne par
#'   [st_validate_config()].
#' @param output Chemin (chr) du script a produire. Si relatif, resolu par
#'   rapport au repertoire du projet. Par defaut,
#'   `"sorties/rapport/analyse.R"`.
#'
#' @return Le chemin absolu (chr) du script genere, de maniere invisible.
#' @export
st_export_script <- function(config, output = "sorties/rapport/analyse.R") {
  checkmate::assert_class(config, "statlab_config")
  if (!isTRUE(attr(config, "valid"))) {
    cli::cli_abort("La configuration doit etre validee (st_validate_config()) avant l'export du script.")
  }
  checkmate::assert_string(output, min.chars = 1)

  project_dir <- attr(config, "project_dir")
  st_log_init(project_dir)

  sources_data <- st_read_all_sources(config)
  sources_meta <- .es_describe_sources(config, sources_data)

  if (!is.null(config$reconciliation)) {
    recon <- st_reconcile(sources_data, config)
    working_data <- recon$table
    working_var_name <- .es_safe_name(config$reconciliation[[length(config$reconciliation)]]$resultat)
  } else {
    if (length(sources_data) != 1) {
      cli::cli_abort(c(
        "Plusieurs sources sont declarees sans operation de reconciliation.",
        "i" = "Declarer une section 'reconciliation' dans config.yml pour indiquer comment les assembler."
      ))
    }
    working_data <- sources_data[[1]]
    working_var_name <- .es_safe_name(config$sources[[1]]$id)
  }

  prepared_data <- st_prepare(working_data, config)
  comparisons_results <- .es_run_all_comparisons(prepared_data, config)
  rules <- st_load_rules()
  needed_packages <- .es_needed_packages(config, sources_meta, comparisons_results)

  script_lines <- c(
    .es_header(config, sources_meta, attr(rules, "version"), needed_packages),
    "",
    .es_packages(needed_packages),
    "",
    .es_helpers(),
    "",
    .es_ingestion(sources_meta),
    "",
    if (!is.null(config$reconciliation)) c(.es_reconciliation(config), "") else NULL,
    .es_preparation(config, working_var_name),
    "",
    if (!is.null(config$analyse$tableau_1)) c(.es_table1(config, prepared_data), "") else NULL,
    if (!is.null(config$analyse$comparaisons)) c(.es_comparisons(comparisons_results), "") else NULL,
    .es_plots(comparisons_results, config$dictionnaire),
    "",
    .es_footer()
  )

  output_path <- if (.is_absolute_path(output)) output else file.path(project_dir, output)
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  writeLines(script_lines, output_path, useBytes = TRUE)
  absolute_output <- normalizePath(output_path, winslash = "/", mustWork = TRUE)

  st_log(
    "export_script",
    module = "export_script", fichier = absolute_output,
    n_lignes = length(script_lines), n_comparaisons = length(comparisons_results),
    level = "info"
  )
  cli::cli_alert_success("Script R annexe genere : {.path {absolute_output}}")
  invisible(absolute_output)
}

# --- Collecte des faits resolus (execution reelle de la chaine) -------------

.es_describe_sources <- function(config, sources_data) {
  lapply(config$sources, function(entry) {
    data <- sources_data[[entry$id]]
    path <- attr(data, "file_path")
    extension <- tolower(tools::file_ext(path))
    is_excel <- extension %in% c("xlsx", "xls")

    delim <- NA_character_
    encoding <- NA_character_
    decimal_mark <- NA_character_
    if (!is_excel) {
      delim <- detect_delimiter(path)
      encoding <- detect_encoding(path)
      decimal_mark <- detect_decimal_mark(path, delim)
    }

    list(
      id = entry$id, fichier_relatif = entry$fichier, chemin_absolu = path,
      hash = attr(data, "file_hash"), is_excel = is_excel,
      onglet = attr(data, "sheet"), ligne_entete = attr(data, "header_row"),
      n_lignes_brutes = attr(data, "n_raw_rows"), n_lignes = nrow(data), n_colonnes = ncol(data),
      delimiteur = delim, encodage = encoding, separateur_decimal = decimal_mark
    )
  })
}

.es_run_all_comparisons <- function(prepared_data, config) {
  entries <- config$analyse$comparaisons
  if (is.null(entries)) {
    return(list())
  }

  results <- list()
  for (entry in entries) {
    paired <- isTRUE(entry$apparie)
    for (variable in entry$variables) {
      result <- st_compare(prepared_data, variable, entry$groupe, config, paired = paired)
      id_variable <- if (paired) .lookup_identifier_variable(config, prepared_data) else NA_character_
      results[[length(results) + 1]] <- list(
        variable = variable, group = entry$groupe, paired = paired,
        nature_variable = .lookup_nature(variable, config),
        nature_group = .lookup_nature(entry$groupe, config),
        id_variable = id_variable, family = .es_family_for_test(result$test_name),
        result = result
      )
    }
  }
  results
}

.es_family_for_test <- function(test_name) {
  switch(test_name,
    "Test de Mann-Whitney" = ,
    "Test t de Student" = ,
    "Test t de Welch" = "continue_2groupes",
    "Test de Kruskal-Wallis" = ,
    "ANOVA de Welch" = ,
    "ANOVA a un facteur" = "continue_multigroupes",
    "Test du Chi2" = ,
    "Test exact de Fisher" = "contingence",
    "Test t de Student apparie" = ,
    "Test de Wilcoxon apparie" = "appariee",
    "Test de McNemar" = "mcnemar",
    "Correlation de Pearson" = ,
    "Correlation de Spearman" = "correlation",
    cli::cli_abort("Test non pris en charge par l'export de script : '{test_name}'.")
  )
}

.es_needed_packages <- function(config, sources_meta, comparisons_results) {
  packages <- character(0)
  if (any(vapply(sources_meta, function(s) s$is_excel, logical(1)))) {
    packages <- c(packages, "readxl")
  }
  if (any(vapply(sources_meta, function(s) !s$is_excel, logical(1)))) {
    packages <- c(packages, "readr")
  }

  recon <- config$reconciliation
  if (!is.null(recon)) {
    ops <- vapply(recon, function(o) o$operation, character(1))
    if ("empiler" %in% ops) packages <- c(packages, "dplyr")
    if ("joindre" %in% ops) packages <- c(packages, "powerjoin")
    if ("pivoter_long" %in% ops) packages <- c(packages, "tidyr", "dplyr")
  }

  recodages <- config$preparation$recodages
  if (!is.null(recodages) && any(vapply(recodages, function(e) !is.null(e$fusionner), logical(1)))) {
    packages <- c(packages, "forcats")
  }

  if (length(comparisons_results) > 0) {
    packages <- c(packages, "ggplot2")
  }

  unique(c("stats", packages))
}

.es_package_versions <- function(packages) {
  vapply(packages, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "inconnue")
  }, character(1))
}

# --- Assemblage du texte du script -------------------------------------------

.es_safe_name <- function(id) {
  name <- gsub("[^A-Za-z0-9_]+", "_", id)
  if (!grepl("^[A-Za-z.]", name)) {
    name <- paste0("x_", name)
  }
  name
}

.es_header <- function(config, sources_meta, rules_version, needed_packages) {
  project_name <- if (!is.null(config$projet$nom)) config$projet$nom else "(sans nom)"
  config_hash <- digest::digest(file = attr(config, "path"), algo = "sha256")
  package_versions <- .es_package_versions(needed_packages)

  source_lines <- unlist(lapply(sources_meta, function(s) {
    c(
      sprintf("#   - %s : %s", s$id, s$fichier_relatif),
      sprintf("#       SHA-256 : %s", s$hash)
    )
  }))

  c(
    "# =============================================================================",
    sprintf("# Script R autonome de reproduction de l'analyse : %s", project_name),
    "# Genere automatiquement par statlab::st_export_script() -- NE PAS EDITER A LA",
    "# MAIN (toute correction doit etre apportee a config.yml, puis le script",
    "# regenere).",
    "#",
    sprintf("# Date de generation : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("# R                  : %s", R.version.string),
    sprintf("# Referentiel methodologique (methodology.yml) : version %s", rules_version),
    sprintf("# Fichier config.yml (empreinte SHA-256) : %s", config_hash),
    "#",
    "# Ce script ne depend que de packages publics du CRAN (section 1 ci-dessous) :",
    "# il s'execute sans le package 'statlab'. Celui-ci reste necessaire pour",
    "# REGENERER ce script (config.yml, moteur de regles, gabarit de rapport),",
    "# mais pas pour REJOUER l'analyse a partir des donnees brutes.",
    "#",
    "# Fichiers sources (chemins relatifs au repertoire de ce script) :",
    source_lines,
    "#",
    "# Versions des packages utilises a la generation :",
    sprintf("#   - %s : %s", names(package_versions), package_versions),
    "#",
    "# NOTE SUR LES GRAPHIQUES : la charte graphique (couleurs, polices, mise en",
    "# forme) du rapport officiel est propre au package statlab (R/theme.R) et",
    "# n'est pas reproduite ici, afin de ne dependre que du CRAN. Les graphiques",
    "# ci-dessous utilisent un theme ggplot2 neutre ; les valeurs representees",
    "# (statistiques, tests) sont en revanche strictement identiques.",
    "# ============================================================================="
  )
}

.es_packages <- function(needed_packages) {
  c(
    "# --- 1. Chargement des packages ---------------------------------------------",
    "# Packages publics du CRAN uniquement (aucune dependance a 'statlab').",
    sprintf("library(%s)", needed_packages)
  )
}

.es_helpers <- function() {
  c(
    "# --- Fonctions utilitaires (reproduisent des helpers internes de statlab) ----",
    "",
    "# Conversion numerique permissive : certains fichiers notent les decimales",
    "# avec une virgule (usage francais). On tente les deux lectures et on retient",
    "# celle qui recupere le plus de valeurs non manquantes.",
    "to_numeric_fr <- function(x) {",
    "  direct <- suppressWarnings(as.numeric(x))",
    '  alternatif <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))',
    "  if (sum(!is.na(alternatif)) > sum(!is.na(direct))) alternatif else direct",
    "}",
    "",
    "# Normalisation des codes de valeur manquante usuels (memes codes que",
    "# statlab::normalize_missing() : comparaison insensible a la casse, apres",
    "# suppression des espaces de bord).",
    "normalize_missing_fr <- function(x) {",
    '  codes <- c("", " ", "NA", "N/A", "NR", "ND", "NC", "-", "--", ".", "?", "999", "9999", "-99")',
    "  x_chr <- as.character(x)",
    "  x_trim <- trimws(x_chr)",
    "  est_manquant <- is.na(x_chr) | (toupper(x_trim) %in% toupper(codes))",
    "  x_chr[est_manquant] <- NA_character_",
    "  x_chr",
    "}",
    "",
    "# Imputation simple : mediane pour le numerique/les dates, modalite la plus",
    "# frequente sinon.",
    "imputer_variable_fr <- function(x) {",
    "  non_manquant <- x[!is.na(x)]",
    '  if (length(non_manquant) == 0) stop("Imputation impossible : aucune valeur non manquante.")',
    '  if (is.numeric(x) || inherits(x, "Date")) {',
    "    x[is.na(x)] <- stats::median(non_manquant)",
    "  } else if (is.factor(x)) {",
    "    frequences <- sort(table(non_manquant), decreasing = TRUE)",
    "    x[is.na(x)] <- names(frequences)[1]",
    "  } else {",
    "    valeurs_num <- suppressWarnings(as.numeric(non_manquant))",
    "    if (!anyNA(valeurs_num)) {",
    "      x[is.na(x)] <- as.character(stats::median(valeurs_num))",
    "    } else {",
    "      frequences <- sort(table(non_manquant), decreasing = TRUE)",
    "      x[is.na(x)] <- names(frequences)[1]",
    "    }",
    "  }",
    "  x",
    "}"
  )
}

# --- Section 2 : lecture ------------------------------------------------------

.es_ingestion <- function(sources_meta) {
  lines <- c(
    "# --- 2. Lecture des donnees brutes ------------------------------------------",
    "# Les chemins sont relatifs au repertoire de ce script (meme repertoire que",
    "# config.yml). Chaque source est lue integralement en texte (aucune",
    "# inference de type a ce stade) : la conversion numerique/date n'intervient",
    "# qu'a l'etape de preparation, pour ne jamais perdre silencieusement une",
    "# valeur mal formee."
  )
  for (s in sources_meta) {
    lines <- c(lines, "", .es_ingestion_one(s))
  }
  lines
}

.es_ingestion_one <- function(s) {
  var_name <- .es_safe_name(s$id)
  if (s$is_excel) {
    c(
      sprintf("# Source '%s' : %s", s$id, s$fichier_relatif),
      sprintf("#   Onglet retenu     : '%s'", s$onglet),
      sprintf("#   Ligne d'en-tete   : %d (sur %d lignes brutes)", s$ligne_entete, s$n_lignes_brutes),
      sprintf("#   Empreinte SHA-256 (verifiee a la generation) : %s", s$hash),
      sprintf(
        '%s <- readxl::read_excel("%s", sheet = "%s", skip = %d, col_names = TRUE, col_types = "text")',
        var_name, s$fichier_relatif, s$onglet, s$ligne_entete - 1
      ),
      sprintf("%s <- as.data.frame(%s, stringsAsFactors = FALSE)", var_name, var_name)
    )
  } else {
    c(
      sprintf("# Source '%s' : %s", s$id, s$fichier_relatif),
      sprintf("#   Delimiteur detecte         : '%s'", gsub("\t", "\\\\t", s$delimiteur)),
      sprintf("#   Encodage detecte           : %s", s$encodage),
      sprintf("#   Separateur decimal detecte : '%s' (indicatif ; la conversion numerique", s$separateur_decimal),
      "#   ci-dessous teste systematiquement le point et la virgule, cf. to_numeric_fr())",
      sprintf("#   Ligne d'en-tete            : %d (sur %d lignes brutes)", s$ligne_entete, s$n_lignes_brutes),
      sprintf("#   Empreinte SHA-256 (verifiee a la generation) : %s", s$hash),
      sprintf(
        '%s <- readr::read_delim("%s", delim = "%s", skip = %d, col_types = readr::cols(.default = readr::col_character()), locale = readr::locale(encoding = "%s"), trim_ws = FALSE)',
        var_name, s$fichier_relatif, .es_escape_delim(s$delimiteur), s$ligne_entete - 1, .encoding_to_iconv(s$encodage)
      ),
      sprintf("%s <- as.data.frame(%s, stringsAsFactors = FALSE)", var_name, var_name)
    )
  }
}

.es_escape_delim <- function(delim) {
  if (identical(delim, "\t")) "\\t" else delim
}

# --- Section 3 : reconciliation -----------------------------------------------

.es_reconciliation <- function(config) {
  lines <- c(
    "# --- 3. Assemblage des sources (reconciliation) ------------------------------",
    "# Chaque operation reproduit les memes fonctions R publiques que",
    "# statlab::st_reconcile() (dplyr::bind_rows, powerjoin::power_*_join,",
    "# tidyr::pivot_longer) : memes entrees, memes resultats numeriques."
  )
  for (op in config$reconciliation) {
    lines <- c(lines, "", switch(op$operation,
      empiler = .es_reconciliation_stack(op),
      joindre = .es_reconciliation_join(op),
      pivoter_long = .es_reconciliation_pivot(op)
    ))
  }
  lines
}

.es_reconciliation_stack <- function(op) {
  var_result <- .es_safe_name(op$resultat)
  sources_vars <- vapply(op$sources, .es_safe_name, character(1))
  list_literal <- paste(sources_vars, collapse = ", ")
  mode <- op$sur_colonnes_divergentes
  header <- sprintf("# Operation 'empiler' -> %s (sources : %s)", op$resultat, paste(op$sources, collapse = ", "))

  if (identical(mode, "intersection")) {
    c(
      header,
      "# Mode 'intersection' : en cas de colonnes divergentes entre sources, seules",
      "# les colonnes communes a toutes les sources sont conservees.",
      sprintf(".tables_%s <- list(%s)", var_result, list_literal),
      sprintf(".colonnes_communes_%s <- Reduce(intersect, lapply(.tables_%s, names))", var_result, var_result),
      sprintf(".tables_%s <- lapply(.tables_%s, function(df) df[, .colonnes_communes_%s, drop = FALSE])", var_result, var_result, var_result),
      sprintf('%s <- dplyr::bind_rows(.tables_%s, .id = ".source_empilee")', var_result, var_result)
    )
  } else {
    note <- if (identical(mode, "union")) {
      "# Mode 'union' : dplyr::bind_rows() complete nativement les colonnes absentes par NA."
    } else {
      "# Mode 'erreur' : les sources sont supposees partager exactement les memes colonnes."
    }
    c(header, note, sprintf('%s <- dplyr::bind_rows(list(%s), .id = ".source_empilee")', var_result, list_literal))
  }
}

.es_reconciliation_join <- function(op) {
  var_result <- .es_safe_name(op$resultat)
  left_var <- .es_safe_name(op$gauche)
  right_var <- .es_safe_name(op$droite)
  keys <- op$cle
  normalize <- isTRUE(op$normaliser_cle)
  join_fn <- switch(op$type, gauche = "power_left_join", interieure = "power_inner_join", complete = "power_full_join")
  check_args <- 'check = powerjoin::check_specs(implicit_keys = "ignore", column_conflict = "ignore", duplicate_keys_left = "ignore", duplicate_keys_right = "ignore", unmatched_keys_left = "ignore", unmatched_keys_right = "ignore")'
  header <- sprintf("# Operation 'joindre' -> %s (%s x %s, type = %s)", op$resultat, op$gauche, op$droite, op$type)

  if (normalize) {
    keys_literal <- paste(sprintf('"%s"', keys), collapse = ", ")
    c(
      header,
      "# Cle(s) normalisee(s) avant appariement : espaces supprimes, majuscules,",
      "# caracteres non alphanumeriques retires, zeros initiaux harmonises. La cle",
      "# d'origine n'est jamais modifiee dans le resultat.",
      "normaliser_cle_fr <- function(x) {",
      "  x <- toupper(as.character(x))",
      '  x <- gsub("[^A-Z0-9]+", "", x)',
      '  est_numerique <- !is.na(x) & grepl("^[0-9]+$", x)',
      '  x[est_numerique] <- sub("^0+(?=[0-9])", "", x[est_numerique], perl = TRUE)',
      "  x",
      "}",
      sprintf(".cles_%s <- c(%s)", var_result, keys_literal),
      sprintf('for (.k in .cles_%s) %s[[paste0(".norm_", .k)]] <- normaliser_cle_fr(%s[[.k]])', var_result, left_var, left_var),
      sprintf('for (.k in .cles_%s) %s[[paste0(".norm_", .k)]] <- normaliser_cle_fr(%s[[.k]])', var_result, right_var, right_var),
      sprintf('.by_%s <- paste0(".norm_", .cles_%s)', var_result, var_result),
      sprintf(
        '%s <- powerjoin::%s(%s, %s, by = .by_%s, suffix = c("_gauche", "_droite"), %s)',
        var_result, join_fn, left_var, right_var, var_result, check_args
      ),
      sprintf("%s[, .by_%s] <- NULL", var_result, var_result)
    )
  } else {
    keys_literal <- paste(sprintf('"%s"', keys), collapse = ", ")
    c(
      header,
      sprintf(
        '%s <- powerjoin::%s(%s, %s, by = c(%s), suffix = c("_gauche", "_droite"), %s)',
        var_result, join_fn, left_var, right_var, keys_literal, check_args
      )
    )
  }
}

.es_reconciliation_pivot <- function(op) {
  var_result <- .es_safe_name(op$resultat)
  var_source <- .es_safe_name(op$source)
  mesures_literal <- paste(sprintf('"%s"', op$mesures), collapse = ", ")
  c(
    sprintf("# Operation 'pivoter_long' -> %s (source : %s)", op$resultat, op$source),
    sprintf(
      '%s <- tidyr::pivot_longer(%s, cols = dplyr::all_of(c(%s)), names_to = "%s", values_to = "%s")',
      var_result, var_source, mesures_literal, op$nom_temps, op$nom_valeur
    )
  )
}

# --- Section 4 : preparation ---------------------------------------------------

.es_preparation <- function(config, working_var_name) {
  prep <- config$preparation
  lines <- c(
    "# --- 4. Preparation des donnees ----------------------------------------------",
    "# Ordre fixe (identique a statlab::st_prepare()) : variables, dates,",
    "# recodages, derivations, classes, exclusions, valeurs manquantes.",
    sprintf("donnees <- %s", working_var_name)
  )
  if (is.null(prep)) {
    return(c(
      lines, "",
      "# Aucune section 'preparation' declaree dans config.yml : les donnees issues",
      "# de la lecture/reconciliation sont utilisees telles quelles."
    ))
  }

  if (!is.null(prep$variables)) lines <- c(lines, "", .es_prep_variables(prep$variables))
  if (!is.null(prep$dates)) lines <- c(lines, "", .es_prep_dates(prep$dates))
  if (!is.null(prep$recodages)) lines <- c(lines, "", .es_prep_recodages(prep$recodages))
  if (!is.null(prep$derivations)) lines <- c(lines, "", .es_prep_derivations(prep$derivations))
  if (!is.null(prep$classes)) lines <- c(lines, "", .es_prep_classes(prep$classes))
  if (!is.null(prep$exclusions)) lines <- c(lines, "", .es_prep_exclusions(prep$exclusions))
  lines <- c(lines, "", .es_prep_manquants(prep$manquants))
  lines
}

.es_prep_variables <- function(spec) {
  lines <- "# 4.1 Selection / renommage de variables"
  if (!is.null(spec$selectionner)) {
    cols <- paste(sprintf('"%s"', spec$selectionner), collapse = ", ")
    lines <- c(lines, sprintf("donnees <- donnees[, c(%s), drop = FALSE]", cols))
  }
  if (!is.null(spec$renommer)) {
    for (ancien in names(spec$renommer)) {
      lines <- c(lines, sprintf('names(donnees)[names(donnees) == "%s"] <- "%s"', ancien, spec$renommer[[ancien]]))
    }
  }
  lines
}

.es_prep_dates <- function(spec) {
  lines <- c(
    "# 4.2 Conversion des dates",
    "# Format impose explicitement par config.yml (jamais devine)."
  )
  for (variable in names(spec)) {
    fmt <- spec[[variable]]$format
    lines <- c(lines, sprintf('donnees[["%s"]] <- as.Date(as.character(donnees[["%s"]]), format = "%s")', variable, variable, fmt))
  }
  lines
}

.es_prep_recodages <- function(spec) {
  lines <- "# 4.3 Recodages (fusion de modalites, ordre du facteur)"
  for (variable in names(spec)) {
    entry <- spec[[variable]]
    lines <- c(lines, sprintf('.x <- as.character(donnees[["%s"]])', variable))
    if (!is.null(entry$fusionner)) {
      fusion_args <- paste(vapply(names(entry$fusionner), function(nouvelle) {
        anciennes <- paste(sprintf('"%s"', entry$fusionner[[nouvelle]]), collapse = ", ")
        sprintf('"%s" = c(%s)', nouvelle, anciennes)
      }, character(1)), collapse = ", ")
      lines <- c(lines, sprintf(".x <- as.character(forcats::fct_collapse(factor(.x), %s))", fusion_args))
    }
    if (!is.null(entry$ordre)) {
      ordre_literal <- paste(sprintf('"%s"', entry$ordre), collapse = ", ")
      lines <- c(lines, sprintf(".x <- factor(.x, levels = c(%s))", ordre_literal))
    }
    lines <- c(lines, sprintf('donnees[["%s"]] <- .x', variable), "")
  }
  lines
}

.es_prep_derivations <- function(derivations) {
  lines <- "# 4.4 Variables derivees (expression du dictionnaire, reprise telle quelle)"
  for (entry in derivations) {
    if (!is.null(entry$libelle)) {
      lines <- c(lines, sprintf("# %s : %s", entry$nom, entry$libelle))
    }
    lines <- c(lines, sprintf('donnees[["%s"]] <- with(donnees, %s)', entry$nom, entry$formule), "")
  }
  lines
}

.es_prep_classes <- function(spec) {
  lines <- "# 4.5 Decoupage en classes"
  for (variable in names(spec)) {
    entry <- spec[[variable]]
    lines <- c(lines, sprintf('.v <- to_numeric_fr(as.character(donnees[["%s"]]))', variable))
    if (!is.null(entry$seuils)) {
      seuils_literal <- paste(entry$seuils, collapse = ", ")
      libelles_literal <- paste(sprintf('"%s"', entry$libelles), collapse = ", ")
      lines <- c(
        lines,
        sprintf(".breaks <- c(-Inf, %s, Inf)", seuils_literal),
        sprintf('donnees[["%s"]] <- cut(.v, breaks = .breaks, labels = c(%s), right = FALSE)', variable, libelles_literal)
      )
    } else {
      lines <- c(
        lines,
        sprintf(".probs <- seq(0, 1, length.out = %d + 1)", entry$n),
        ".breaks <- unique(stats::quantile(.v, probs = .probs, na.rm = TRUE, names = FALSE))",
        sprintf('.labels <- paste0("Q", seq_len(%d))', entry$n),
        sprintf(
          'if (length(.labels) != length(.breaks) - 1) stop("Nombre de classes de quantiles insuffisant pour %s")',
          variable
        ),
        sprintf('donnees[["%s"]] <- cut(.v, breaks = .breaks, labels = .labels, include.lowest = TRUE)', variable)
      )
    }
    lines <- c(lines, "")
  }
  lines
}

.es_prep_exclusions <- function(exclusions) {
  lines <- "# 4.6 Exclusions de lignes"
  for (entry in exclusions) {
    lines <- c(
      lines,
      sprintf("# Motif : %s", entry$motif),
      sprintf(".exclure <- with(donnees, %s)", entry$condition),
      ".exclure[is.na(.exclure)] <- FALSE",
      ".n_avant <- nrow(donnees)",
      "donnees <- donnees[!.exclure, , drop = FALSE]",
      sprintf(
        'message(sprintf("Exclusion (%%s) : %%d -> %%d observation(s)", "%s", .n_avant, nrow(donnees)))',
        gsub('"', "'", entry$motif)
      ),
      ""
    )
  }
  lines
}

.es_prep_manquants <- function(manquants_specs) {
  lines <- c(
    "# 4.7 Traitement des valeurs manquantes",
    "# Codes reconnus comme manquants (identique a statlab::normalize_missing()) :",
    "# '', ' ', NA, N/A, NR, ND, NC, -, --, ., ?, 999, 9999, -99 (insensible a la",
    "# casse, apres suppression des espaces de bord).",
    "for (.v in names(donnees)) {",
    "  if (is.character(donnees[[.v]])) donnees[[.v]] <- normalize_missing_fr(donnees[[.v]])",
    "}"
  )
  if (length(manquants_specs) == 0) {
    return(lines)
  }

  explicit_vars <- unlist(lapply(manquants_specs, function(s) s$variables))
  explicit_literal <- if (length(explicit_vars) == 0) "character(0)" else paste(sprintf('"%s"', explicit_vars), collapse = ", ")
  lines <- c(lines, "", sprintf(".variables_avec_strategie_explicite <- c(%s)", explicit_literal))

  for (spec in manquants_specs) {
    target_expr <- if (is.null(spec$variables)) {
      "setdiff(names(donnees), .variables_avec_strategie_explicite)"
    } else {
      sprintf("c(%s)", paste(sprintf('"%s"', spec$variables), collapse = ", "))
    }
    label <- if (is.null(spec$variables)) {
      sprintf("Strategie '%s' (s'applique au reste des variables)", spec$strategie)
    } else {
      sprintf("Strategie '%s' pour : %s", spec$strategie, paste(spec$variables, collapse = ", "))
    }
    lines <- c(lines, "", sprintf("# %s", label), sprintf(".cibles <- intersect(%s, names(donnees))", target_expr))

    if (identical(spec$strategie, "exclure_ligne")) {
      lines <- c(
        lines,
        ".masque_manquant <- Reduce(`|`, lapply(.cibles, function(v) is.na(donnees[[v]])))",
        ".n_avant <- nrow(donnees)",
        "donnees <- donnees[!.masque_manquant, , drop = FALSE]",
        'message(sprintf("Exclusion (valeurs manquantes : %s) : %d -> %d observation(s)", paste(.cibles, collapse = ", "), .n_avant, nrow(donnees)))'
      )
    } else if (identical(spec$strategie, "imputer")) {
      lines <- c(lines, "for (.v in .cibles) donnees[[.v]] <- imputer_variable_fr(donnees[[.v]])")
    } else {
      lines <- c(lines, "# strategie 'conserver' : aucune action (valeurs manquantes conservees telles quelles)")
    }
  }
  lines
}

# --- Section 5 : tableau 1 ------------------------------------------------------

.es_table1 <- function(config, prepared_data) {
  spec <- config$analyse$tableau_1
  dictionary <- config$dictionnaire
  lines <- c(
    "# --- 5. Tableau 1 (statistiques descriptives) ---------------------------------",
    "# Pour chaque variable continue, la statistique (moyenne/ecart-type ou",
    "# mediane/[Q1-Q3]) depend de la normalite (Shapiro-Wilk, seuil 5%), comme",
    "# dans statlab::st_table1(). Les p-values de stratification, si demandees,",
    "# sont exactement celles calculees a la section 'Comparaisons' ci-dessous",
    "# (meme test statistique) : elles ne sont pas recalculees ici."
  )

  for (variable in spec$variables) {
    entry <- dictionary[[variable]]
    nature <- if (!is.null(entry)) entry$nature else NA_character_
    label <- if (!is.null(entry) && !is.null(entry$libelle)) entry$libelle else variable

    if (identical(nature, "continue") || identical(nature, "entiere")) {
      values <- .to_numeric_permissive(as.character(prepared_data[[variable]]))
      values <- values[!is.na(values)]
      shapiro_p <- .safe_shapiro(values)
      is_normal <- !is.na(shapiro_p) && shapiro_p >= 0.05
      stat_comment <- if (is_normal) {
        "moyenne (ecart-type) -- Shapiro-Wilk non significatif (normalite plausible)"
      } else {
        "mediane [Q1-Q3] -- Shapiro-Wilk significatif (distribution non normale)"
      }
      lines <- c(
        lines, "",
        sprintf("# %s : %s", label, stat_comment),
        sprintf('.v <- to_numeric_fr(as.character(donnees[["%s"]])); .v <- .v[!is.na(.v)]', variable)
      )
      lines <- c(lines, if (is_normal) {
        sprintf('message(sprintf("%s : %%.2f (%%.2f), n = %%d", mean(.v), stats::sd(.v), length(.v)))', label)
      } else {
        sprintf(
          'message(sprintf("%s : %%.2f [%%.2f-%%.2f], n = %%d", stats::median(.v), stats::quantile(.v, 0.25, names = FALSE), stats::quantile(.v, 0.75, names = FALSE), length(.v)))',
          label
        )
      })
    } else {
      lines <- c(
        lines, "",
        sprintf("# %s : effectif (pourcentage) par modalite", label),
        sprintf('print(table(as.character(donnees[["%s"]]), useNA = "ifany"))', variable)
      )
    }
  }
  lines
}

# --- Section 6 : comparaisons ---------------------------------------------------

.es_comparisons <- function(comparisons_results) {
  lines <- c(
    "# --- 6. Comparaisons statistiques ---------------------------------------------",
    "# Le test employe pour chaque comparaison est celui retenu par le moteur de",
    "# regles methodologiques de statlab (Shapiro-Wilk pour la normalite, Levene",
    "# pour l'homogeneite des variances, effectifs theoriques pour le choix",
    "# Chi2/Fisher). Le choix est fige au resultat obtenu lors de la generation :",
    "# la justification figure en commentaire au-dessus de chaque test."
  )
  for (item in comparisons_results) {
    lines <- c(lines, "", .es_one_comparison(item))
  }
  lines
}

.es_one_comparison <- function(item) {
  r <- item$result
  header <- c(
    sprintf("## Comparaison : %s selon %s", item$variable, item$group),
    sprintf("# Test retenu : %s", r$test_name),
    strwrap(r$justification, width = 78, prefix = "#   ", initial = "# Justification : "),
    sprintf("# p-value obtenue a la generation (verification) : %s", format(r$p_value, digits = 6, scientific = FALSE))
  )

  body <- switch(item$family,
    continue_2groupes = .es_test_continue_2groupes(item$variable, item$group, r$test_name),
    continue_multigroupes = .es_test_continue_multigroupes(item$variable, item$group, r$test_name),
    contingence = .es_test_contingence(item$variable, item$group, r$test_name),
    appariee = .es_test_appariee(item$variable, item$group, item$id_variable, r$test_name),
    mcnemar = .es_test_mcnemar(item$variable, item$group, item$id_variable),
    correlation = .es_test_correlation(item$variable, item$group, r$test_name)
  )
  c(header, body)
}

.es_test_continue_2groupes <- function(variable, group, test_name) {
  lines <- c(
    sprintf('.complet <- donnees[!is.na(donnees[["%s"]]) & !is.na(donnees[["%s"]]), , drop = FALSE]', variable, group),
    sprintf('.grp <- as.character(.complet[["%s"]])', group),
    ".niveaux <- unique(.grp)",
    sprintf('.x <- to_numeric_fr(as.character(.complet[["%s"]][.grp == .niveaux[1]]))', variable),
    sprintf('.y <- to_numeric_fr(as.character(.complet[["%s"]][.grp == .niveaux[2]]))', variable),
    ".x <- .x[!is.na(.x)]; .y <- .y[!is.na(.y)]"
  )
  call <- switch(test_name,
    "Test de Mann-Whitney" = ".resultat_test <- stats::wilcox.test(.x, .y, conf.int = TRUE)",
    "Test t de Student" = ".resultat_test <- stats::t.test(.x, .y, var.equal = TRUE)",
    "Test t de Welch" = ".resultat_test <- stats::t.test(.x, .y, var.equal = FALSE)"
  )
  c(lines, call, "print(.resultat_test)")
}

.es_test_continue_multigroupes <- function(variable, group, test_name) {
  lines <- c(
    sprintf('.complet <- donnees[!is.na(donnees[["%s"]]) & !is.na(donnees[["%s"]]), , drop = FALSE]', variable, group),
    sprintf(
      '.df <- data.frame(value = to_numeric_fr(as.character(.complet[["%s"]])), grp = factor(as.character(.complet[["%s"]])))',
      variable, group
    ),
    ".df <- .df[!is.na(.df$value), , drop = FALSE]"
  )
  call <- switch(test_name,
    "Test de Kruskal-Wallis" = ".resultat_test <- stats::kruskal.test(value ~ grp, data = .df)",
    "ANOVA de Welch" = ".resultat_test <- stats::oneway.test(value ~ grp, data = .df, var.equal = FALSE)",
    "ANOVA a un facteur" = ".resultat_test <- summary(stats::aov(value ~ grp, data = .df))"
  )
  c(lines, call, "print(.resultat_test)")
}

.es_test_contingence <- function(variable, group, test_name) {
  lines <- c(
    sprintf('.complet <- donnees[!is.na(donnees[["%s"]]) & !is.na(donnees[["%s"]]), , drop = FALSE]', variable, group),
    sprintf(
      '.tableau <- table(factor(as.character(.complet[["%s"]])), factor(as.character(.complet[["%s"]])))',
      variable, group
    )
  )
  call <- switch(test_name,
    "Test du Chi2" = ".resultat_test <- stats::chisq.test(.tableau)",
    "Test exact de Fisher" = ".resultat_test <- stats::fisher.test(.tableau)"
  )
  c(lines, call, "print(.resultat_test)")
}

.es_test_correlation <- function(variable, group, test_name) {
  methode <- if (identical(test_name, "Correlation de Spearman")) "spearman" else "pearson"
  c(
    sprintf('.complet <- donnees[!is.na(donnees[["%s"]]) & !is.na(donnees[["%s"]]), , drop = FALSE]', variable, group),
    sprintf('.x <- to_numeric_fr(as.character(.complet[["%s"]]))', variable),
    sprintf('.y <- to_numeric_fr(as.character(.complet[["%s"]]))', group),
    ".garder <- !is.na(.x) & !is.na(.y); .x <- .x[.garder]; .y <- .y[.garder]",
    sprintf('.resultat_test <- stats::cor.test(.x, .y, method = "%s")', methode),
    "print(.resultat_test)"
  )
}

.es_test_appariee <- function(variable, group, id_variable, test_name) {
  lines <- c(
    sprintf('.niv <- unique(stats::na.omit(as.character(donnees[["%s"]])))', group),
    sprintf('.d1 <- donnees[!is.na(donnees[["%s"]]) & as.character(donnees[["%s"]]) == .niv[1], , drop = FALSE]', group, group),
    sprintf('.d2 <- donnees[!is.na(donnees[["%s"]]) & as.character(donnees[["%s"]]) == .niv[2], , drop = FALSE]', group, group),
    sprintf('.id1 <- as.character(.d1[["%s"]]); .id2 <- as.character(.d2[["%s"]])', id_variable, id_variable),
    ".communs <- intersect(.id1, .id2)",
    sprintf('.x <- to_numeric_fr(as.character(.d1[["%s"]][match(.communs, .id1)]))', variable),
    sprintf('.y <- to_numeric_fr(as.character(.d2[["%s"]][match(.communs, .id2)]))', variable),
    ".garder <- !is.na(.x) & !is.na(.y); .x <- .x[.garder]; .y <- .y[.garder]"
  )
  call <- switch(test_name,
    "Test de Wilcoxon apparie" = ".resultat_test <- stats::wilcox.test(.x, .y, paired = TRUE, conf.int = TRUE)",
    "Test t de Student apparie" = ".resultat_test <- stats::t.test(.x, .y, paired = TRUE)"
  )
  c(lines, call, "print(.resultat_test)")
}

.es_test_mcnemar <- function(variable, group, id_variable) {
  c(
    sprintf('.niv <- unique(stats::na.omit(as.character(donnees[["%s"]])))', group),
    sprintf('.d1 <- donnees[!is.na(donnees[["%s"]]) & as.character(donnees[["%s"]]) == .niv[1], , drop = FALSE]', group, group),
    sprintf('.d2 <- donnees[!is.na(donnees[["%s"]]) & as.character(donnees[["%s"]]) == .niv[2], , drop = FALSE]', group, group),
    sprintf('.id1 <- as.character(.d1[["%s"]]); .id2 <- as.character(.d2[["%s"]])', id_variable, id_variable),
    ".communs <- intersect(.id1, .id2)",
    sprintf('.v1 <- as.character(.d1[["%s"]][match(.communs, .id1)])', variable),
    sprintf('.v2 <- as.character(.d2[["%s"]][match(.communs, .id2)])', variable),
    ".garder <- !is.na(.v1) & !is.na(.v2); .v1 <- .v1[.garder]; .v2 <- .v2[.garder]",
    ".niveaux_var <- sort(unique(c(.v1, .v2)))",
    ".tableau <- table(factor(.v1, levels = .niveaux_var), factor(.v2, levels = .niveaux_var))",
    ".resultat_test <- stats::mcnemar.test(.tableau, correct = TRUE)",
    "print(.resultat_test)"
  )
}

# --- Section 7 : graphiques ------------------------------------------------------

.es_plots <- function(comparisons_results, dictionary) {
  if (length(comparisons_results) == 0) {
    return(character(0))
  }
  lines <- c(
    "# --- 7. Graphiques --------------------------------------------------------",
    "# Theme ggplot2 neutre (theme_minimal) : la charte graphique du rapport",
    "# officiel (statlab::st_theme()) n'est pas reproduite ici, pour rester",
    "# independant du package statlab. Les valeurs tracees sont identiques."
  )
  seen <- character(0)
  for (item in comparisons_results) {
    key <- paste(item$variable, item$group, sep = "|")
    if (key %in% seen) next
    seen <- c(seen, key)
    lines <- c(lines, "", .es_one_plot(item, dictionary))
  }
  lines
}

.es_one_plot <- function(item, dictionary) {
  variable <- item$variable
  group <- item$group
  is_continue <- item$nature_variable %in% c("continue", "entiere")
  label_var <- if (!is.null(dictionary[[variable]]$libelle)) dictionary[[variable]]$libelle else variable
  label_grp <- if (!is.null(dictionary[[group]]$libelle)) dictionary[[group]]$libelle else group

  if (is_continue) {
    c(
      sprintf("# Graphique : %s selon %s (boite a moustaches)", label_var, label_grp),
      sprintf(
        '.graphique <- ggplot2::ggplot(donnees, ggplot2::aes(x = factor(%s), y = to_numeric_fr(as.character(%s)))) +',
        group, variable
      ),
      "  ggplot2::geom_boxplot(na.rm = TRUE) +",
      sprintf('  ggplot2::labs(title = "%s selon %s", x = "%s", y = "%s") +', label_var, label_grp, label_grp, label_var),
      "  ggplot2::theme_minimal()",
      "print(.graphique)"
    )
  } else {
    c(
      sprintf("# Graphique : %s selon %s (barres)", label_var, label_grp),
      sprintf('.graphique <- ggplot2::ggplot(donnees, ggplot2::aes(x = factor(%s), fill = factor(%s))) +', variable, group),
      "  ggplot2::geom_bar(position = ggplot2::position_dodge(), na.rm = TRUE) +",
      sprintf('  ggplot2::labs(title = "%s selon %s", x = "%s", fill = "%s") +', label_var, label_grp, label_var, label_grp),
      "  ggplot2::theme_minimal()",
      "print(.graphique)"
    )
  }
}

# --- Section 8 : pied de script ---------------------------------------------------

.es_footer <- function() {
  c(
    "# --- 8. Informations de session ------------------------------------------------",
    "print(sessionInfo())"
  )
}

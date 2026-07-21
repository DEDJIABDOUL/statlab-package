# =============================================================================
# Execution des tests statistiques, pilotee par le moteur de regles
# (R/rules.R). Le choix du test n'est jamais code en dur pour les
# preconditions couvertes par le referentiel (normalite, homogeneite des
# variances, effectifs theoriques) : st_compare() construit un contexte,
# le soumet a st_evaluate_rules(), et n'execute le test par defaut que si
# aucune regle bloquante ne l'en detourne.
# =============================================================================

#' Comparer une variable selon un groupe
#'
#' Construit le contexte methodologique (natures, effectifs, normalite,
#' homogeneite des variances, effectifs theoriques, appariement), le
#' soumet au moteur de regles ([st_evaluate_rules()]), puis execute le
#' test statistique retenu par l'arbre de decision (ou impose par
#' `config$analyse$comparaisons[].forcer_test`).
#'
#' Arbre de decision : `group` continu et `variable` continue ->
#' correlation (Pearson, sauf regle de normalite -> Spearman). Sinon, si
#' `paired = TRUE` -> test t/Wilcoxon apparie (variable continue) ou test
#' de McNemar (variable binaire), l'appariement des observations reposant
#' sur la variable de nature `identifiant` du dictionnaire. Sinon (non
#' apparie) : variable continue et 2 groupes -> Student/Welch/Mann-Whitney ;
#' variable continue et 3+ groupes -> ANOVA/ANOVA de Welch/Kruskal-Wallis ;
#' variable categorielle -> Chi2/Fisher exact.
#'
#' @param data Un data.frame (typiquement le resultat de [st_prepare()]).
#' @param variable Nom (chr) de la variable comparee.
#' @param group Nom (chr) de la variable de groupement (ou de la seconde
#'   variable continue, pour une correlation).
#' @param config Un objet `statlab_config` valide, tel que retourne par
#'   [st_validate_config()].
#' @param paired Si `TRUE`, traite la comparaison comme appariee (requiert
#'   une variable de nature `identifiant` dans le dictionnaire).
#'
#' @return Une liste (classe `statlab_comparison`) avec les champs
#'   `variable`, `group`, `test_name`, `statistic`, `parameter`,
#'   `p_value`, `effect_size`, `effect_size_name`, `conf_low`,
#'   `conf_high`, `n_by_group`, `descriptive_by_group`, `justification`,
#'   `rules_triggered`, `derogations`.
#' @export
st_compare <- function(data, variable, group, config, paired = FALSE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(variable, min.chars = 1)
  checkmate::assert_string(group, min.chars = 1)
  checkmate::assert_class(config, "statlab_config")
  checkmate::assert_flag(paired)
  if (!isTRUE(attr(config, "valid"))) {
    cli::cli_abort("La configuration doit etre validee (st_validate_config()) avant la comparaison.")
  }
  if (!variable %in% names(data)) {
    cli::cli_abort("Variable introuvable : '{variable}'.")
  }
  if (!group %in% names(data)) {
    cli::cli_abort("Variable de groupe introuvable : '{group}'.")
  }

  variable_nature <- .lookup_nature(variable, config)
  group_nature <- .lookup_nature(group, config)
  forced_test <- .lookup_forced_test(config, variable, group)

  is_variable_continuous <- variable_nature %in% c("continue", "entiere")
  is_group_continuous <- group_nature %in% c("continue", "entiere")

  if (is_group_continuous && is_variable_continuous) {
    if (paired) {
      cli::cli_abort("L'appariement (paired = TRUE) ne s'applique pas a une correlation entre deux variables continues.")
    }
    return(.compare_correlation(data, variable, group, forced_test))
  }

  if (paired) {
    id_variable <- .lookup_identifier_variable(config, data)
    if (is.null(id_variable)) {
      cli::cli_abort("L'appariement necessite une variable de nature 'identifiant' declaree dans le dictionnaire.")
    }
    if (is_variable_continuous) {
      return(.compare_paired_continuous(data, variable, group, forced_test, id_variable))
    }
    if (identical(variable_nature, "binaire")) {
      return(.compare_mcnemar(data, variable, group, forced_test, id_variable))
    }
    cli::cli_abort("Aucun test appariee standard pour une variable de nature '{variable_nature}' ('{variable}').")
  }

  if (is_variable_continuous) {
    n_groups <- length(unique(stats::na.omit(as.character(data[[group]]))))
    if (n_groups < 2) {
      cli::cli_abort("Le groupe '{group}' ne comporte qu'un seul niveau : comparaison impossible.")
    }
    if (n_groups == 2) {
      return(.compare_two_groups_continuous(data, variable, group, forced_test))
    }
    return(.compare_multi_groups_continuous(data, variable, group, forced_test))
  }

  if (variable_nature %in% c("binaire", "nominale", "ordinale")) {
    return(.compare_contingency(data, variable, group, forced_test))
  }

  cli::cli_abort("Nature non prise en charge pour la comparaison : '{variable_nature}' ('{variable}').")
}

# --- Familles de comparaison -----------------------------------------------

.compare_two_groups_continuous <- function(data, variable, group, forced_test) {
  complete <- data[!is.na(data[[variable]]) & !is.na(data[[group]]), , drop = FALSE]
  group_values <- as.character(complete[[group]])
  levels_present <- unique(group_values)
  if (length(levels_present) != 2) {
    cli::cli_abort("Une comparaison a deux groupes necessite exactement deux niveaux pour '{group}' ({length(levels_present)} observe(s)).")
  }

  x <- .clean_numeric(complete[[variable]][group_values == levels_present[1]])
  y <- .clean_numeric(complete[[variable]][group_values == levels_present[2]])
  .check_group_feasibility(list(x, y), variable)

  shapiro_p <- min(.safe_shapiro(x), .safe_shapiro(y), na.rm = TRUE)
  levene_p <- .safe_levene(c(x, y), rep(levels_present, c(length(x), length(y))))

  context <- list(
    test_type = "comparaison_2_groupes", is_continuous = TRUE,
    min_group_n = min(length(x), length(y)), shapiro_p = shapiro_p, levene_p = levene_p
  )
  override <- if (!is.null(forced_test)) c("NORM-002", "VARIANCE-001") else character(0)
  triggered <- st_evaluate_rules(context, family = "preconditions", override = override)

  test_choice <- if (!is.null(forced_test)) {
    forced_test
  } else if ("NORM-002" %in% triggered$id) {
    "mann_whitney"
  } else if ("VARIANCE-001" %in% triggered$id) {
    "welch"
  } else {
    "student"
  }

  if (test_choice == "mann_whitney") {
    fit <- stats::wilcox.test(x, y, conf.int = TRUE)
    statistic <- unname(fit$statistic)
    parameter <- NA_real_
    conf_low <- .safe_conf_int(fit, 1)
    conf_high <- .safe_conf_int(fit, 2)
    effect_size <- .wilcoxon_effect_size(fit$p.value, length(x) + length(y))
    effect_size_name <- "r (correlation de rang biserial)"
    test_name <- "Test de Mann-Whitney"
  } else {
    var_equal <- identical(test_choice, "student")
    fit <- stats::t.test(x, y, var.equal = var_equal)
    statistic <- unname(fit$statistic)
    parameter <- unname(fit$parameter)
    conf_low <- fit$conf.int[1]
    conf_high <- fit$conf.int[2]
    effect_size <- .cohens_d_effect(x, y, var_equal)
    effect_size_name <- "d de Cohen"
    test_name <- if (var_equal) "Test t de Student" else "Test t de Welch"
  }

  descriptive <- rbind(.descriptive_stats(x, levels_present[1]), .descriptive_stats(y, levels_present[2]))
  n_by_group <- stats::setNames(c(length(x), length(y)), levels_present)
  justification <- .build_justification(test_name, triggered, context, forced_test)

  .build_comparison_result(
    variable, group, test_name, statistic, parameter, fit$p.value,
    effect_size, effect_size_name, conf_low, conf_high,
    n_by_group, descriptive, justification, triggered
  )
}

.compare_multi_groups_continuous <- function(data, variable, group, forced_test) {
  complete <- data[!is.na(data[[variable]]) & !is.na(data[[group]]), , drop = FALSE]
  grouping <- droplevels(factor(as.character(complete[[group]])))
  levels_present <- levels(grouping)
  if (length(levels_present) < 3) {
    cli::cli_abort("Une comparaison multi-groupes necessite au moins trois niveaux pour '{group}' ({length(levels_present)} observe(s)).")
  }

  values <- .clean_numeric(complete[[variable]])
  keep <- !is.na(values)
  values <- values[keep]
  grouping <- droplevels(grouping[keep])
  .check_group_feasibility(split(values, grouping), variable)

  shapiro_p <- min(vapply(split(values, grouping), .safe_shapiro, numeric(1)), na.rm = TRUE)
  levene_p <- .safe_levene(values, grouping)

  context <- list(
    test_type = "comparaison_multi_groupes", shapiro_p = shapiro_p,
    levene_p = levene_p, min_group_n = min(table(grouping))
  )
  override <- if (!is.null(forced_test)) c("NORM-004", "VARIANCE-002") else character(0)
  triggered <- st_evaluate_rules(context, family = "preconditions", override = override)

  test_choice <- if (!is.null(forced_test)) {
    forced_test
  } else if ("NORM-004" %in% triggered$id) {
    "kruskal_wallis"
  } else if ("VARIANCE-002" %in% triggered$id) {
    "welch_anova"
  } else {
    "anova"
  }

  df <- data.frame(value = values, grp = grouping)
  conf_low <- NA_real_
  conf_high <- NA_real_

  if (test_choice == "kruskal_wallis") {
    fit <- stats::kruskal.test(value ~ grp, data = df)
    statistic <- unname(fit$statistic)
    parameter <- unname(fit$parameter)
    eff <- rstatix::kruskal_effsize(df, value ~ grp)
    effect_size <- eff$effsize[1]
    effect_size_name <- "Eta carre (H de Kruskal-Wallis)"
    test_name <- "Test de Kruskal-Wallis"
    p_value <- fit$p.value
  } else if (test_choice == "welch_anova") {
    fit <- stats::oneway.test(value ~ grp, data = df, var.equal = FALSE)
    statistic <- unname(fit$statistic)
    parameter <- sprintf("%.2f, %.2f", fit$parameter[1], fit$parameter[2])
    effect_size <- .eta_squared(df)
    effect_size_name <- "Eta carre"
    test_name <- "ANOVA de Welch"
    p_value <- fit$p.value
  } else {
    fit_summary <- summary(stats::aov(value ~ grp, data = df))[[1]]
    statistic <- fit_summary[["F value"]][1]
    parameter <- sprintf("%d, %d", fit_summary[["Df"]][1], fit_summary[["Df"]][2])
    effect_size <- fit_summary[["Sum Sq"]][1] / sum(fit_summary[["Sum Sq"]])
    effect_size_name <- "Eta carre"
    test_name <- "ANOVA a un facteur"
    p_value <- fit_summary[["Pr(>F)"]][1]
  }

  descriptive <- do.call(rbind, lapply(levels_present, function(lvl) .descriptive_stats(values[grouping == lvl], lvl)))
  n_by_group <- stats::setNames(as.integer(table(grouping)), levels_present)
  justification <- .build_justification(test_name, triggered, context, forced_test)

  .build_comparison_result(
    variable, group, test_name, statistic, parameter, p_value,
    effect_size, effect_size_name, conf_low, conf_high,
    n_by_group, descriptive, justification, triggered
  )
}

.compare_contingency <- function(data, variable, group, forced_test) {
  complete <- data[!is.na(data[[variable]]) & !is.na(data[[group]]), , drop = FALSE]
  var_factor <- droplevels(factor(as.character(complete[[variable]])))
  grp_factor <- droplevels(factor(as.character(complete[[group]])))
  if (nlevels(var_factor) < 2 || nlevels(grp_factor) < 2) {
    cli::cli_abort("Le tableau croise necessite au moins deux modalites pour '{variable}' et '{group}'.")
  }

  tab <- table(var_factor, grp_factor)
  if (sum(tab) == 0) {
    cli::cli_abort("Tableau croise vide pour '{variable}' x '{group}'.")
  }

  fit_chi2 <- suppressWarnings(stats::chisq.test(tab))
  effectif_theorique_min <- min(fit_chi2$expected)
  col_totals <- colSums(tab)

  context <- list(
    test_type = "tableau_croise", effectif_theorique_min = effectif_theorique_min,
    min_group_n = min(col_totals[col_totals > 0])
  )
  override <- if (!is.null(forced_test)) "CHI2-001" else character(0)
  triggered <- st_evaluate_rules(context, family = "preconditions", override = override)

  test_choice <- if (!is.null(forced_test)) {
    forced_test
  } else if ("CHI2-001" %in% triggered$id) {
    "fisher"
  } else {
    "chi2"
  }

  conf_low <- NA_real_
  conf_high <- NA_real_
  if (test_choice == "fisher") {
    fit <- stats::fisher.test(tab)
    statistic <- NA_real_
    parameter <- NA_real_
    p_value <- fit$p.value
    if (!is.null(fit$conf.int)) {
      conf_low <- fit$conf.int[1]
      conf_high <- fit$conf.int[2]
    }
    test_name <- "Test exact de Fisher"
  } else {
    fit <- fit_chi2
    statistic <- unname(fit$statistic)
    parameter <- unname(fit$parameter)
    p_value <- fit$p.value
    test_name <- "Test du Chi2"
  }

  effect_size <- suppressWarnings(as.numeric(rstatix::cramer_v(tab)))
  effect_size_name <- "V de Cramer"

  descriptive <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(descriptive) <- c(variable, group, "n")
  n_by_group <- col_totals

  justification <- .build_justification(test_name, triggered, context, forced_test)

  .build_comparison_result(
    variable, group, test_name, statistic, parameter, p_value,
    effect_size, effect_size_name, conf_low, conf_high,
    n_by_group, descriptive, justification, triggered
  )
}

.compare_paired_continuous <- function(data, variable, group, forced_test, id_variable) {
  aligned <- .align_pairs(data, group, id_variable)
  x <- .clean_numeric(aligned$data1[[variable]])
  y <- .clean_numeric(aligned$data2[[variable]])
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  .check_group_feasibility(list(x, y), variable)

  differences <- x - y
  shapiro_p <- .safe_shapiro(differences)

  context <- list(
    test_type = "comparaison_appariee", shapiro_p = shapiro_p,
    apparie = TRUE, n_paires_completes = aligned$n_paires_completes,
    n_total_declare = aligned$n_total_declare, min_group_n = length(x)
  )
  override <- if (!is.null(forced_test)) "NORM-003" else character(0)
  triggered <- st_evaluate_rules(context, family = "preconditions", override = override)

  test_choice <- if (!is.null(forced_test)) {
    forced_test
  } else if ("NORM-003" %in% triggered$id) {
    "wilcoxon_apparie"
  } else {
    "student_apparie"
  }

  if (test_choice == "wilcoxon_apparie") {
    fit <- stats::wilcox.test(x, y, paired = TRUE, conf.int = TRUE)
    statistic <- unname(fit$statistic)
    parameter <- NA_real_
    conf_low <- .safe_conf_int(fit, 1)
    conf_high <- .safe_conf_int(fit, 2)
    effect_size <- .wilcoxon_effect_size(fit$p.value, length(x) * 2)
    effect_size_name <- "r (correlation de rang biserial)"
    test_name <- "Test de Wilcoxon apparie"
  } else {
    fit <- stats::t.test(x, y, paired = TRUE)
    statistic <- unname(fit$statistic)
    parameter <- unname(fit$parameter)
    conf_low <- fit$conf.int[1]
    conf_high <- fit$conf.int[2]
    effect_size <- mean(differences) / stats::sd(differences)
    effect_size_name <- "d de Cohen (apparie)"
    test_name <- "Test t de Student apparie"
  }

  descriptive <- rbind(
    .descriptive_stats(x, paste0(aligned$level1)),
    .descriptive_stats(y, paste0(aligned$level2))
  )
  n_by_group <- stats::setNames(c(length(x), length(y)), c(aligned$level1, aligned$level2))
  justification <- .build_justification(test_name, triggered, context, forced_test)

  .build_comparison_result(
    variable, group, test_name, statistic, parameter, fit$p.value,
    effect_size, effect_size_name, conf_low, conf_high,
    n_by_group, descriptive, justification, triggered
  )
}

.compare_mcnemar <- function(data, variable, group, forced_test, id_variable) {
  aligned <- .align_pairs(data, group, id_variable)
  v1 <- as.character(aligned$data1[[variable]])
  v2 <- as.character(aligned$data2[[variable]])
  keep <- !is.na(v1) & !is.na(v2)
  v1 <- v1[keep]
  v2 <- v2[keep]

  levels_var <- sort(unique(c(v1, v2)))
  if (length(levels_var) != 2) {
    cli::cli_abort("Le test de McNemar necessite une variable binaire ('{variable}' a {length(levels_var)} modalite(s)).")
  }
  if (length(v1) == 0) {
    cli::cli_abort("Effectif nul pour '{variable}' : test impossible.")
  }

  tab <- table(factor(v1, levels = levels_var), factor(v2, levels = levels_var))

  context <- list(
    apparie = TRUE, n_paires_completes = aligned$n_paires_completes,
    n_total_declare = aligned$n_total_declare, min_group_n = length(v1)
  )
  triggered <- st_evaluate_rules(context, family = "preconditions")

  fit <- stats::mcnemar.test(tab, correct = TRUE)
  statistic <- unname(fit$statistic)
  parameter <- unname(fit$parameter)

  discordant_b <- tab[1, 2]
  discordant_c <- tab[2, 1]
  effect_size <- if (discordant_c == 0) NA_real_ else as.numeric(discordant_b) / as.numeric(discordant_c)
  effect_size_name <- "Rapport de cotes (discordances)"
  test_name <- "Test de McNemar"

  descriptive <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(descriptive) <- c(aligned$level1, aligned$level2, "n")
  n_by_group <- stats::setNames(c(length(v1), length(v2)), c(aligned$level1, aligned$level2))
  justification <- .build_justification(test_name, triggered, context, NULL)

  .build_comparison_result(
    variable, group, test_name, statistic, parameter, fit$p.value,
    effect_size, effect_size_name, NA_real_, NA_real_,
    n_by_group, descriptive, justification, triggered
  )
}

.compare_correlation <- function(data, variable, group, forced_test) {
  complete <- data[!is.na(data[[variable]]) & !is.na(data[[group]]), , drop = FALSE]
  x <- .clean_numeric(complete[[variable]])
  y <- .clean_numeric(complete[[group]])
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]

  if (length(x) < 3) {
    cli::cli_abort("Correlation impossible entre '{variable}' et '{group}' : effectif insuffisant.")
  }
  if (stats::sd(x) == 0 || stats::sd(y) == 0) {
    cli::cli_abort("Correlation impossible : au moins une des deux variables ('{variable}', '{group}') est constante.")
  }

  shapiro_p_x <- .safe_shapiro(x)
  shapiro_p_y <- .safe_shapiro(y)

  context <- list(test_type = "correlation", shapiro_p_x = shapiro_p_x, shapiro_p_y = shapiro_p_y)
  override <- if (!is.null(forced_test)) "CORR-001" else character(0)
  triggered <- st_evaluate_rules(context, family = "preconditions", override = override)

  test_choice <- if (!is.null(forced_test)) {
    forced_test
  } else if ("CORR-001" %in% triggered$id) {
    "spearman"
  } else {
    "pearson"
  }

  fit <- stats::cor.test(x, y, method = test_choice)
  statistic <- unname(fit$statistic)
  parameter <- if (!is.null(fit$parameter)) unname(fit$parameter) else NA_real_
  conf_low <- .safe_conf_int(fit, 1)
  conf_high <- .safe_conf_int(fit, 2)
  effect_size <- unname(fit$estimate)
  effect_size_name <- if (identical(test_choice, "spearman")) "rho de Spearman" else "r de Pearson"
  test_name <- if (identical(test_choice, "spearman")) "Correlation de Spearman" else "Correlation de Pearson"

  descriptive <- rbind(.descriptive_stats(x, variable), .descriptive_stats(y, group))
  n_by_group <- c(n = length(x))
  justification <- .build_justification(test_name, triggered, context, forced_test)

  .build_comparison_result(
    variable, group, test_name, statistic, parameter, fit$p.value,
    effect_size, effect_size_name, conf_low, conf_high,
    n_by_group, descriptive, justification, triggered
  )
}

# --- Config / dictionnaire ---------------------------------------------------

.lookup_nature <- function(variable, config) {
  entry <- config$dictionnaire[[variable]]
  if (is.null(entry) || is.null(entry$nature)) {
    cli::cli_abort("Nature non declaree dans le dictionnaire pour '{variable}'. Declarer 'dictionnaire.{variable}.nature' dans config.yml.")
  }
  entry$nature
}

.lookup_forced_test <- function(config, variable, group) {
  comparisons <- config$analyse$comparaisons
  if (is.null(comparisons)) {
    return(NULL)
  }
  for (entry in comparisons) {
    if (variable %in% entry$variables && identical(entry$groupe, group) && !is.null(entry$forcer_test)) {
      return(entry$forcer_test)
    }
  }
  NULL
}

.lookup_identifier_variable <- function(config, data) {
  dictionary <- config$dictionnaire
  if (is.null(dictionary)) {
    return(NULL)
  }
  for (name in names(dictionary)) {
    if (identical(dictionary[[name]]$nature, "identifiant") && name %in% names(data)) {
      return(name)
    }
  }
  NULL
}

# --- Appariement ---------------------------------------------------------------

.align_pairs <- function(data, group, id_variable) {
  levels_present <- unique(stats::na.omit(as.character(data[[group]])))
  if (length(levels_present) != 2) {
    cli::cli_abort("Une comparaison appariee necessite exactement deux niveaux pour '{group}' ({length(levels_present)} observe(s)).")
  }
  level1 <- levels_present[1]
  level2 <- levels_present[2]

  subset1 <- data[!is.na(data[[group]]) & data[[group]] == level1, , drop = FALSE]
  subset2 <- data[!is.na(data[[group]]) & data[[group]] == level2, , drop = FALSE]

  ids1 <- as.character(subset1[[id_variable]])
  ids2 <- as.character(subset2[[id_variable]])

  common_ids <- intersect(ids1, ids2)
  all_ids <- union(ids1, ids2)

  list(
    level1 = level1, level2 = level2,
    data1 = subset1[match(common_ids, ids1), , drop = FALSE],
    data2 = subset2[match(common_ids, ids2), , drop = FALSE],
    n_paires_completes = length(common_ids),
    n_total_declare = length(all_ids)
  )
}

# --- Statistiques auxiliaires (normalite, variance, effets, IC) -------------

.clean_numeric <- function(x) {
  # Une colonne deja numerique ne doit jamais transiter par une conversion
  # en caractere : l'aller-retour texte introduit une perte de precision
  # flottante evitable sur les derniers chiffres.
  values <- if (is.numeric(x)) x else .to_numeric_permissive(as.character(x))
  values[!is.na(values)]
}

.safe_shapiro <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(x) > 5000 || stats::sd(x) == 0) {
    return(NA_real_)
  }
  tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
}

.safe_levene <- function(values, grouping) {
  grouping <- droplevels(factor(grouping))
  keep <- !is.na(values) & !is.na(grouping)
  values <- values[keep]
  grouping <- droplevels(grouping[keep])
  if (length(values) < 3 || nlevels(grouping) < 2) {
    return(NA_real_)
  }
  result <- tryCatch(car::leveneTest(values, grouping), error = function(e) NULL)
  if (is.null(result)) {
    return(NA_real_)
  }
  result[["Pr(>F)"]][1]
}

.eta_squared <- function(df) {
  fit_summary <- summary(stats::aov(value ~ grp, data = df))[[1]]
  fit_summary[["Sum Sq"]][1] / sum(fit_summary[["Sum Sq"]])
}

.cohens_d_effect <- function(x, y, var_equal) {
  df <- data.frame(value = c(x, y), grp = rep(c("g1", "g2"), c(length(x), length(y))))
  result <- rstatix::cohens_d(df, value ~ grp, var.equal = var_equal)
  result$effsize[1]
}

.wilcoxon_effect_size <- function(p_value, n) {
  if (is.na(p_value) || p_value <= 0 || p_value >= 1 || n <= 0) {
    return(NA_real_)
  }
  z <- stats::qnorm(p_value / 2, lower.tail = FALSE)
  unname(z / sqrt(n))
}

.safe_conf_int <- function(fit, index) {
  if (is.null(fit$conf.int) || length(fit$conf.int) < index) {
    return(NA_real_)
  }
  fit$conf.int[index]
}

.descriptive_stats <- function(values, group_label) {
  values <- values[!is.na(values)]
  data.frame(
    group = group_label, n = length(values),
    mean = if (length(values) > 0) mean(values) else NA_real_,
    sd = if (length(values) > 1) stats::sd(values) else NA_real_,
    median = if (length(values) > 0) stats::median(values) else NA_real_,
    q1 = if (length(values) > 0) stats::quantile(values, 0.25, names = FALSE) else NA_real_,
    q3 = if (length(values) > 0) stats::quantile(values, 0.75, names = FALSE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

.check_group_feasibility <- function(value_list, variable) {
  for (values in value_list) {
    if (length(values) == 0) {
      cli::cli_abort("Effectif nul pour '{variable}' dans au moins un groupe : test impossible.")
    }
  }
  all_values <- unlist(value_list, use.names = FALSE)
  if (length(unique(all_values)) <= 1) {
    cli::cli_abort("La variable '{variable}' est constante : test impossible.")
  }
}

# --- Objet de retour et justification ----------------------------------------

.build_comparison_result <- function(variable, group, test_name, statistic, parameter, p_value,
                                      effect_size, effect_size_name, conf_low, conf_high,
                                      n_by_group, descriptive_by_group, justification, rules_triggered) {
  derogations <- if (nrow(rules_triggered) > 0) rules_triggered$id[rules_triggered$derogation] else character(0)

  structure(
    list(
      variable = variable, group = group, test_name = test_name,
      statistic = statistic, parameter = parameter, p_value = p_value,
      effect_size = effect_size, effect_size_name = effect_size_name,
      conf_low = conf_low, conf_high = conf_high,
      n_by_group = n_by_group, descriptive_by_group = descriptive_by_group,
      justification = justification, rules_triggered = rules_triggered,
      derogations = derogations
    ),
    class = "statlab_comparison"
  )
}

.build_justification <- function(test_name, triggered, context, forced_test) {
  if (!is.null(forced_test)) {
    base_text <- sprintf("%s impose par l'operateur (config 'forcer_test').", test_name)
    if (nrow(triggered) > 0 && any(triggered$derogation)) {
      bypassed <- triggered$id[triggered$derogation]
      base_text <- paste0(base_text, sprintf(" Regle(s) contournee(s) : %s.", paste(bypassed, collapse = ", ")))
    }
    return(base_text)
  }

  applied <- triggered[!triggered$derogation & !is.na(triggered$justification), , drop = FALSE]
  if (nrow(applied) > 0) {
    return(paste(applied$justification, collapse = " "))
  }

  .default_justification(test_name, context)
}

.default_justification <- function(test_name, context) {
  details <- character(0)
  if (!is.null(context$shapiro_p) && !is.na(context$shapiro_p)) {
    details <- c(details, sprintf("normalite verifiee (Shapiro-Wilk, p = %s)", format(context$shapiro_p, digits = 3)))
  }
  if (!is.null(context$shapiro_p_x) && !is.na(context$shapiro_p_x) && !is.null(context$shapiro_p_y) && !is.na(context$shapiro_p_y)) {
    details <- c(details, sprintf(
      "normalite verifiee pour les deux variables (Shapiro-Wilk, p1 = %s, p2 = %s)",
      format(context$shapiro_p_x, digits = 3), format(context$shapiro_p_y, digits = 3)
    ))
  }
  if (!is.null(context$levene_p) && !is.na(context$levene_p)) {
    details <- c(details, sprintf("variances homogenes (test de Levene, p = %s)", format(context$levene_p, digits = 3)))
  }
  if (!is.null(context$effectif_theorique_min) && !is.na(context$effectif_theorique_min)) {
    details <- c(details, sprintf("effectifs theoriques suffisants (minimum = %s)", format(context$effectif_theorique_min, digits = 3)))
  }

  if (length(details) == 0) {
    sprintf("%s retenu : aucune regle de precondition n'a ete declenchee.", test_name)
  } else {
    sprintf("%s retenu : %s.", test_name, paste(details, collapse = " ; "))
  }
}

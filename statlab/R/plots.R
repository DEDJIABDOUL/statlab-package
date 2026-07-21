# Production de figures a partir de donnees preparees, avec libelles, unites
# et ordre de modalites issus du dictionnaire, et charte graphique st_theme().

#' @importFrom ggplot2 .data
NULL

#' Boite a moustaches d'une variable continue selon un groupe
#'
#' Trace une boite a moustaches (moustaches de Tukey), avec les effectifs
#' affiches sous chaque groupe. Si l'effectif total est inferieur a 100, les
#' points individuels sont superposes. Si `test_result` est fourni (issu de
#' [st_compare()]), une annotation de significativite est ajoutee.
#'
#' @param data Un `data.frame`.
#' @param variable Nom de la variable continue (chaine).
#' @param group Nom de la variable de groupement (chaine).
#' @param config Un objet `statlab_config`.
#' @param test_result Optionnel, resultat de [st_compare()].
#'
#' @return Un objet `ggplot`.
#' @export
st_plot_box <- function(data, variable, group, config, test_result = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(variable, min.chars = 1)
  checkmate::assert_string(group, min.chars = 1)
  checkmate::assert_class(config, "statlab_config")
  checkmate::assert_list(test_result, null.ok = TRUE)
  .assert_variable_presente(data, variable)
  .assert_variable_presente(data, group)

  dictionary <- config$dictionnaire
  values <- .clean_numeric_na(data[[variable]])
  groupes_bruts <- data[[group]]

  complete_idx <- !is.na(values) & !is.na(groupes_bruts)
  n_excluded <- sum(!complete_idx)
  if (sum(complete_idx) == 0) {
    cli::cli_abort("Aucune observation complete pour '{variable}' x '{group}' : impossible de tracer le graphique.")
  }

  plot_data <- data.frame(
    valeur = values[complete_idx],
    groupe = .apply_dictionary_levels_vec(groupes_bruts[complete_idx], dictionary[[group]])
  )
  plot_data <- plot_data[!is.na(plot_data$groupe), , drop = FALSE]

  jitter_needed <- nrow(plot_data) < 100
  n_par_groupe <- table(plot_data$groupe)
  n_labels <- data.frame(groupe = names(n_par_groupe), n = as.integer(n_par_groupe))
  y_range <- range(plot_data$valeur)
  y_etendue <- diff(y_range)
  y_etendue <- if (y_etendue == 0) 1 else y_etendue
  n_labels$y <- y_range[1] - y_etendue * 0.08

  niveaux <- levels(droplevels(plot_data$groupe))
  couleurs <- stats::setNames(st_palette(length(niveaux), "qualitative", "ecran"), niveaux)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$groupe, y = .data$valeur, fill = .data$groupe)) +
    ggplot2::geom_boxplot(outlier.shape = if (jitter_needed) NA else 19, width = 0.6, alpha = 0.85, staplewidth = 0.3)

  if (jitter_needed) {
    p <- p + ggplot2::geom_jitter(width = 0.12, height = 0, size = 1.4, alpha = 0.55, color = "grey20")
  }

  p <- p +
    ggplot2::geom_text(
      data = n_labels, ggplot2::aes(x = .data$groupe, y = .data$y, label = sprintf("n = %d", .data$n)),
      inherit.aes = FALSE, size = 3, color = "grey40"
    ) +
    ggplot2::scale_fill_manual(values = couleurs)

  caption_test <- NULL
  if (!is.null(test_result)) {
    if (length(niveaux) == 2) {
      p <- p + ggsignif::geom_signif(
        comparisons = list(niveaux),
        annotations = .format_p_value(test_result$p_value),
        tip_length = 0.01, textsize = 3.2
      )
    } else {
      caption_test <- sprintf("%s (%s)", .format_p_value(test_result$p_value), test_result$test_name)
    }
  }

  variable_label <- .variable_label(variable, dictionary)
  group_label <- .variable_label(group, dictionary)
  caption <- .combine_captions(.missing_caption(n_excluded), caption_test)

  p + ggplot2::labs(
    title = sprintf("%s selon %s", variable_label, group_label),
    x = NULL, y = variable_label, caption = caption
  ) + st_theme() + ggplot2::theme(legend.position = "none")
}

#' Diagramme en barres d'une variable categorielle, avec effectifs et pourcentages
#'
#' @param data Un `data.frame`.
#' @param variable Nom de la variable categorielle (chaine).
#' @param group Optionnel, nom d'une variable de groupement (chaine).
#' @param config Un objet `statlab_config`.
#' @param test_result Optionnel, resultat de [st_compare()].
#'
#' @return Un objet `ggplot`.
#' @export
st_plot_bar <- function(data, variable, group = NULL, config, test_result = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(variable, min.chars = 1)
  checkmate::assert_string(group, min.chars = 1, null.ok = TRUE)
  checkmate::assert_class(config, "statlab_config")
  checkmate::assert_list(test_result, null.ok = TRUE)
  .assert_variable_presente(data, variable)
  if (!is.null(group)) .assert_variable_presente(data, group)

  dictionary <- config$dictionnaire
  categories <- .apply_dictionary_levels_vec(data[[variable]], dictionary[[variable]])
  variable_label <- .variable_label(variable, dictionary)

  if (is.null(group)) {
    complete_idx <- !is.na(categories)
    n_excluded <- sum(!complete_idx)
    valeurs <- droplevels(categories[complete_idx])
    if (length(valeurs) == 0) {
      cli::cli_abort("Aucune observation complete pour '{variable}' : impossible de tracer le graphique.")
    }
    n_total <- length(valeurs)
    counts <- table(valeurs)
    resume <- data.frame(
      categorie = factor(names(counts), levels = names(counts)),
      n = as.integer(counts)
    )
    resume$pourcentage <- resume$n / n_total
    ic <- t(mapply(.wilson_ci, resume$n, n_total))
    resume$ic_bas <- ic[, 1]
    resume$ic_haut <- ic[, 2]

    couleurs <- stats::setNames(st_palette(nrow(resume), "qualitative", "ecran"), levels(resume$categorie))

    p <- ggplot2::ggplot(resume, ggplot2::aes(x = .data$categorie, y = .data$pourcentage, fill = .data$categorie)) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ic_bas, ymax = .data$ic_haut), width = 0.15, color = "grey30") +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%d (%s)", .data$n, scales::percent(.data$pourcentage, accuracy = 1))),
        vjust = -1.4, size = 3.1
      ) +
      ggplot2::scale_fill_manual(values = couleurs) +
      ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.18))) +
      ggplot2::labs(
        title = variable_label, x = NULL, y = "Pourcentage",
        caption = .combine_captions(.missing_caption(n_excluded), sprintf("Denominateur : n = %d.", n_total))
      ) + st_theme() + ggplot2::theme(legend.position = "none")

    return(p)
  }

  groupes <- .apply_dictionary_levels_vec(data[[group]], dictionary[[group]])
  complete_idx <- !is.na(categories) & !is.na(groupes)
  n_excluded <- sum(!complete_idx)
  categories <- droplevels(categories[complete_idx])
  groupes <- droplevels(groupes[complete_idx])
  if (length(categories) == 0) {
    cli::cli_abort("Aucune observation complete pour '{variable}' x '{group}' : impossible de tracer le graphique.")
  }

  n_par_groupe <- table(groupes)
  resume <- as.data.frame(table(groupe = groupes, categorie = categories), stringsAsFactors = FALSE)
  resume$n_groupe <- as.integer(n_par_groupe[resume$groupe])
  resume$pourcentage <- resume$Freq / resume$n_groupe
  ic <- t(mapply(.wilson_ci, resume$Freq, resume$n_groupe))
  resume$ic_bas <- ic[, 1]
  resume$ic_haut <- ic[, 2]
  resume$categorie <- factor(resume$categorie, levels = levels(categories))
  resume$groupe <- factor(resume$groupe, levels = levels(groupes))

  couleurs <- stats::setNames(st_palette(nlevels(resume$categorie), "qualitative", "ecran"), levels(resume$categorie))
  group_label <- .variable_label(group, dictionary)

  caption_test <- if (!is.null(test_result)) {
    sprintf("%s (%s)", .format_p_value(test_result$p_value), test_result$test_name)
  } else {
    NULL
  }
  denominateurs <- paste(sprintf("%s : n = %d", names(n_par_groupe), as.integer(n_par_groupe)), collapse = " ; ")

  ggplot2::ggplot(resume, ggplot2::aes(x = .data$groupe, y = .data$pourcentage, fill = .data$categorie)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$ic_bas, ymax = .data$ic_haut),
      position = ggplot2::position_dodge(width = 0.75), width = 0.15, color = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%d (%s)", .data$Freq, scales::percent(.data$pourcentage, accuracy = 1))),
      position = ggplot2::position_dodge(width = 0.75), vjust = -1.2, size = 2.8
    ) +
    ggplot2::scale_fill_manual(values = couleurs) +
    ggplot2::scale_y_continuous(labels = scales::percent, expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(
      title = sprintf("%s selon %s", variable_label, group_label), x = NULL, y = "Pourcentage",
      caption = .combine_captions(.missing_caption(n_excluded), sprintf("Denominateur par groupe (%s).", denominateurs), caption_test)
    ) + st_theme()
}

#' Nuage de points entre deux variables continues, avec droite de regression
#'
#' @param data Un `data.frame`.
#' @param x Nom de la variable en abscisse (chaine).
#' @param y Nom de la variable en ordonnee (chaine).
#' @param config Un objet `statlab_config`.
#' @param group Optionnel, nom d'une variable de groupement (chaine).
#' @param smooth Si `TRUE` (defaut), ajoute une droite de regression avec
#'   intervalle de confiance et annote le coefficient de correlation et sa p-value.
#'
#' @return Un objet `ggplot`.
#' @export
st_plot_scatter <- function(data, x, y, config, group = NULL, smooth = TRUE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(x, min.chars = 1)
  checkmate::assert_string(y, min.chars = 1)
  checkmate::assert_class(config, "statlab_config")
  checkmate::assert_string(group, min.chars = 1, null.ok = TRUE)
  checkmate::assert_flag(smooth)
  .assert_variable_presente(data, x)
  .assert_variable_presente(data, y)
  if (!is.null(group)) .assert_variable_presente(data, group)

  dictionary <- config$dictionnaire
  x_values <- .clean_numeric_na(data[[x]])
  y_values <- .clean_numeric_na(data[[y]])
  groupes_bruts <- if (!is.null(group)) .apply_dictionary_levels_vec(data[[group]], dictionary[[group]]) else NULL

  complete_idx <- !is.na(x_values) & !is.na(y_values)
  if (!is.null(group)) complete_idx <- complete_idx & !is.na(groupes_bruts)
  n_excluded <- sum(!complete_idx)
  if (sum(complete_idx) < 2) {
    cli::cli_abort("Moins de deux observations completes pour '{x}' x '{y}' : impossible de tracer le graphique.")
  }

  plot_data <- data.frame(x = x_values[complete_idx], y = y_values[complete_idx])
  if (!is.null(group)) plot_data$groupe <- droplevels(groupes_bruts[complete_idx])

  p <- if (!is.null(group)) {
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y, color = .data$groupe))
  } else {
    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y))
  }
  p <- p + ggplot2::geom_point(alpha = 0.7, size = 1.8)

  correlation_text <- NULL
  if (isTRUE(smooth)) {
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.6, alpha = 0.15)
    fit <- stats::cor.test(plot_data$x, plot_data$y)
    correlation_text <- sprintf("r = %s ; %s", .format_decimal_fr(unname(fit$estimate), 2), .format_p_value(fit$p.value))
  }

  if (!is.null(group)) {
    couleurs <- stats::setNames(st_palette(nlevels(plot_data$groupe), "qualitative", "ecran"), levels(plot_data$groupe))
    p <- p + ggplot2::scale_color_manual(values = couleurs)
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  x_label <- .variable_label(x, dictionary)
  y_label <- .variable_label(y, dictionary)

  p + ggplot2::labs(
    title = sprintf("%s selon %s", y_label, x_label), x = x_label, y = y_label,
    subtitle = correlation_text, caption = .missing_caption(n_excluded)
  ) + st_theme()
}

#' Enregistre un graphique dans toutes les combinaisons format x declinaison
#'
#' Produit un fichier par combinaison de `formats` et `variants`, nomme
#' systematiquement `{name}_{variant}.{ext}` (PDF, SVG) et
#' `{name}_{variant}_{dpi}.png` pour chaque resolution PNG demandee.
#'
#' @param plot Un objet `ggplot`.
#' @param name Prefixe de nom de fichier (peut inclure un chemin).
#' @param config Un objet `statlab_config`.
#' @param formats Formats a produire, parmi "pdf", "png", "svg".
#' @param variants Declinaisons a produire, parmi "ecran", "impression",
#'   "projection". Par defaut, `config$rendu$declinaisons`.
#' @param width Largeur en pouces.
#' @param height Hauteur en pouces.
#' @param dpi Resolution(s) PNG, en points par pouce.
#'
#' @return Invisiblement, le vecteur des chemins de fichiers produits.
#' @export
st_save_plot <- function(plot, name, config, formats = c("pdf", "png", "svg"),
                          variants = config$rendu$declinaisons, width, height, dpi = c(300, 600)) {
  checkmate::assert_class(plot, "ggplot")
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_class(config, "statlab_config")
  formats <- match.arg(formats, choices = c("pdf", "png", "svg"), several.ok = TRUE)
  if (is.null(variants)) {
    cli::cli_abort(c(
      "Aucune declinaison a produire.",
      "i" = "Declarer {.field rendu.declinaisons} dans config.yml, ou passer l'argument {.arg variants} explicitement."
    ))
  }
  checkmate::assert_character(variants, min.len = 1)
  invalides <- setdiff(variants, c("ecran", "impression", "projection"))
  if (length(invalides) > 0) {
    cli::cli_abort("Declinaison(s) invalide(s) : {paste(invalides, collapse = ', ')}. Valeurs valides : ecran, impression, projection.")
  }
  checkmate::assert_number(width, lower = 0)
  checkmate::assert_number(height, lower = 0)
  checkmate::assert_numeric(dpi, lower = 1, min.len = 1)

  fichiers <- character(0)
  for (variant in variants) {
    plot_variant <- st_apply_variant(plot, variant)
    for (format in formats) {
      if (identical(format, "png")) {
        for (resolution in dpi) {
          fichier <- sprintf("%s_%s_%d.png", name, variant, as.integer(resolution))
          ggplot2::ggsave(fichier, plot = plot_variant, device = ragg::agg_png,
                           width = width, height = height, units = "in", dpi = resolution)
          fichiers <- c(fichiers, fichier)
        }
      } else if (identical(format, "svg")) {
        fichier <- sprintf("%s_%s.svg", name, variant)
        ggplot2::ggsave(fichier, plot = plot_variant, device = svglite::svglite,
                         width = width, height = height, units = "in")
        fichiers <- c(fichiers, fichier)
      } else {
        fichier <- sprintf("%s_%s.pdf", name, variant)
        ggplot2::ggsave(fichier, plot = plot_variant, device = grDevices::cairo_pdf,
                         width = width, height = height, units = "in")
        fichiers <- c(fichiers, fichier)
      }
    }
  }

  cli::cli_alert_success("{length(fichiers)} fichier(s) produit(s) pour '{name}'.")
  invisible(fichiers)
}

# --- Helpers communs ----------------------------------------------------------

.assert_variable_presente <- function(data, variable) {
  if (!variable %in% names(data)) {
    cli::cli_abort("Variable introuvable dans les donnees : '{variable}'.")
  }
}

.clean_numeric_na <- function(x) {
  # Contrairement a .clean_numeric() (tests.R), les NA doivent etre conserves
  # ici : l'exclusion et le comptage des valeurs manquantes sont a la charge
  # de l'appelant (note de graphique), pas de cet helper de conversion.
  if (is.numeric(x)) x else .to_numeric_permissive(as.character(x))
}

.apply_dictionary_levels_vec <- function(x, entry) {
  x_chr <- as.character(x)
  if (!is.null(entry) && !is.null(entry$modalites)) {
    return(factor(x_chr, levels = entry$modalites))
  }
  factor(x_chr)
}

.variable_label <- function(variable, dictionary) {
  entry <- dictionary[[variable]]
  if (is.null(entry) || is.null(entry$libelle)) {
    return(variable)
  }
  if (!is.null(entry$unite)) {
    return(sprintf("%s (%s)", entry$libelle, entry$unite))
  }
  entry$libelle
}

.missing_caption <- function(n_excluded) {
  if (n_excluded == 0) {
    return(NULL)
  }
  sprintf("%d observation(s) exclue(s) pour valeur manquante.", n_excluded)
}

.combine_captions <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  if (length(parts) == 0) {
    return(NULL)
  }
  paste(unlist(parts), collapse = " ; ")
}

.wilson_ci <- function(x, n) {
  if (is.na(n) || n == 0) {
    return(c(NA_real_, NA_real_))
  }
  resultat <- suppressWarnings(stats::prop.test(x, n, correct = FALSE))
  resultat$conf.int
}

.format_p_value <- function(p) {
  if (is.null(p) || is.na(p)) {
    return(NA_character_)
  }
  if (p < 0.001) {
    return("p < 0,001")
  }
  sprintf("p = %s", .format_decimal_fr(p, 3))
}

.format_decimal_fr <- function(x, digits) {
  sub("\\.", ",", formatC(x, digits = digits, format = "f"))
}

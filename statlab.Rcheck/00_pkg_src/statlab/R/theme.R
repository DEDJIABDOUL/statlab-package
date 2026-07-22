# =============================================================================
# Charte graphique : theme ggplot2 et palettes, en trois declinaisons
# obligatoires (ecran, impression, projection). Les palettes sont
# construites pour rester distinguables en deuteranopie et en
# protanopie (colorspace::deutan/protan) ainsi qu'apres conversion en
# niveaux de gris (colorspace::desaturate) ; la declinaison "impression"
# n'utilise jamais de couleur (palette en gris), le motif de points/traits
# assurant le canal de distinction supplementaire.
# =============================================================================

#' Theme ggplot2 de la charte graphique
#'
#' Fond blanc, aucune grille verticale, grille horizontale discrete, axes
#' sobres, legende en bas, titres alignes a gauche, marges genereuses.
#' Les trois declinaisons ajustent la taille de police, l'epaisseur des
#' traits et le contraste :
#' - `"ecran"` : couleur, corps `base_size` (11 pt par defaut), traits fins.
#' - `"impression"` : corps `base_size - 1`, traits renforces, contrastes
#'   accrus (destine a une palette en niveaux de gris, cf. [st_palette()]).
#' - `"projection"` : corps `base_size + 5`, traits epais, contraste
#'   eleve, elements de grille reduits au minimum.
#'
#' @param base_size Taille de police de base (pt) pour la declinaison
#'   `"ecran"` ; les autres declinaisons en derivent un decalage fixe.
#' @param variant Declinaison graphique.
#'
#' @return Un objet `theme` ggplot2, avec l'attribut `variant`.
#' @export
st_theme <- function(base_size = 11, variant = c("ecran", "impression", "projection")) {
  checkmate::assert_number(base_size, lower = 1)
  variant <- match.arg(variant)

  size_offset <- c(ecran = 0, impression = -1, projection = 5)[[variant]]
  effective_size <- base_size + size_offset

  line_width <- c(ecran = 0.3, impression = 0.6, projection = 0.9)[[variant]]
  grid_color <- c(ecran = "grey85", impression = "grey55", projection = "grey75")[[variant]]
  axis_color <- c(ecran = "grey30", impression = "black", projection = "black")[[variant]]
  margin_scale <- c(ecran = 1, impression = 1, projection = 1.3)[[variant]]

  font_family <- .resolve_font_family()

  result <- ggplot2::theme_minimal(base_size = effective_size, base_family = font_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = grid_color, linewidth = line_width * 0.6),
      axis.line = ggplot2::element_line(color = axis_color, linewidth = line_width),
      axis.ticks = ggplot2::element_line(color = axis_color, linewidth = line_width),
      axis.text = ggplot2::element_text(color = axis_color),
      axis.title = ggplot2::element_text(color = axis_color),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = ggplot2::rel(1.15), margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(hjust = 0, color = "grey30", margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(hjust = 0, color = "grey40", size = ggplot2::rel(0.75)),
      strip.text = ggplot2::element_text(face = "bold", color = axis_color),
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = 16 * margin_scale, r = 20 * margin_scale,
        b = 12 * margin_scale, l = 12 * margin_scale
      )
    )

  if (identical(variant, "projection")) {
    result <- result + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = grid_color, linewidth = line_width * 0.45),
      legend.text = ggplot2::element_text(size = ggplot2::rel(1))
    )
  }

  attr(result, "variant") <- variant
  result
}

#' Palette de couleurs de la charte graphique
#'
#' Palettes definies dans `inst/templates/palettes.yml` (editable sans
#' toucher au code). Pour `variant = "impression"`, retourne toujours une
#' palette en niveaux de gris (jamais de couleur imprimee), quel que soit
#' `type` : le motif de remplissage et le type de trait (cf.
#' `inst/templates/palettes.yml`, champs `formes`/`types_trait`) portent
#' alors la distinction supplementaire.
#'
#' Pour `type = "qualitative"`, la distinguabilite de la palette retenue
#' est verifiee en deuteranopie et en protanopie
#' ([colorspace::deutan()]/[colorspace::protan()]) ainsi qu'apres
#' conversion en niveaux de gris ([colorspace::desaturate()]) ; un
#' avertissement est emis (jamais silencieux) si `n` couleurs simultanees
#' reduisent cette distinguabilite sous le seuil declare dans
#' `palettes.yml`.
#'
#' @param n Nombre (entier positif) de couleurs demandees.
#' @param type Nature de la palette : categorielle (`"qualitative"`),
#'   magnitude continue a une teinte (`"sequentielle"`), ou deux poles
#'   opposes (`"divergente"`).
#' @param variant Declinaison graphique (cf. [st_theme()]).
#'
#' @return Un vecteur (chr) de `n` couleurs hexadecimales.
#' @export
st_palette <- function(n, type = c("qualitative", "sequentielle", "divergente"),
                        variant = c("ecran", "impression", "projection")) {
  checkmate::assert_count(n, positive = TRUE)
  type <- match.arg(type)
  variant <- match.arg(variant)

  if (identical(variant, "impression")) {
    return(.grayscale_palette(n))
  }

  palettes <- .load_palettes()
  switch(type,
    qualitative = .qualitative_palette(n, palettes),
    sequentielle = colorspace::sequential_hcl(n, palette = palettes$sequentielle$teinte),
    divergente = colorspace::diverging_hcl(n, palette = palettes$divergente$teinte)
  )
}

#' Appliquer une declinaison a un graphique existant
#'
#' Remplace le theme d'un graphique ggplot2 deja construit par celui de la
#' declinaison demandee. Pour une conversion complete en niveaux de gris
#' (couleurs de donnees comprises), il est preferable de reconstruire le
#' graphique en appelant directement [st_palette()] avec
#' `variant = "impression"` : cette fonction ne modifie que l'habillage
#' (theme), pas les couleurs deja fixees dans les couches du graphique.
#'
#' @param plot Un objet `ggplot`.
#' @param variant Declinaison graphique (cf. [st_theme()]).
#'
#' @return Le graphique `ggplot`, avec la declinaison appliquee.
#' @export
st_apply_variant <- function(plot, variant = c("ecran", "impression", "projection")) {
  checkmate::assert_class(plot, "ggplot")
  variant <- match.arg(variant)
  plot + st_theme(variant = variant)
}

#' Planche de demonstration de la charte graphique
#'
#' Produit trois graphiques de demonstration (boite a moustaches, barres,
#' nuage de points), chacun decline en trois declinaisons graphiques et
#' quatre modes de simulation (vision normale, deuteranopie, protanopie,
#' niveaux de gris), disposes en grille de facettes.
#'
#' @param n_groups Nombre de groupes (couleurs) illustres (2 a 6).
#'
#' @return Une liste de trois objets `ggplot` (`boite`, `barres`,
#'   `nuage_points`) ; chacun a imprimer ou enregistrer separement (ex :
#'   `print(st_theme_preview()$boite)`).
#' @export
st_theme_preview <- function(n_groups = 4) {
  checkmate::assert_count(n_groups, positive = TRUE)
  checkmate::assert_true(n_groups <= 6)

  groupes <- paste0("Groupe ", LETTERS[seq_len(n_groups)])
  variantes <- c("ecran", "impression", "projection")
  simulations <- c("vision normale", "deuteranopie", "protanopie", "niveaux de gris")

  couleurs <- .preview_color_grid(groupes, variantes, simulations)

  list(
    boite = .preview_plot_box(groupes, variantes, simulations, couleurs),
    barres = .preview_plot_bar(groupes, variantes, simulations, couleurs),
    nuage_points = .preview_plot_scatter(groupes, variantes, simulations, couleurs)
  )
}

# --- Palettes internes -----------------------------------------------------

.load_palettes <- function() {
  path <- system.file("templates", "palettes.yml", package = "statlab")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort("Fichier de palettes introuvable : inst/templates/palettes.yml")
  }
  yaml::read_yaml(path)
}

.qualitative_palette <- function(n, palettes) {
  spec <- palettes$qualitative
  base_colors <- unlist(spec$couleurs)
  if (n > length(base_colors)) {
    cli::cli_abort(c(
      "Palette qualitative demandee ({n} couleurs) au-dela du maximum disponible ({length(base_colors)}).",
      "i" = "Regrouper des categories ou utiliser un facettage plutot qu'une palette a {n} couleurs simultanees."
    ))
  }
  colors <- base_colors[seq_len(n)]
  .validate_qualitative_palette(colors, spec$seuil_avertissement, n)
  colors
}

.validate_qualitative_palette <- function(colors, threshold, n) {
  if (length(colors) < 2) {
    return(invisible(NULL))
  }

  checks <- list(
    "vision normale" = colors,
    "deuteranopie" = colorspace::deutan(colors),
    "protanopie" = colorspace::protan(colors),
    "niveaux de gris" = colorspace::desaturate(colors)
  )

  problems <- character(0)
  for (label in names(checks)) {
    distance <- .min_pairwise_lab_distance(checks[[label]])
    if (distance < threshold) {
      problems <- c(problems, sprintf("%s (distance minimale %.1f < seuil %.1f)", label, distance, threshold))
    }
  }

  if (length(problems) > 0) {
    cli::cli_alert_warning(paste0(
      sprintf("Distinguabilite reduite pour %d couleurs simultanees : ", n),
      paste(problems, collapse = " ; "),
      ". Envisager de regrouper des categories ou de faceter."
    ))
  }
  invisible(NULL)
}

.min_pairwise_lab_distance <- function(colors) {
  lab <- methods::as(colorspace::hex2RGB(colors), "LAB")
  coordinates <- colorspace::coords(lab)
  distances <- as.matrix(stats::dist(coordinates))
  diag(distances) <- NA
  min(distances, na.rm = TRUE)
}

.grayscale_palette <- function(n) {
  palettes <- .load_palettes()
  spec <- palettes$gris_impression
  l_min <- spec$luminosite_min
  l_max <- spec$luminosite_max
  l_values <- if (n == 1) (l_min + l_max) / 2 else seq(l_max, l_min, length.out = n)
  colorspace::hex(colorspace::polarLUV(L = l_values, C = 0, H = 0))
}

.palette_shapes <- function(n) {
  palettes <- .load_palettes()
  shapes <- unlist(palettes$formes)
  if (n > length(shapes)) {
    cli::cli_abort("Nombre de formes demandees ({n}) au-dela du maximum disponible ({length(shapes)}).")
  }
  as.integer(shapes[seq_len(n)])
}

.palette_linetypes <- function(n) {
  palettes <- .load_palettes()
  types <- unlist(palettes$types_trait)
  if (n > length(types)) {
    cli::cli_abort("Nombre de types de trait demandes ({n}) au-dela du maximum disponible ({length(types)}).")
  }
  types[seq_len(n)]
}

# --- Polices ------------------------------------------------------------------

.resolve_font_family <- function() {
  available <- tryCatch(systemfonts::system_fonts()$family, error = function(e) character(0))
  preferred <- c("Segoe UI", "Helvetica Neue", "Helvetica", "Arial", "Liberation Sans")
  for (font in preferred) {
    if (font %in% available) {
      return(font)
    }
  }
  ""
}

# --- Planche de demonstration --------------------------------------------------

.preview_color_grid <- function(groupes, variantes, simulations) {
  n_groupes <- length(groupes)
  grid <- expand.grid(
    variant = variantes, simulation = simulations, groupe = groupes,
    stringsAsFactors = FALSE
  )
  grid$couleur <- NA_character_

  for (variant in variantes) {
    base_colors <- stats::setNames(st_palette(n_groupes, type = "qualitative", variant = variant), groupes)
    for (simulation in simulations) {
      colors_for_simulation <- switch(simulation,
        "vision normale" = base_colors,
        "deuteranopie" = stats::setNames(colorspace::deutan(base_colors), groupes),
        "protanopie" = stats::setNames(colorspace::protan(base_colors), groupes),
        "niveaux de gris" = stats::setNames(colorspace::desaturate(base_colors), groupes)
      )
      idx <- grid$variant == variant & grid$simulation == simulation
      grid$couleur[idx] <- colors_for_simulation[grid$groupe[idx]]
    }
  }

  grid$simulation <- factor(grid$simulation, levels = simulations)
  grid$variant <- factor(grid$variant, levels = variantes)
  grid
}

.preview_plot_box <- function(groupes, variantes, simulations, couleurs) {
  set.seed(42)
  base_data <- data.frame(
    groupe = rep(groupes, each = 30),
    valeur = unlist(lapply(seq_along(groupes), function(i) stats::rnorm(30, mean = i * 3, sd = 2)))
  )
  data <- .preview_expand(base_data, variantes, simulations, couleurs)

  ggplot2::ggplot(data, ggplot2::aes(x = groupe, y = valeur, fill = couleur)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_grid(simulation ~ variant) +
    ggplot2::labs(title = "Boite a moustaches", x = NULL, y = NULL) +
    st_theme() +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.preview_plot_bar <- function(groupes, variantes, simulations, couleurs) {
  base_data <- data.frame(groupe = groupes, effectif = seq(10, 10 + 5 * (length(groupes) - 1), by = 5))
  data <- .preview_expand(base_data, variantes, simulations, couleurs)

  ggplot2::ggplot(data, ggplot2::aes(x = groupe, y = effectif, fill = couleur)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_grid(simulation ~ variant) +
    ggplot2::labs(title = "Barres", x = NULL, y = NULL) +
    st_theme() +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.preview_plot_scatter <- function(groupes, variantes, simulations, couleurs) {
  set.seed(42)
  n_par_groupe <- 20
  base_data <- data.frame(
    groupe = rep(groupes, each = n_par_groupe),
    x = stats::rnorm(n_par_groupe * length(groupes)),
    y = stats::rnorm(n_par_groupe * length(groupes))
  )
  data <- .preview_expand(base_data, variantes, simulations, couleurs)

  ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = couleur)) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_color_identity() +
    ggplot2::facet_grid(simulation ~ variant) +
    ggplot2::labs(title = "Nuage de points", x = NULL, y = NULL) +
    st_theme() +
    ggplot2::theme(legend.position = "none")
}

.preview_expand <- function(base_data, variantes, simulations, couleurs) {
  combinations <- expand.grid(variant = variantes, simulation = simulations, stringsAsFactors = FALSE)
  expanded <- lapply(seq_len(nrow(combinations)), function(i) {
    chunk <- base_data
    chunk$variant <- combinations$variant[i]
    chunk$simulation <- combinations$simulation[i]
    chunk
  })
  data <- do.call(rbind, expanded)
  data <- merge(data, couleurs, by = c("variant", "simulation", "groupe"))
  data$simulation <- factor(data$simulation, levels = simulations)
  data$variant <- factor(data$variant, levels = variantes)
  data
}

# Tests : st_theme -----------------------------------------------------------

test_that("st_theme retourne un objet theme avec la declinaison en attribut", {
  th <- st_theme(variant = "ecran")
  expect_s3_class(th, "theme")
  expect_equal(attr(th, "variant"), "ecran")
})

test_that("st_theme derive la taille de police attendue par declinaison (base_size = 11 par defaut)", {
  th_ecran <- st_theme(variant = "ecran")
  th_impression <- st_theme(variant = "impression")
  th_projection <- st_theme(variant = "projection")

  expect_equal(th_ecran$text$size, 11)
  expect_equal(th_impression$text$size, 10)
  expect_equal(th_projection$text$size, 16)
})

test_that("st_theme respecte un base_size personnalise", {
  th <- st_theme(base_size = 20, variant = "projection")
  expect_equal(th$text$size, 25)
})

test_that("st_theme supprime la grille verticale et conserve une grille horizontale discrete", {
  th <- st_theme(variant = "ecran")
  expect_true(inherits(th$panel.grid.major.x, "element_blank"))
  expect_true(inherits(th$panel.grid.minor.x, "element_blank"))
  expect_false(inherits(th$panel.grid.major.y, "element_blank"))
})

test_that("st_theme aligne les titres a gauche", {
  th <- st_theme()
  expect_equal(th$plot.title$hjust, 0)
  expect_equal(th$plot.subtitle$hjust, 0)
})

test_that("st_theme place la legende en bas par defaut", {
  th <- st_theme()
  expect_equal(th$legend.position, "bottom")
})

# Tests : st_palette -----------------------------------------------------------

test_that("st_palette qualitative retourne n couleurs hexadecimales distinctes", {
  couleurs <- st_palette(4, "qualitative", "ecran")
  expect_length(couleurs, 4)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", couleurs)))
  expect_equal(length(unique(couleurs)), 4)
})

test_that("st_palette qualitative s'arrete au-dela du nombre de couleurs disponibles", {
  expect_error(st_palette(50, "qualitative", "ecran"), "au-dela du maximum")
})

test_that("st_palette qualitative avertit d'une distinguabilite reduite pour un grand nombre de couleurs", {
  expect_message(st_palette(7, "qualitative", "ecran"), "[Dd]istinguabilite reduite")
})

test_that("st_palette qualitative ne produit aucun avertissement pour un nombre restreint de couleurs", {
  messages <- testthat::capture_messages(st_palette(4, "qualitative", "ecran"))
  expect_length(messages, 0)
})

test_that("st_palette qualitative reste distinguable en deuteranopie/protanopie/niveaux de gris pour n petit", {
  couleurs <- st_palette(4, "qualitative", "ecran")
  lab_dist <- function(x) {
    lab <- methods::as(colorspace::hex2RGB(x), "LAB")
    d <- as.matrix(stats::dist(colorspace::coords(lab)))
    diag(d) <- NA
    min(d, na.rm = TRUE)
  }
  expect_gt(lab_dist(colorspace::deutan(couleurs)), 5)
  expect_gt(lab_dist(colorspace::protan(couleurs)), 5)
  expect_gt(lab_dist(colorspace::desaturate(couleurs)), 5)
})

test_that("st_palette variant = impression retourne toujours des niveaux de gris, quel que soit 'type'", {
  is_gray <- function(hex_colors) {
    rgb <- grDevices::col2rgb(hex_colors)
    all(rgb["red", ] == rgb["green", ] & rgb["green", ] == rgb["blue", ])
  }
  expect_true(is_gray(st_palette(4, "qualitative", "impression")))
  expect_true(is_gray(st_palette(4, "sequentielle", "impression")))
  expect_true(is_gray(st_palette(4, "divergente", "impression")))
})

test_that("st_palette sequentielle et divergente retournent n couleurs valides", {
  seq_colors <- st_palette(5, "sequentielle", "ecran")
  div_colors <- st_palette(5, "divergente", "ecran")
  expect_length(seq_colors, 5)
  expect_length(div_colors, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", seq_colors)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", div_colors)))
})

test_that("st_palette est deterministe (memes couleurs a chaque appel)", {
  expect_identical(st_palette(4, "qualitative", "ecran"), st_palette(4, "qualitative", "ecran"))
})

# Tests : st_apply_variant -----------------------------------------------------

test_that("st_apply_variant applique la declinaison demandee a un graphique existant", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) + ggplot2::geom_boxplot()
  p2 <- st_apply_variant(p, "projection")

  expect_s3_class(p2, "ggplot")
  theme_applied <- Filter(function(l) inherits(l, "theme"), p2$theme)
  expect_equal(p2$theme$text$size, 16)
})

test_that("st_apply_variant s'arrete si l'objet n'est pas un ggplot", {
  expect_error(st_apply_variant(list(), "ecran"))
})

# Tests : st_theme_preview ------------------------------------------------------

test_that("st_theme_preview retourne trois graphiques ggplot factices par les trois declinaisons", {
  plots <- st_theme_preview(n_groups = 3)
  expect_named(plots, c("boite", "barres", "nuage_points"))
  for (p in plots) expect_s3_class(p, "ggplot")

  construit <- ggplot2::ggplot_build(plots$boite)
  expect_equal(length(unique(construit$layout$layout$PANEL)), 12)
})

test_that("st_theme_preview refuse plus de 6 groupes", {
  expect_error(st_theme_preview(n_groups = 8))
})

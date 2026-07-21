# =============================================================================
# Profilage des variables : brique fondatrice du dictionnaire de donnees.
# L'inference produite ici est une PROPOSITION. Elle ne modifie jamais les
# donnees en entree ; l'operateur valide (ou corrige) le resultat dans
# config.yml (section dictionnaire).
# =============================================================================

.MISSING_CODES <- c("", " ", "NA", "N/A", "NR", "ND", "NC", "-", "--", ".", "?", "999", "9999", "-99")
.IDENTIFIER_TOKENS <- c("id", "code", "num", "matricule", "dossier", "ipp")
.DATE_FORMATS <- c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y")

#' Profiler les variables d'un jeu de donnees
#'
#' Calcule, pour chaque variable, des statistiques descriptives et propose
#' une nature (`inferred_nature`). Cette proposition alimente le
#' dictionnaire de `config.yml`, que l'operateur doit valider : le
#' profilage ne modifie jamais `data`.
#'
#' @param data Un data.frame (typiquement le resultat de
#'   [st_read_source()]).
#'
#' @return Un data.frame, une ligne par variable de `data`, avec les
#'   colonnes `name`, `inferred_nature`, `n_total`, `n_missing`,
#'   `pct_missing`, `n_distinct`, `n_unique_ratio`, `sample_values`,
#'   `min`, `max`, `median`, `mean`, `sd`, `top_levels`.
#' @export
st_profile <- function(data) {
  checkmate::assert_data_frame(data, min.cols = 1)

  profils <- lapply(names(data), function(nom) .profile_variable(nom, data[[nom]]))

  resultat <- data.frame(
    name = vapply(profils, function(p) p$name, character(1)),
    inferred_nature = vapply(profils, function(p) p$inferred_nature, character(1)),
    n_total = vapply(profils, function(p) p$n_total, integer(1)),
    n_missing = vapply(profils, function(p) p$n_missing, integer(1)),
    pct_missing = vapply(profils, function(p) p$pct_missing, numeric(1)),
    n_distinct = vapply(profils, function(p) p$n_distinct, integer(1)),
    n_unique_ratio = vapply(profils, function(p) p$n_unique_ratio, numeric(1)),
    min = vapply(profils, function(p) p$min, numeric(1)),
    max = vapply(profils, function(p) p$max, numeric(1)),
    median = vapply(profils, function(p) p$median, numeric(1)),
    mean = vapply(profils, function(p) p$mean, numeric(1)),
    sd = vapply(profils, function(p) p$sd, numeric(1)),
    top_levels = vapply(profils, function(p) p$top_levels, character(1)),
    stringsAsFactors = FALSE
  )
  resultat$sample_values <- lapply(profils, function(p) p$sample_values)

  resultat
}

# --- Fonctions internes de normalisation (reutilisees par d'autres modules) -

#' Normaliser les codes de valeur manquante d'un vecteur
#'
#' Reconnait, apres suppression des espaces de bord et sans tenir compte de
#' la casse, les codes suivants comme valeurs manquantes : "", " ", "NA",
#' "N/A", "NR", "ND", "NC", "-", "--", ".", "?", "999", "9999", "-99". Ne
#' modifie pas le vecteur d'entree : retourne une copie normalisee.
#'
#' @param x Un vecteur (converti en caractere).
#' @return Une liste avec `x` (le vecteur normalise, codes remplaces par
#'   `NA`) et `codes` (les codes de manquant effectivement rencontres).
#' @keywords internal
normalize_missing <- function(x) {
  x_chr <- as.character(x)
  x_trim <- trimws(x_chr)

  est_code <- !is.na(x_chr) & toupper(x_trim) %in% toupper(.MISSING_CODES)
  est_manquant <- is.na(x_chr) | est_code

  codes_rencontres <- sort(unique(x_trim[est_code]))

  x_normalise <- x_chr
  x_normalise[est_manquant] <- NA_character_

  list(x = x_normalise, codes = codes_rencontres)
}

#' Regrouper les modalites candidates a la fusion
#'
#' Compare les modalites non manquantes d'un vecteur apres suppression des
#' espaces de bord, mise en minuscules et suppression des accents, et
#' retourne les groupes de modalites qui deviennent identiques apres cette
#' normalisation (candidates a une fusion manuelle par l'operateur).
#'
#' @param x Un vecteur (converti en caractere).
#' @return Une liste de vecteurs de caracteres ; chaque element est un
#'   groupe (taille >= 2) de modalites distinctes qui se normalisent vers
#'   la meme forme.
#' @keywords internal
normalize_levels <- function(x) {
  valeurs <- unique(stats::na.omit(as.character(x)))
  if (length(valeurs) < 2) {
    return(list())
  }

  cles <- vapply(valeurs, .normalize_level_key, character(1))
  groupes <- split(valeurs, cles)
  groupes <- groupes[lengths(groupes) > 1]
  unname(groupes)
}

#' Parser des dates avec plusieurs formats candidats
#'
#' Teste successivement les formats jj/mm/aaaa, aaaa-mm-jj, jj-mm-aaaa,
#' mm/jj/aaaa, ainsi que le numerique-serie Excel (bornes 20000 a 60000),
#' et retient le format qui maximise le taux de succes.
#'
#' @param x Un vecteur (converti en caractere).
#' @return Une liste avec `dates` (vecteur `Date`, meme longueur que `x`),
#'   `format` (le format retenu, ou `"serie_excel"`, ou `NA`) et
#'   `success_rate` (taux de succes sur les valeurs non manquantes).
#' @keywords internal
parse_dates_robust <- function(x) {
  x_chr <- as.character(x)
  non_manquant <- !is.na(x_chr) & nzchar(trimws(x_chr))
  valeurs <- trimws(x_chr[non_manquant])

  dates <- as.Date(rep(NA, length(x_chr)))
  if (length(valeurs) == 0) {
    return(list(dates = dates, format = NA_character_, success_rate = 0))
  }

  meilleur_format <- NA_character_
  meilleur_taux <- 0
  meilleures_dates <- as.Date(rep(NA, length(valeurs)))

  for (fmt in .DATE_FORMATS) {
    essai <- as.Date(valeurs, format = fmt)
    taux <- mean(!is.na(essai))
    if (taux > meilleur_taux) {
      meilleur_taux <- taux
      meilleur_format <- fmt
      meilleures_dates <- essai
    }
  }

  serie_excel <- suppressWarnings(as.numeric(valeurs))
  valide_serie <- !is.na(serie_excel) & serie_excel >= 20000 & serie_excel <= 60000
  taux_serie <- mean(valide_serie)
  if (taux_serie > meilleur_taux) {
    meilleur_taux <- taux_serie
    meilleur_format <- "serie_excel"
    dates_serie <- as.Date(ifelse(valide_serie, serie_excel, NA), origin = "1899-12-30")
    meilleures_dates <- dates_serie
  }

  dates[non_manquant] <- meilleures_dates
  list(dates = dates, format = meilleur_format, success_rate = meilleur_taux)
}

# --- Helpers internes d'implementation (non decrits dans la specification) -

.normalize_level_key <- function(valeur) {
  valeur <- trimws(valeur)
  valeur <- tolower(valeur)
  .remove_accents(valeur)
}

.remove_accents <- function(valeur) {
  accentues <- paste0(
    "àáâãäåçèéêëìíîï",
    "ñòóôõöùúûüý"
  )
  sans_accent <- "aaaaaaceeeeiiiinooooouuuuy"
  chartr(accentues, sans_accent, valeur)
}

.to_numeric_permissive <- function(x) {
  direct <- suppressWarnings(as.numeric(x))
  alternatif <- suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  if (sum(!is.na(alternatif)) > sum(!is.na(direct))) alternatif else direct
}

.is_identifier_name <- function(name) {
  jetons <- strsplit(tolower(name), "[^a-z0-9]+")[[1]]
  any(jetons %in% .IDENTIFIER_TOKENS)
}

.is_identifier <- function(name, valeurs, n_distinct) {
  n <- length(valeurs)
  if (n == 0) {
    return(FALSE)
  }
  if (n_distinct == n) {
    return(TRUE)
  }
  ratio <- n_distinct / n
  ratio > 0.95 && .is_identifier_name(name)
}

.infer_nature <- function(name, valeurs, n_distinct) {
  n <- length(valeurs)
  if (n == 0) {
    return("texte")
  }

  if (.is_identifier(name, valeurs, n_distinct)) {
    return("identifiant")
  }

  dates <- parse_dates_robust(valeurs)
  if (dates$success_rate > 0.9) {
    return("date")
  }

  if (n_distinct == 2) {
    return("binaire")
  }

  valeurs_num <- .to_numeric_permissive(valeurs)
  numerique_ok <- !anyNA(valeurs_num)

  if (numerique_ok && n_distinct > 15) {
    return("continue")
  }
  if (numerique_ok) {
    entiers <- all(valeurs_num == round(valeurs_num))
    if (entiers && n_distinct <= 15) {
      return("entiere")
    }
  }

  if (n_distinct <= 30) {
    return("nominale")
  }

  "texte"
}

.profile_variable <- function(name, x) {
  n_total <- length(x)
  normalise <- normalize_missing(x)
  valeurs_norm <- normalise$x
  manquant <- is.na(valeurs_norm)
  n_missing <- sum(manquant)
  pct_missing <- if (n_total > 0) n_missing / n_total else NA_real_

  non_manquantes <- valeurs_norm[!manquant]
  n_distinct <- length(unique(non_manquantes))
  n_unique_ratio <- if (length(non_manquantes) > 0) n_distinct / length(non_manquantes) else NA_real_

  nature <- .infer_nature(name, non_manquantes, n_distinct)
  echantillon <- as.character(utils::head(unique(non_manquantes), 5))

  min_val <- NA_real_
  max_val <- NA_real_
  median_val <- NA_real_
  mean_val <- NA_real_
  sd_val <- NA_real_
  top_levels <- NA_character_

  if (nature %in% c("continue", "entiere")) {
    valeurs_num <- .to_numeric_permissive(non_manquantes)
    valeurs_num <- valeurs_num[!is.na(valeurs_num)]
    if (length(valeurs_num) > 0) {
      min_val <- min(valeurs_num)
      max_val <- max(valeurs_num)
      median_val <- stats::median(valeurs_num)
      mean_val <- mean(valeurs_num)
      sd_val <- if (length(valeurs_num) > 1) stats::sd(valeurs_num) else NA_real_
    }
  }

  if (nature %in% c("nominale", "binaire", "ordinale") && length(non_manquantes) > 0) {
    effectifs <- sort(table(non_manquantes), decreasing = TRUE)
    top <- utils::head(effectifs, 5)
    top_levels <- paste(paste0(names(top), " (", as.integer(top), ")"), collapse = "; ")
  }

  list(
    name = name,
    inferred_nature = nature,
    n_total = as.integer(n_total),
    n_missing = as.integer(n_missing),
    pct_missing = pct_missing,
    n_distinct = as.integer(n_distinct),
    n_unique_ratio = n_unique_ratio,
    sample_values = echantillon,
    min = min_val,
    max = max_val,
    median = median_val,
    mean = mean_val,
    sd = sd_val,
    top_levels = top_levels
  )
}

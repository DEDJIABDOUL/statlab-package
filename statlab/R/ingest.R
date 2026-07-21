# =============================================================================
# Lecture des fichiers sources declares en config.
# Lecture seule stricte : aucune fonction de ce fichier n'ecrit dans un
# fichier source. L'inference de type est hors-perimetre (cf. R/profile.R).
# =============================================================================

.LARGE_FILE_BYTES <- 50 * 1024^2

#' Lire une source declaree en config
#'
#' Lit un fichier de donnees brutes et retourne son contenu sous forme de
#' data.frame dont toutes les colonnes sont des chaines de caracteres.
#' L'inference de type (numerique, date, etc.) n'est pas realisee ici.
#'
#' @param source_spec Liste avec les champs `id` (chr), `fichier` (chr,
#'   chemin vers le fichier, absolu ou relatif au repertoire courant),
#'   `onglet` (chr, optionnel) et `ligne_entete` (int, optionnel). Ces deux
#'   derniers champs, s'ils sont fournis, sont prioritaires sur la
#'   detection automatique.
#'
#' @return Un data.frame dont toutes les colonnes sont de type caractere,
#'   avec les attributs `source_id`, `file_path`, `file_hash`, `sheet`,
#'   `header_row` et `n_raw_rows`.
#' @export
st_read_source <- function(source_spec) {
  checkmate::assert_list(source_spec, min.len = 1)
  checkmate::assert_string(source_spec$id, min.chars = 1)
  checkmate::assert_string(source_spec$fichier, min.chars = 1)
  checkmate::assert_string(source_spec$onglet, null.ok = TRUE)
  checkmate::assert_count(source_spec$ligne_entete, positive = TRUE, null.ok = TRUE)

  path <- source_spec$fichier
  if (!file.exists(path)) {
    cli::cli_abort("Fichier source introuvable pour {.field {source_spec$id}} : {.path {path}}")
  }

  extension <- tolower(tools::file_ext(path))
  lecture <- if (extension %in% c("xlsx", "xls")) {
    .read_source_excel(source_spec, path)
  } else if (extension %in% c("csv", "tsv", "txt")) {
    .read_source_delimited(source_spec, path)
  } else {
    cli::cli_abort(c(
      "Format de fichier non pris en charge pour {.field {source_spec$id}} : '.{extension}'.",
      "i" = "Formats acceptes : csv, tsv, txt, xlsx, xls."
    ))
  }

  empreinte <- digest::digest(file = path, algo = "sha256")
  chemin_absolu <- normalizePath(path, winslash = "/", mustWork = TRUE)

  donnees <- lecture$donnees
  attr(donnees, "source_id") <- source_spec$id
  attr(donnees, "file_path") <- chemin_absolu
  attr(donnees, "file_hash") <- empreinte
  attr(donnees, "sheet") <- lecture$sheet
  attr(donnees, "header_row") <- lecture$header_row
  attr(donnees, "n_raw_rows") <- lecture$n_raw_rows

  st_log(
    "lecture_source",
    module = "ingest",
    source_id = source_spec$id,
    fichier = chemin_absolu,
    onglet = if (is.na(lecture$sheet)) "(sans objet)" else lecture$sheet,
    ligne_entete = lecture$header_row,
    n_lignes_brutes = lecture$n_raw_rows,
    n_lignes = nrow(donnees),
    n_colonnes = ncol(donnees),
    separateur_decimal = if (is.na(lecture$decimal_mark)) "(sans objet)" else lecture$decimal_mark,
    empreinte = empreinte,
    level = "info"
  )

  donnees
}

#' Lire toutes les sources d'une configuration
#'
#' Lit chaque source declaree dans `config$sources` via [st_read_source()]
#' et retourne les jeux de donnees dans une liste nommee par identifiant
#' de source.
#'
#' @param config Un objet `statlab_config` valide, tel que retourne par
#'   `st_validate_config()`.
#'
#' @return Une liste de data.frame, nommee par identifiant de source.
#' @export
st_read_all_sources <- function(config) {
  checkmate::assert_class(config, "statlab_config")
  if (!isTRUE(attr(config, "valid"))) {
    cli::cli_abort("La configuration doit etre validee (st_validate_config()) avant la lecture des sources.")
  }

  project_dir <- attr(config, "project_dir")
  identifiants <- vapply(config$sources, function(entree) entree$id, character(1))

  resultats <- lapply(config$sources, function(entree) {
    spec <- list(
      id = entree$id,
      fichier = file.path(project_dir, entree$fichier),
      onglet = entree$onglet,
      ligne_entete = entree$ligne_entete
    )
    st_read_source(spec)
  })

  names(resultats) <- identifiants
  resultats
}

# --- Detection automatique (fonctions internes, reutilisables) -------------

#' Detecter le delimiteur d'un fichier texte
#'
#' @param path Chemin du fichier.
#' @return Le caractere delimiteur le plus plausible parmi virgule,
#'   point-virgule, tabulation et barre verticale.
#' @keywords internal
detect_delimiter <- function(path) {
  candidats <- c(",", ";", "\t", "|")
  lignes <- readLines(path, n = 20, warn = FALSE)
  lignes <- lignes[nzchar(lignes)]
  if (length(lignes) == 0) {
    return(",")
  }

  scores <- vapply(candidats, function(delim) {
    comptes <- vapply(lignes, .compter_occurrences, integer(1), motif = delim)
    comptes <- comptes[comptes > 0]
    if (length(comptes) == 0) {
      return(0)
    }
    table_comptes <- table(comptes)
    mode_compte <- as.integer(names(table_comptes)[which.max(table_comptes)])
    proportion_stable <- mean(comptes == mode_compte) * (length(comptes) / length(lignes))
    mode_compte * proportion_stable
  }, numeric(1))

  if (max(scores) == 0) {
    return(",")
  }
  candidats[which.max(scores)]
}

#' Detecter l'encodage probable d'un fichier texte
#'
#' @param path Chemin du fichier.
#' @return `"UTF-8"`, `"Latin-1"` ou `"Windows-1252"`.
#' @keywords internal
detect_encoding <- function(path) {
  taille <- min(file.size(path), 200000)
  if (taille == 0) {
    return("UTF-8")
  }
  octets <- readBin(path, what = "raw", n = taille)

  if (length(octets) >= 3 && identical(octets[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    return("UTF-8")
  }

  chaine <- rawToChar(octets[octets != as.raw(0)])
  if (isTRUE(validUTF8(chaine))) {
    return("UTF-8")
  }

  valeurs <- as.integer(octets)
  if (any(valeurs >= 128 & valeurs <= 159)) {
    return("Windows-1252")
  }
  if (any(valeurs >= 160)) {
    return("Latin-1")
  }
  "UTF-8"
}

#' Detecter le separateur decimal d'un fichier delimite
#'
#' @param path Chemin du fichier.
#' @param delim Delimiteur de colonnes, tel que retourne par
#'   [detect_delimiter()].
#' @return `"."` ou `","`.
#' @keywords internal
detect_decimal_mark <- function(path, delim) {
  lignes <- readLines(path, n = 50, warn = FALSE)
  lignes <- lignes[nzchar(lignes)]
  if (length(lignes) == 0) {
    return(".")
  }

  champs <- trimws(unlist(strsplit(lignes, delim, fixed = TRUE)))

  n_point <- sum(grepl("^-?[0-9]+\\.[0-9]+$", champs))
  n_virgule <- if (delim != ",") sum(grepl("^-?[0-9]+,[0-9]+$", champs)) else 0L

  if (n_point == 0 && n_virgule == 0) {
    return(".")
  }
  if (n_virgule > n_point) "," else "."
}

#' Detecter la ligne d'en-tete d'un tableau brut
#'
#' Heuristique : une ligne d'en-tete plausible a des cellules
#' majoritairement non vides, non numeriques et distinctes entre elles, et
#' est suivie de lignes dont le taux de remplissage est homogene.
#'
#' @param raw data.frame brut (toutes colonnes en caractere), sans en-tete.
#' @param max_scan Nombre maximal de lignes examinees.
#' @return L'indice (1-based) de la ligne d'en-tete, ou `NA_integer_` si
#'   aucune ligne plausible n'a ete trouvee.
#' @keywords internal
detect_header_row <- function(raw, max_scan = 15) {
  n <- nrow(raw)
  if (n == 0) {
    return(NA_integer_)
  }
  limite <- min(max_scan, n)

  for (i in seq_len(limite)) {
    cellules <- as.character(raw[i, ])
    non_vides <- !is.na(cellules) & nzchar(trimws(cellules))
    ratio_non_vide <- mean(non_vides)
    if (ratio_non_vide < 0.7) {
      next
    }

    valeurs <- cellules[non_vides]
    ratio_non_numerique <- mean(is.na(suppressWarnings(as.numeric(valeurs))))
    ratio_distinct <- length(unique(valeurs)) / length(valeurs)
    if (ratio_non_numerique < 0.7 || ratio_distinct < 0.8) {
      next
    }

    lignes_suivantes <- seq(i + 1, min(i + 5, n))
    if (length(lignes_suivantes) < 1) {
      next
    }
    remplissage <- vapply(lignes_suivantes, function(j) {
      c_j <- as.character(raw[j, ])
      mean(!is.na(c_j) & nzchar(trimws(c_j)))
    }, numeric(1))
    homogene <- length(remplissage) < 2 || stats::sd(remplissage) < 0.25

    if (homogene) {
      return(as.integer(i))
    }
  }

  NA_integer_
}

#' Choisir l'onglet d'un classeur Excel
#'
#' A n'appeler que lorsqu'aucun onglet n'a ete declare en config. S'il n'y a
#' qu'un seul onglet, il est retenu automatiquement. S'il y en a plusieurs,
#' la fonction s'arrete en listant les onglets disponibles et leurs
#' dimensions : le moteur ne devine jamais un onglet ambigu.
#'
#' @param path Chemin du classeur Excel.
#' @return Le nom de l'onglet retenu.
#' @keywords internal
pick_sheet <- function(path) {
  onglets <- readxl::excel_sheets(path)
  if (length(onglets) == 1) {
    return(onglets)
  }

  dimensions <- vapply(onglets, function(feuille) {
    donnees <- readxl::read_excel(path, sheet = feuille, col_names = FALSE, col_types = "text", .name_repair = "minimal")
    sprintf("%d lignes x %d colonnes", nrow(donnees), ncol(donnees))
  }, character(1))

  cli::cli_abort(c(
    "Plusieurs onglets sont disponibles et aucun n'est declare en config (champ 'onglet').",
    stats::setNames(paste0(onglets, " : ", dimensions), rep("*", length(onglets)))
  ))
}

# --- Helpers internes d'implementation (non decrits dans la specification) -

.compter_occurrences <- function(ligne, motif) {
  correspondances <- gregexpr(motif, ligne, fixed = TRUE)[[1]]
  if (length(correspondances) == 1 && correspondances[1] == -1) {
    return(0L)
  }
  length(correspondances)
}

.lines_to_dataframe <- function(lignes, delim) {
  if (length(lignes) == 0) {
    return(data.frame(V1 = character(0), stringsAsFactors = FALSE))
  }

  champs <- strsplit(lignes, delim, fixed = TRUE)
  n_colonnes <- max(1L, vapply(champs, length, integer(1)))
  champs <- lapply(champs, function(v) {
    if (length(v) < n_colonnes) {
      v <- c(v, rep(NA_character_, n_colonnes - length(v)))
    }
    v[seq_len(n_colonnes)]
  })

  raw <- as.data.frame(do.call(rbind, champs), stringsAsFactors = FALSE)
  colnames(raw) <- paste0("V", seq_len(n_colonnes))
  rownames(raw) <- NULL
  raw
}

.encoding_to_iconv <- function(encodage) {
  switch(encodage,
    "UTF-8" = "UTF-8",
    "Latin-1" = "ISO-8859-1",
    "Windows-1252" = "windows-1252",
    "UTF-8"
  )
}

.determine_header_row <- function(source_spec, raw, n_raw_rows) {
  if (!is.null(source_spec$ligne_entete)) {
    header_row <- as.integer(source_spec$ligne_entete)
    if (header_row < 1 || header_row > n_raw_rows) {
      cli::cli_abort(
        "ligne_entete ({header_row}) hors bornes pour {.field {source_spec$id}} (1 a {n_raw_rows})."
      )
    }
    return(header_row)
  }

  header_row <- detect_header_row(raw)
  if (is.na(header_row)) {
    cli::cli_abort(c(
      "Impossible de detecter automatiquement la ligne d'en-tete pour {.field {source_spec$id}}.",
      "i" = "Declarer 'ligne_entete' dans config.yml pour cette source."
    ))
  }
  header_row
}

.split_header_and_data <- function(raw, header_row) {
  n_raw_rows <- nrow(raw)
  entetes <- as.character(raw[header_row, ])
  vides <- is.na(entetes) | !nzchar(trimws(entetes))
  entetes[vides] <- paste0("colonne_", seq_along(entetes))[vides]

  donnees <- if (header_row < n_raw_rows) {
    raw[(header_row + 1):n_raw_rows, , drop = FALSE]
  } else {
    raw[0, , drop = FALSE]
  }
  colnames(donnees) <- entetes
  rownames(donnees) <- NULL
  donnees[] <- lapply(donnees, as.character)
  donnees
}

.read_source_excel <- function(source_spec, path) {
  onglets_disponibles <- readxl::excel_sheets(path)
  sheet <- source_spec$onglet
  if (!is.null(sheet)) {
    if (!sheet %in% onglets_disponibles) {
      cli::cli_abort(c(
        "Onglet declare introuvable pour {.field {source_spec$id}} : '{sheet}'",
        "i" = "Onglets disponibles : {paste(onglets_disponibles, collapse = ', ')}"
      ))
    }
  } else {
    sheet <- pick_sheet(path)
  }

  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  n_raw_rows <- nrow(raw)

  header_row <- .determine_header_row(source_spec, raw, n_raw_rows)
  donnees <- .split_header_and_data(raw, header_row)

  list(donnees = donnees, sheet = sheet, header_row = header_row, n_raw_rows = n_raw_rows, decimal_mark = NA_character_)
}

.read_source_delimited <- function(source_spec, path) {
  delim <- detect_delimiter(path)
  encodage <- detect_encoding(path)
  decimal_mark <- detect_decimal_mark(path, delim)

  gros_fichier <- file.size(path) > .LARGE_FILE_BYTES

  if (gros_fichier) {
    encodage_fread <- if (encodage == "UTF-8") "UTF-8" else "Latin-1"
    if (identical(encodage, "Windows-1252")) {
      cli::cli_alert_warning(
        "Fichier volumineux encode en Windows-1252 : approxime en Latin-1 pour la lecture rapide."
      )
    }
    raw <- data.table::fread(
      path,
      sep = delim, header = FALSE, colClasses = "character",
      encoding = encodage_fread, data.table = FALSE, fill = TRUE,
      na.strings = character(0), strip.white = FALSE, blank.lines.skip = FALSE
    )
    raw[] <- lapply(raw, as.character)
  } else {
    # Lecture ligne a ligne puis decoupage manuel : la position de la ligne
    # d'en-tete n'est pas encore connue a ce stade, et readr::read_delim()
    # deduit le nombre de colonnes des premieres lignes, ce qui echoue si
    # des lignes de titre precedent l'en-tete reel (nombre de champs
    # different). Chaque ligne physique est ainsi preservee independamment.
    lignes <- readr::read_lines(path, locale = readr::locale(encoding = .encoding_to_iconv(encodage)))
    raw <- .lines_to_dataframe(lignes, delim)
  }

  n_raw_rows <- nrow(raw)
  header_row <- .determine_header_row(source_spec, raw, n_raw_rows)
  donnees <- .split_header_and_data(raw, header_row)

  list(
    donnees = donnees, sheet = NA_character_, header_row = header_row,
    n_raw_rows = n_raw_rows, decimal_mark = decimal_mark
  )
}

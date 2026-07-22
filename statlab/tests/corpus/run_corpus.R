# =============================================================================
# Campagne de tests sur un corpus de fichiers reels.
#
# Ce script n'est PAS un test testthat et ne fait pas partie du package
# installe : c'est un outil de validation manuelle, a executer par
# l'operateur sur un corpus de fichiers reels (anonymises) representatifs
# des donnees rencontrees en pratique. C'est ce qui permet de repondre a la
# question "le MVP est-il reellement livrable ?", au-dela des jeux de test
# synthetiques du package.
#
# Usage :
#   source("tests/corpus/run_corpus.R")
#   run_corpus_tests()
#
# Structure attendue :
#   tests/corpus/corpus/           - fichiers bruts a tester (.csv/.tsv/.txt/
#                                     .xlsx/.xls), a deposer manuellement.
#   tests/corpus/attendu.csv       - annotation manuelle (voir README.md du
#                                     meme repertoire pour le format exact).
#   tests/corpus/faux_positifs.csv - annotation manuelle, optionnelle,
#                                     produite apres une premiere execution
#                                     (voir README.md).
#
# Pour chaque fichier, la chaine executee est volontairement minimale
# (lecture -> profilage -> anomalies -> tableau 1 sur un dictionnaire
# invente a partir du profilage) : il ne s'agit pas de rejouer une analyse
# reelle (qui necessite un config.yml ecrit par un operateur), mais de
# mesurer la ROBUSTESSE de la chaine face a des fichiers reels varies.
# =============================================================================

#' Executer la campagne de tests sur le corpus de fichiers reels
#'
#' @param corpus_dir Repertoire (chr) contenant les fichiers bruts a
#'   tester. Par defaut, `"corpus/"` (relatif au repertoire courant, donc
#'   `tests/corpus/corpus/` si ce script est source depuis
#'   `tests/corpus/`).
#' @param report Chemin (chr) du rapport HTML de synthese a produire.
#'
#' @return Invisiblement, un data.frame (une ligne par fichier teste).
run_corpus_tests <- function(corpus_dir = "corpus/", report = "corpus_report.html") {
  if (!requireNamespace("statlab", quietly = TRUE)) {
    stop("Le package statlab doit etre installe (ou charge via pkgload::load_all()) avant d'executer run_corpus_tests().")
  }
  if (!dir.exists(corpus_dir)) {
    stop(sprintf("Repertoire de corpus introuvable : '%s'.", corpus_dir))
  }

  # Les fonctions statlab appelees ci-dessous (st_read_source(), etc.)
  # journalisent systematiquement : un journal jetable, dans un repertoire
  # temporaire, suffit ici (la campagne n'est pas un "projet" statlab).
  # Le mode silencieux evite un message par fichier x etape sur un corpus
  # potentiellement volumineux.
  ancienne_option_quiet <- getOption("statlab.quiet", FALSE)
  options(statlab.quiet = TRUE)
  on.exit(options(statlab.quiet = ancienne_option_quiet), add = TRUE)
  repertoire_journal <- tempfile("statlab_corpus_")
  dir.create(repertoire_journal)
  statlab::st_log_init(repertoire_journal)

  fichiers <- .corpus_list_files(corpus_dir)
  if (length(fichiers) == 0) {
    message(sprintf(
      "Aucun fichier a tester dans '%s' (extensions attendues : csv, tsv, txt, xlsx, xls). Rien a faire.",
      corpus_dir
    ))
    return(invisible(data.frame()))
  }

  annotation_dir <- dirname(sub("/+$", "", corpus_dir))
  attendu <- .corpus_read_attendu(file.path(annotation_dir, "attendu.csv"))
  faux_positifs <- .corpus_read_faux_positifs(file.path(annotation_dir, "faux_positifs.csv"))

  resultats <- lapply(fichiers, .corpus_test_one_file, attendu = attendu, faux_positifs = faux_positifs)
  resume <- do.call(rbind, resultats)
  rownames(resume) <- NULL

  .corpus_write_report(resume, report)

  n_echecs <- sum(!resume$lecture_reussie)
  message(sprintf(
    "Campagne terminee : %d fichier(s) teste(s), %d echec(s) de lecture. Rapport : '%s'.",
    nrow(resume), n_echecs, report
  ))

  invisible(resume)
}

# --- Decouverte des fichiers et annotations -----------------------------------

.corpus_list_files <- function(corpus_dir) {
  candidats <- list.files(corpus_dir, pattern = "\\.(csv|tsv|txt|xlsx|xls)$", ignore.case = TRUE, full.names = TRUE)
  exclus <- tolower(basename(candidats)) %in% c("attendu.csv", "faux_positifs.csv")
  candidats[!exclus]
}

.corpus_read_attendu <- function(path) {
  vide <- data.frame(
    fichier = character(0), variable = character(0),
    nature_attendue = character(0), ligne_entete_attendue = integer(0),
    stringsAsFactors = FALSE
  )
  if (!file.exists(path)) {
    message(sprintf("Aucun fichier d'annotation '%s' : les taux de concordance ne seront pas calcules.", path))
    return(vide)
  }
  lu <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE, colClasses = "character")
  if (nrow(lu) == 0) {
    return(vide)
  }
  data.frame(
    fichier = lu$fichier,
    variable = ifelse(is.na(lu$variable) | !nzchar(lu$variable), NA_character_, lu$variable),
    nature_attendue = ifelse(is.na(lu$nature_attendue) | !nzchar(lu$nature_attendue), NA_character_, lu$nature_attendue),
    ligne_entete_attendue = suppressWarnings(as.integer(lu$ligne_entete_attendue)),
    stringsAsFactors = FALSE
  )
}

.corpus_read_faux_positifs <- function(path) {
  vide <- data.frame(fichier = character(0), check_id = character(0), variable = character(0), faux_positif = logical(0), stringsAsFactors = FALSE)
  if (!file.exists(path)) {
    return(vide)
  }
  lu <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE, colClasses = "character")
  if (nrow(lu) == 0) {
    return(vide)
  }
  data.frame(
    fichier = lu$fichier,
    check_id = lu$check_id,
    variable = ifelse(is.na(lu$variable) | !nzchar(lu$variable), NA_character_, lu$variable),
    faux_positif = toupper(trimws(lu$faux_positif)) %in% c("TRUE", "VRAI", "1", "OUI"),
    stringsAsFactors = FALSE
  )
}

# --- Test d'un fichier ---------------------------------------------------------

.corpus_test_one_file <- function(chemin, attendu, faux_positifs) {
  nom <- basename(chemin)
  ligne <- .corpus_empty_result(nom)

  attendu_fichier <- attendu[!is.na(attendu$fichier) & attendu$fichier == nom, , drop = FALSE]
  ligne_entete_attendue <- if (nrow(attendu_fichier) > 0) attendu_fichier$ligne_entete_attendue[1] else NA_integer_
  attendu_variables <- attendu_fichier[!is.na(attendu_fichier$variable), , drop = FALSE]

  # --- Lecture -----------------------------------------------------------
  t_lecture <- system.time({
    donnees <- tryCatch(
      statlab::st_read_source(list(id = tools::file_path_sans_ext(nom), fichier = chemin)),
      error = function(e) e
    )
  })["elapsed"]
  ligne$temps_lecture_s <- as.numeric(t_lecture)

  if (inherits(donnees, "error")) {
    ligne$lecture_reussie <- FALSE
    ligne$erreur_lecture <- conditionMessage(donnees)
    return(ligne)
  }
  ligne$lecture_reussie <- TRUE
  ligne$ligne_entete_detectee <- attr(donnees, "header_row")
  if (!is.na(ligne_entete_attendue)) {
    ligne$ligne_entete_correcte <- isTRUE(ligne$ligne_entete_detectee == ligne_entete_attendue)
  }
  ligne$n_lignes <- nrow(donnees)
  ligne$n_variables <- ncol(donnees)

  # --- Profilage -----------------------------------------------------------
  t_profil <- system.time({
    profil <- tryCatch(statlab::st_profile(donnees), error = function(e) e)
  })["elapsed"]
  ligne$temps_profilage_s <- as.numeric(t_profil)

  if (inherits(profil, "error")) {
    ligne$erreur_profilage <- conditionMessage(profil)
    return(ligne)
  }

  if (nrow(attendu_variables) > 0) {
    concordance <- merge(
      attendu_variables[, c("variable", "nature_attendue")],
      profil[, c("name", "inferred_nature")],
      by.x = "variable", by.y = "name", all.x = TRUE
    )
    ligne$n_natures_annotees <- nrow(concordance)
    ligne$n_natures_concordantes <- sum(concordance$nature_attendue == concordance$inferred_nature, na.rm = TRUE)
    ligne$taux_concordance_natures <- ligne$n_natures_concordantes / ligne$n_natures_annotees
  }

  # --- Anomalies -----------------------------------------------------------
  t_anomalies <- system.time({
    anomalies <- tryCatch(statlab::st_detect_anomalies(donnees, profil), error = function(e) e)
  })["elapsed"]
  ligne$temps_anomalies_s <- as.numeric(t_anomalies)

  if (inherits(anomalies, "error")) {
    ligne$erreur_anomalies <- conditionMessage(anomalies)
  } else {
    ligne$n_anomalies <- nrow(anomalies)
    annotees <- faux_positifs[!is.na(faux_positifs$fichier) & faux_positifs$fichier == nom, , drop = FALSE]
    if (nrow(annotees) > 0 && nrow(anomalies) > 0) {
      cle_detectee <- paste(anomalies$check_id, ifelse(is.na(anomalies$variable), "", anomalies$variable))
      cle_annotee <- paste(annotees$check_id, ifelse(is.na(annotees$variable), "", annotees$variable))
      correspond <- cle_detectee %in% cle_annotee
      ligne$n_anomalies_annotees <- sum(correspond)
      ligne$n_faux_positifs <- sum(annotees$faux_positif[match(cle_detectee[correspond], cle_annotee)])
      if (ligne$n_anomalies_annotees > 0) {
        ligne$taux_faux_positifs <- ligne$n_faux_positifs / ligne$n_anomalies_annotees
      }
    }
  }

  # --- Tableau 1 (dictionnaire invente a partir du profilage) --------------
  t_tableau1 <- system.time({
    tableau1 <- tryCatch(.corpus_try_table1(donnees, profil), error = function(e) e)
  })["elapsed"]
  ligne$temps_tableau1_s <- as.numeric(t_tableau1)

  if (inherits(tableau1, "error")) {
    ligne$tableau1_produit <- FALSE
    ligne$erreur_tableau1 <- conditionMessage(tableau1)
  } else if (is.null(tableau1)) {
    ligne$tableau1_produit <- NA
    ligne$erreur_tableau1 <- "Aucune variable non-identifiant/texte a resumer."
  } else {
    ligne$tableau1_produit <- TRUE
  }

  ligne
}

.corpus_empty_result <- function(nom) {
  data.frame(
    fichier = nom,
    lecture_reussie = NA, erreur_lecture = NA_character_,
    ligne_entete_detectee = NA_integer_, ligne_entete_correcte = NA,
    n_lignes = NA_integer_, n_variables = NA_integer_,
    erreur_profilage = NA_character_,
    n_natures_annotees = 0L, n_natures_concordantes = 0L, taux_concordance_natures = NA_real_,
    n_anomalies = NA_integer_, erreur_anomalies = NA_character_,
    n_anomalies_annotees = 0L, n_faux_positifs = 0L, taux_faux_positifs = NA_real_,
    tableau1_produit = NA, erreur_tableau1 = NA_character_,
    temps_lecture_s = NA_real_, temps_profilage_s = NA_real_,
    temps_anomalies_s = NA_real_, temps_tableau1_s = NA_real_,
    stringsAsFactors = FALSE
  )
}

.corpus_try_table1 <- function(donnees, profil) {
  utiles <- profil[!profil$inferred_nature %in% c("identifiant", "texte"), , drop = FALSE]
  if (nrow(utiles) == 0) {
    return(NULL)
  }

  dictionnaire <- stats::setNames(
    lapply(seq_len(nrow(utiles)), function(i) list(nature = utiles$inferred_nature[i], libelle = utiles$name[i])),
    utiles$name
  )
  config <- structure(
    list(dictionnaire = dictionnaire, analyse = list(tableau_1 = list(variables = utiles$name))),
    class = "statlab_config", valid = TRUE
  )
  statlab::st_table1(donnees[, utiles$name, drop = FALSE], config)
}

# --- Rapport HTML ----------------------------------------------------------

.corpus_write_report <- function(resume, report) {
  n <- nrow(resume)
  taux <- function(x) if (n == 0) NA_real_ else sum(x, na.rm = TRUE) / n

  synthese <- data.frame(
    Etape = c("Lecture", "Ligne d'en-tete correcte (annotes)", "Tableau 1 produit"),
    "Taux de reussite" = c(
      sprintf("%.0f %% (%d/%d)", 100 * taux(resume$lecture_reussie), sum(resume$lecture_reussie, na.rm = TRUE), n),
      .corpus_format_rate(resume$ligne_entete_correcte),
      .corpus_format_rate(resume$tableau1_produit)
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  est_echec <- .corpus_est_echec(resume)
  echecs <- resume[est_echec, , drop = FALSE]

  lignes_html <- c(
    "<!doctype html><html lang=\"fr\"><head><meta charset=\"utf-8\">",
    "<title>Campagne de tests corpus statlab</title>",
    "<style>",
    "body{font-family:sans-serif;margin:2rem;color:#222}",
    "table{border-collapse:collapse;width:100%;margin-bottom:2rem}",
    "th,td{border:1px solid #ccc;padding:6px 10px;text-align:left;font-size:14px}",
    "th{background:#f2f2f2}",
    "tr.echec{background:#fdecea}",
    "tr.ok{background:#eafaf1}",
    ".titre{margin-top:2rem}",
    "</style></head><body>",
    sprintf("<h1>Campagne de tests corpus statlab</h1><p>%s &mdash; %d fichier(s) teste(s)</p>", format(Sys.time(), "%Y-%m-%d %H:%M"), n),
    "<h2>Synthese</h2>",
    .corpus_df_to_html(synthese, row_class = NULL),
    "<h2 class=\"titre\">Detail par fichier</h2>",
    .corpus_df_to_html(
      resume[, c(
        "fichier", "lecture_reussie", "ligne_entete_detectee", "ligne_entete_correcte",
        "taux_concordance_natures", "n_anomalies", "taux_faux_positifs", "tableau1_produit"
      ), drop = FALSE],
      row_class = ifelse(est_echec, "echec", "ok")
    ),
    "<h2 class=\"titre\">Fichiers en echec</h2>"
  )

  if (nrow(echecs) == 0) {
    lignes_html <- c(lignes_html, "<p>Aucun.</p>")
  } else {
    items <- vapply(seq_len(nrow(echecs)), function(i) {
      cause <- if (!isTRUE(echecs$lecture_reussie[i])) {
        sprintf("lecture : %s", echecs$erreur_lecture[i])
      } else {
        sprintf("tableau 1 : %s", echecs$erreur_tableau1[i])
      }
      cause <- gsub("[\r\n]+", " ", cause)
      sprintf("<li><strong>%s</strong> &mdash; %s</li>", echecs$fichier[i], cause)
    }, character(1))
    lignes_html <- c(lignes_html, "<ul>", items, "</ul>")
  }

  lignes_html <- c(lignes_html, "</body></html>")
  writeLines(lignes_html, report, useBytes = TRUE)
}

.corpus_est_echec <- function(resume) {
  # isTRUE()/isFALSE() ne se vectorisent pas (ils retournent toujours un
  # scalaire, meme applique a un vecteur) : la comparaison logique
  # elementwise doit passer par des tests explicitement NA-surs.
  lecture_en_echec <- is.na(resume$lecture_reussie) | !resume$lecture_reussie
  tableau1_en_echec <- !is.na(resume$tableau1_produit) & !resume$tableau1_produit
  lecture_en_echec | tableau1_en_echec
}

.corpus_format_rate <- function(x) {
  annotes <- x[!is.na(x)]
  if (length(annotes) == 0) {
    return("non annote")
  }
  sprintf("%.0f %% (%d/%d)", 100 * mean(annotes), sum(annotes), length(annotes))
}

.corpus_df_to_html <- function(df, row_class = NULL) {
  entetes <- sprintf("<th>%s</th>", names(df))
  formatter_cellule <- function(x) {
    if (is.logical(x)) ifelse(is.na(x), "&mdash;", ifelse(x, "oui", "non"))
    else if (is.numeric(x)) ifelse(is.na(x), "&mdash;", format(round(x, 3)))
    else ifelse(is.na(x), "&mdash;", as.character(x))
  }
  corps <- vapply(seq_len(nrow(df)), function(i) {
    cellules <- vapply(names(df), function(col) sprintf("<td>%s</td>", formatter_cellule(df[[col]][i])), character(1))
    classe <- if (!is.null(row_class)) sprintf(" class=\"%s\"", row_class[i]) else ""
    sprintf("<tr%s>%s</tr>", classe, paste(cellules, collapse = ""))
  }, character(1))
  paste0(
    "<table><thead><tr>", paste(entetes, collapse = ""), "</tr></thead><tbody>",
    paste(corps, collapse = ""), "</tbody></table>"
  )
}

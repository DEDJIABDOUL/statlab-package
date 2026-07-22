# =============================================================================
# Orchestration du pipeline avec {targets} : seules les etapes en aval d'une
# modification sont reexecutees. C'est ce qui permet de tenir l'exigence de
# regeneration en moins de 5 minutes sur un projet deja construit une
# premiere fois.
#
# Principe d'invalidation fine : `config.yml` n'est jamais utilise comme un
# bloc monolithique dans le plan. Chaque section (sources, dictionnaire,
# preparation, analyse, rendu, reconciliation) devient sa PROPRE cible
# {targets}. Une cible en aval ne depend que des sections qu'elle utilise
# reellement (via des "mini-configurations" construites a la volee,
# cf. .pipeline_mini_config()) : modifier `analyse.comparaisons` ne touche
# donc jamais les cibles de lecture/profilage/audit, qui ne dependent que de
# `sources` et `dictionnaire`. Les fichiers sources et `config.yml`
# eux-memes sont suivis par empreinte (format = "file" de {targets}) : toute
# modification d'un fichier source invalide naturellement toute la chaine
# qui en depend (soit, en pratique, l'integralite du pipeline).
#
# Les etapes finales (audit, script, attestation, rapport) delegent a des
# fonctions deja existantes et deja validees (st_audit(), st_export_script(),
# st_attestation(), st_report()), qui relisent et retraitent elles-memes
# leurs entrees de bout en bout : elles ne sont donc pas refondues ici pour
# accepter des resultats intermediaires injectes. Le gain de {targets} porte
# integralement sur les etapes de traitement des donnees (lecture,
# profilage, anomalies, reconciliation, preparation, tableau 1,
# comparaisons, graphiques), qui ne sont jamais recalculees si rien de ce
# dont elles dependent n'a change ; les etapes finales, elles, sont
# simplement sautees dans leur ensemble si toutes leurs dependances (donc
# `config` et les fichiers sources) sont deja a jour.
# =============================================================================

#' Construire et executer le pipeline d'analyse d'un projet
#'
#' Copie le gabarit `inst/templates/_targets.R` a la racine du projet (aux
#' cotes de `config.yml`), puis execute le plan avec [targets::tar_make()].
#' Un second appel, sans modification, ne reexecute aucune etape ; un appel
#' apres modification de `config.yml` ne reexecute que les etapes en aval
#' de la section modifiee ; un appel apres modification d'un fichier source
#' reexecute l'integralite du pipeline.
#'
#' @param config_path Chemin (chr) vers le fichier `config.yml`.
#'
#' @return Invisiblement, un data.frame decrivant chaque cible reexecutee
#'   lors de cet appel (colonnes `name`, `seconds`), ou un data.frame vide
#'   si aucune cible n'a ete recalculee.
#' @export
st_pipeline <- function(config_path) {
  checkmate::assert_string(config_path, min.chars = 1)
  if (!file.exists(config_path)) {
    cli::cli_abort("Fichier de configuration introuvable : {.path {config_path}}")
  }

  project_dir <- .pipeline_prepare(config_path)
  ancien_wd <- getwd()
  setwd(project_dir)
  on.exit(setwd(ancien_wd), add = TRUE)

  # st_log_init() doit s'executer a CHAQUE invocation (pas seulement quand
  # une cible est reexecutee) : l'etat du journal (.statlab_log_state) est
  # un etat de SESSION R, remis a zero a chaque nouveau processus, alors
  # que le cache {targets} persiste entre les sessions. L'initialiser ici,
  # avant tar_make(), garantit qu'il est pret des la premiere cible
  # effectivement executee, meme si toutes les etapes precedentes du plan
  # sont deja a jour et sautees.
  st_log_init(project_dir)

  # tar_progress()/tar_meta() ne distinguent pas directement "recalcule a
  # cet appel" de "recalcule lors d'un appel anterieur" : on capture la
  # liste des cibles obsoletes AVANT l'execution pour ne rapporter que
  # celles-la.
  a_recalculer_avant <- tryCatch(targets::tar_outdated(callr_function = NULL), error = function(e) character(0))

  targets::tar_make(callr_function = NULL, reporter = "silent")

  meta <- tryCatch(targets::tar_meta(), error = function(e) NULL)
  execute <- if (!is.null(meta)) {
    meta[meta$name %in% a_recalculer_avant, c("name", "seconds"), drop = FALSE]
  } else {
    data.frame(name = character(0), seconds = numeric(0))
  }
  rownames(execute) <- NULL

  if (nrow(execute) == 0) {
    cli::cli_alert_success("Pipeline a jour : aucune etape a reexecuter.")
  } else {
    cli::cli_alert_success("Pipeline execute : {nrow(execute)} etape(s) reexecutee(s) en {round(sum(execute$seconds), 1)} s ({paste(execute$name, collapse = ', ')}).")
  }

  st_log("pipeline", module = "pipeline", n_etapes_executees = nrow(execute), level = "info")

  invisible(execute)
}

#' Afficher l'etat du pipeline d'un projet
#'
#' Indique, cible par cible, ce qui est a jour et ce qui doit etre
#' recalcule, et pourquoi (fichier source modifie, section de `config.yml`
#' modifiee, ou etape en aval d'une cible elle-meme obsolete). N'execute
#' rien : lecture seule.
#'
#' @inheritParams st_pipeline
#'
#' @return Invisiblement, un data.frame (colonnes `cible`, `etat`,
#'   `raison`).
#' @export
st_status <- function(config_path) {
  checkmate::assert_string(config_path, min.chars = 1)
  if (!file.exists(config_path)) {
    cli::cli_abort("Fichier de configuration introuvable : {.path {config_path}}")
  }

  project_dir <- .pipeline_prepare(config_path)
  ancien_wd <- getwd()
  setwd(project_dir)
  on.exit(setwd(ancien_wd), add = TRUE)

  if (!file.exists(file.path(project_dir, "_targets"))) {
    cli::cli_alert_info("Aucune execution anterieure : toutes les etapes seront calculees au prochain st_pipeline().")
  }

  rapport <- .pipeline_status_table()

  a_jour <- rapport[rapport$etat == "a_jour", , drop = FALSE]
  a_recalculer <- rapport[rapport$etat == "a_recalculer", , drop = FALSE]

  cli::cli_h1("Etat du pipeline")
  cli::cli_bullets(c("*" = "A jour : {nrow(a_jour)}", "*" = "A recalculer : {nrow(a_recalculer)}"))

  if (nrow(a_recalculer) > 0) {
    cli::cli_h2("Etapes a recalculer")
    for (i in seq_len(nrow(a_recalculer))) {
      cli::cli_bullets(stats::setNames(
        sprintf("{.strong %s} : %s", a_recalculer$cible[i], a_recalculer$raison[i]),
        "*"
      ))
    }
  }

  invisible(rapport)
}

# --- Preparation du plan {targets} -------------------------------------------

.pipeline_prepare <- function(config_path) {
  config_path_abs <- normalizePath(config_path, winslash = "/", mustWork = TRUE)
  project_dir <- dirname(config_path_abs)

  template_path <- system.file("templates", "_targets.R", package = "statlab")
  if (!nzchar(template_path)) {
    cli::cli_abort("Gabarit de pipeline introuvable dans le package (inst/templates/_targets.R).")
  }

  contenu <- readLines(template_path)
  contenu <- gsub("__CONFIG_PATH__", config_path_abs, contenu, fixed = TRUE)
  writeLines(contenu, file.path(project_dir, "_targets.R"), useBytes = TRUE)

  project_dir
}

.pipeline_status_table <- function() {
  reseau <- targets::tar_network(targets_only = TRUE, callr_function = NULL)
  sommets <- reseau$vertices
  aretes <- reseau$edges

  if (is.null(sommets) || nrow(sommets) == 0) {
    return(data.frame(cible = character(0), etat = character(0), raison = character(0), stringsAsFactors = FALSE))
  }

  outdated <- targets::tar_outdated(callr_function = NULL)

  raisons <- vapply(sommets$name, function(cible) {
    if (!cible %in% outdated) {
      return(NA_character_)
    }
    parents <- aretes$from[aretes$to == cible]
    parents_outdated <- intersect(parents, outdated)
    if (length(parents_outdated) == 0) {
      "fichier ou dependance directe modifie(e) (config.yml ou fichier source)"
    } else {
      sprintf("en aval de : %s", paste(parents_outdated, collapse = ", "))
    }
  }, character(1))

  data.frame(
    cible = sommets$name,
    etat = ifelse(sommets$name %in% outdated, "a_recalculer", "a_jour"),
    raison = ifelse(sommets$name %in% outdated, raisons, "a jour"),
    stringsAsFactors = FALSE
  )
}

# --- Fonctions utilisees par inst/templates/_targets.R -----------------------
# (invoquees par nom de cible ; conservees ici, dans le package, plutot que
# dans le gabarit lui-meme, pour rester testables independamment.)

.pipeline_mini_config <- function(...) {
  structure(list(...), class = "statlab_config", valid = TRUE)
}

.pipeline_source_paths <- function(sources_spec, project_dir) {
  vapply(sources_spec, function(entry) {
    normalizePath(file.path(project_dir, entry$fichier), winslash = "/", mustWork = TRUE)
  }, character(1))
}

.pipeline_read_sources <- function(sources_spec, project_dir) {
  config_sources <- .pipeline_mini_config(sources = sources_spec)
  attr(config_sources, "project_dir") <- project_dir
  st_read_all_sources(config_sources)
}

.pipeline_all_profiles <- function(sources) {
  lapply(sources, st_profile)
}

.pipeline_all_anomalies <- function(sources, profiles, dictionnaire) {
  config_dict <- .pipeline_mini_config(dictionnaire = dictionnaire)
  Map(function(data, profile) st_detect_anomalies(data, profile, config_dict), sources, profiles)
}

.pipeline_run_reconciliation <- function(sources, reconciliation_spec) {
  if (!is.null(reconciliation_spec)) {
    config_recon <- .pipeline_mini_config(reconciliation = reconciliation_spec)
    return(st_reconcile(sources, config_recon)$table)
  }
  if (length(sources) != 1) {
    cli::cli_abort(c(
      "Plusieurs sources sont declarees sans operation de reconciliation.",
      "i" = "Declarer une section 'reconciliation' dans config.yml pour indiquer comment les assembler."
    ))
  }
  sources[[1]]
}

.pipeline_run_preparation <- function(donnees, preparation_spec) {
  config_prep <- .pipeline_mini_config(preparation = preparation_spec)
  st_prepare(donnees, config_prep)
}

.pipeline_run_table1 <- function(donnees_finales, dictionnaire, tableau1_spec, rendu_spec) {
  if (is.null(tableau1_spec)) {
    return(NULL)
  }
  config_t1 <- .pipeline_mini_config(dictionnaire = dictionnaire, analyse = list(tableau_1 = tableau1_spec), rendu = rendu_spec)
  st_table1(donnees_finales, config_t1)
}

.pipeline_run_comparaisons <- function(donnees_finales, dictionnaire, comparisons_spec) {
  if (is.null(comparisons_spec)) {
    return(list())
  }
  config_cmp <- .pipeline_mini_config(dictionnaire = dictionnaire, analyse = list(comparaisons = comparisons_spec))

  resultats <- list()
  for (entry in comparisons_spec) {
    paired <- isTRUE(entry$apparie)
    for (variable in entry$variables) {
      resultat <- st_compare(donnees_finales, variable, entry$groupe, config_cmp, paired = paired)
      resultats[[length(resultats) + 1]] <- list(
        variable = variable, group = entry$groupe,
        nature_variable = .lookup_nature(variable, config_cmp),
        result = resultat
      )
    }
  }
  resultats
}

.pipeline_run_graphiques <- function(donnees_finales, comparaisons, dictionnaire, project_dir, declinaisons) {
  if (length(comparaisons) == 0) {
    return(character(0))
  }
  config_plot <- .pipeline_mini_config(dictionnaire = dictionnaire, rendu = list(declinaisons = declinaisons))
  output_dir <- file.path(project_dir, "sorties", "graphiques")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  chemins <- character(0)
  vus <- character(0)
  for (item in comparaisons) {
    cle <- paste(item$variable, item$group, sep = "|")
    if (cle %in% vus) {
      next
    }
    vus <- c(vus, cle)

    graphique <- tryCatch(
      {
        if (item$nature_variable %in% c("continue", "entiere")) {
          st_plot_box(donnees_finales, item$variable, item$group, config_plot, test_result = item$result)
        } else {
          st_plot_bar(donnees_finales, item$variable, item$group, config_plot, test_result = item$result)
        }
      },
      error = function(e) NULL
    )
    if (is.null(graphique)) {
      next
    }

    nom <- sprintf("%s_selon_%s", item$variable, item$group)
    fichiers <- st_save_plot(
      graphique, file.path(output_dir, nom), config_plot,
      formats = "png", variants = declinaisons, width = 6, height = 4, dpi = 150
    )
    chemins <- c(chemins, fichiers)
  }
  chemins
}

.pipeline_run_audit <- function(config_file, project_dir) {
  st_audit(config_file, output_dir = file.path(project_dir, "sorties", "audit"))
}

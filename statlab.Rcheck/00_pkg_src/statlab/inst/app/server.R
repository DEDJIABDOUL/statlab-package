# =============================================================================
# Serveur : couche mince au-dessus du package. Chaque observateur/reactive
# ci-dessous fait l'une de deux choses seulement :
#   (a) transcrire une saisie operateur (widgets) vers la forme attendue
#       par config.yml (aucune regle, aucun calcul) ;
#   (b) appeler une fonction EXISTANTE du package (exportee, ou interne
#       via ::: quand il s'agit d'un helper deja construit pour
#       R/pipeline.R) et afficher son resultat.
# Aucun detecteur, aucune regle de validation, aucun test statistique,
# aucune decision de rendu n'est reimplemente ici : tout vient de
# statlab::*. Le config.yml ecrit par cette interface est le meme objet
# que celui lu par la ligne de commande ; il n'existe aucun etat cache
# propre a l'interface qui ne soit pas reflete dans ce fichier.
# =============================================================================

# --- Petits utilitaires de presentation (transcription pure, aucune regle) --

.app_id_unique <- function(base, existants) {
  if (!(base %in% existants)) {
    return(base)
  }
  i <- 2L
  while (sprintf("%s_%d", base, i) %in% existants) {
    i <- i + 1L
  }
  sprintf("%s_%d", base, i)
}

.app_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.app_alert <- function(texte, type = c("success", "danger", "info")) {
  type <- match.arg(type)
  shiny::div(class = sprintf("alert alert-%s", type), texte)
}

# Traduit rv$dictionnaire_df (une ligne par variable, colonnes texte) vers
# la forme liste attendue par le schema config.yml. Transcription pure.
.app_dictionnaire_config <- function(df) {
  stats::setNames(lapply(seq_len(nrow(df)), function(i) {
    entree <- list(nature = df$nature[i])
    if (nzchar(df$libelle[i])) entree$libelle <- df$libelle[i]
    if (nzchar(df$unite[i])) entree$unite <- df$unite[i]
    if (nzchar(df$modalites[i])) entree$modalites <- trimws(strsplit(df$modalites[i], ",", fixed = TRUE)[[1]])
    entree
  }), df$variable)
}

# Assemble le config.yml complet a partir de l'etat reactif ET des widgets
# actuels (n operations/comparaisons generees dynamiquement). Transcription
# pure : aucune regle metier, uniquement la mise en forme attendue par le
# schema (inst/schema/config_schema.yml).
.app_build_config_list <- function(rv, input) {
  cfg <- list(projet = list(nom = if (nzchar(rv$nom_projet)) rv$nom_projet else "Projet sans nom"))
  cfg$sources <- rv$sources

  if (!is.null(rv$dictionnaire_df) && nrow(rv$dictionnaire_df) > 0) {
    cfg$dictionnaire <- .app_dictionnaire_config(rv$dictionnaire_df)
  }

  n_recon <- input$n_operations_reconciliation
  if (!is.null(n_recon) && isTRUE(n_recon > 0)) {
    cfg$reconciliation <- lapply(seq_len(n_recon), function(i) {
      list(
        operation = "joindre",
        gauche = input[[paste0("recon_gauche_", i)]] %||% "",
        droite = input[[paste0("recon_droite_", i)]] %||% "",
        cle = input[[paste0("recon_cle_", i)]] %||% "",
        type = input[[paste0("recon_type_jointure_", i)]] %||% "gauche",
        resultat = input[[paste0("recon_resultat_", i)]] %||% sprintf("resultat_%d", i)
      )
    })
  }

  # Aucun constructeur de preparation dans l'interface (hors perimetre des
  # ecrans demandes) : une strategie de manquants "conserver" par defaut
  # permet a st_prepare()/st_report() de s'executer ; un besoin de
  # preparation plus riche (dates, recodages, derivations...) s'edite a la
  # main dans config.yml, ce que l'interface ne remet jamais en cause.
  cfg$preparation <- list(manquants = list(list(strategie = "conserver")))

  analyse <- list()
  if (isTRUE(input$tableau1_actif) && length(input$tableau1_variables) > 0) {
    tableau_1 <- list(variables = input$tableau1_variables)
    if (!is.null(input$tableau1_stratification) && nzchar(input$tableau1_stratification)) {
      tableau_1$stratification <- input$tableau1_stratification
    }
    tableau_1$denominateur <- input$tableau1_denominateur %||% "exclure_manquants"
    analyse$tableau_1 <- tableau_1
  }

  n_comp <- input$n_comparaisons
  if (!is.null(n_comp) && isTRUE(n_comp > 0)) {
    analyse$comparaisons <- lapply(seq_len(n_comp), function(i) {
      list(
        variables = input[[paste0("comp_variables_", i)]] %||% character(0),
        groupe = input[[paste0("comp_groupe_", i)]] %||% "",
        apparie = isTRUE(input[[paste0("comp_apparie_", i)]])
      )
    })
  }
  if (length(analyse) > 0) {
    cfg$analyse <- analyse
  }

  cfg$rendu <- list(declinaisons = "ecran")
  cfg
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Serveur ------------------------------------------------------------------

server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    project_dir = getOption("statlab.app.project_dir", getwd()),
    nom_projet = "",
    sources = list(),
    dictionnaire_df = NULL,
    sources_data = NULL,
    profils = NULL,
    anomalies = NULL,
    audit_dir = NULL,
    donnees_finales = NULL,
    table1_obj = NULL,
    comparaisons_resultats = list(),
    rapport_dir = NULL
  )

  # --- 1. Projet --------------------------------------------------------------

  racines <- c(Projet = rv$project_dir, shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "choisir_dossier", roots = racines, session = session)

  shiny::observeEvent(input$choisir_dossier, {
    chemin <- shinyFiles::parseDirPath(racines, input$choisir_dossier)
    if (length(chemin) == 1 && nzchar(chemin)) {
      rv$project_dir <- chemin
    }
  })

  output$dossier_actuel <- shiny::renderPrint(cat(rv$project_dir))

  shiny::observeEvent(input$nom_projet, {
    rv$nom_projet <- input$nom_projet
  })

  shiny::observeEvent(input$fichiers_sources, {
    fichiers <- input$fichiers_sources
    shiny::req(fichiers)

    donnees_brutes_dir <- file.path(rv$project_dir, "donnees_brutes")
    if (!dir.exists(donnees_brutes_dir)) {
      dir.create(donnees_brutes_dir, recursive = TRUE)
    }

    for (i in seq_len(nrow(fichiers))) {
      nom_fichier <- fichiers$name[i]
      destination <- file.path(donnees_brutes_dir, nom_fichier)
      file.copy(fichiers$datapath[i], destination, overwrite = TRUE)

      ids_existants <- vapply(rv$sources, function(s) s$id, character(1))
      id_source <- .app_id_unique(tools::file_path_sans_ext(nom_fichier), ids_existants)
      rv$sources <- c(rv$sources, list(list(id = id_source, fichier = file.path("donnees_brutes", nom_fichier))))
    }
  })

  output$table_sources <- DT::renderDT({
    df <- if (length(rv$sources) == 0) {
      data.frame(id = character(0), fichier = character(0))
    } else {
      data.frame(
        id = vapply(rv$sources, function(s) s$id, character(1)),
        fichier = vapply(rv$sources, function(s) s$fichier, character(1))
      )
    }
    DT::datatable(df, rownames = FALSE, selection = "none", options = list(dom = "t", paging = FALSE))
  })

  shiny::observeEvent(input$btn_auditer, {
    shiny::validate(shiny::need(length(rv$sources) > 0, "Declarer au moins un fichier source avant d'auditer."))

    cfg <- list(projet = list(nom = if (nzchar(rv$nom_projet)) rv$nom_projet else "Projet sans nom"), sources = rv$sources)
    config_path <- file.path(rv$project_dir, "config.yml")
    yaml::write_yaml(cfg, config_path)

    resultat <- tryCatch(
      statlab::st_audit(config_path, output_dir = file.path(rv$project_dir, "sorties", "audit")),
      error = function(e) e
    )

    if (inherits(resultat, "error")) {
      output$statut_audit <- shiny::renderUI(.app_alert(conditionMessage(resultat), "danger"))
      return(invisible(NULL))
    }

    rv$audit_dir <- file.path(rv$project_dir, "sorties", "audit")
    sources_data <- statlab:::.pipeline_read_sources(cfg$sources, rv$project_dir)
    rv$sources_data <- sources_data
    rv$profils <- lapply(sources_data, statlab::st_profile)
    rv$anomalies <- Map(function(d, p) statlab::st_detect_anomalies(d, p), sources_data, rv$profils)

    shiny::addResourcePath("audit_files", rv$audit_dir)
    output$statut_audit <- shiny::renderUI(.app_alert("Audit termine : voir l'onglet 2. Audit.", "success"))
    shiny::updateSelectInput(session, "audit_source_select", choices = names(sources_data))
    shiny::updateNavbarPage(session, "nav_principal", selected = "audit")
  })

  # --- 2. Audit -----------------------------------------------------------------

  output$audit_rapport_cadre <- shiny::renderUI({
    shiny::req(rv$audit_dir)
    chemin_html <- file.path(rv$audit_dir, "audit.html")
    if (!file.exists(chemin_html)) {
      return(shiny::p("Aucun rapport d'audit disponible : utiliser le bouton « Auditer » (onglet 1. Projet)."))
    }
    shiny::tags$iframe(src = "audit_files/audit.html", width = "100%", height = "800px", style = "border:1px solid #ccc;")
  })

  output$table_profil <- DT::renderDT({
    shiny::req(rv$profils, input$audit_source_select)
    profil <- rv$profils[[input$audit_source_select]]
    shiny::req(profil)
    DT::datatable(
      profil[, setdiff(names(profil), "sample_values"), drop = FALSE],
      rownames = FALSE, filter = "top", options = list(pageLength = 15)
    )
  })

  output$anomalies_accordeon <- shiny::renderUI({
    shiny::req(rv$anomalies)
    avec_source <- Map(function(df, id) {
      if (nrow(df) > 0) df$source <- id
      df
    }, rv$anomalies, names(rv$anomalies))
    toutes <- do.call(rbind, avec_source)

    if (is.null(toutes) || nrow(toutes) == 0) {
      return(shiny::p("Aucune anomalie detectee."))
    }

    libelles_severite <- c(bloquant = "Bloquant", avertissement = "Avertissement", information = "Information")
    sections <- lapply(names(libelles_severite), function(sev) {
      sous <- toutes[toutes$severity == sev, , drop = FALSE]
      if (nrow(sous) == 0) {
        return(NULL)
      }
      items <- lapply(seq_len(nrow(sous)), function(i) {
        ligne <- sous[i, ]
        indices <- utils::head(ligne$rows_affected[[1]], 20)
        source_donnees <- rv$sources_data[[ligne$source]]
        extrait <- if (length(indices) > 0 && !is.null(source_donnees)) {
          DT::datatable(source_donnees[indices, , drop = FALSE], rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
        } else {
          shiny::p("Aucune ligne individuelle associee.")
        }
        shiny::tags$details(
          shiny::tags$summary(sprintf(
            "[%s] %s — %s (%d observation(s))",
            ligne$source, ligne$check_id, ifelse(is.na(ligne$variable), "(ensemble)", ligne$variable), ligne$n_affected
          )),
          shiny::tags$p(ligne$detail),
          extrait
        )
      })
      shiny::tagList(shiny::h5(sprintf("%s (%d)", libelles_severite[[sev]], nrow(sous))), items)
    })
    shiny::tagList(sections)
  })

  # --- 3. Configuration -----------------------------------------------------------

  shiny::observeEvent(rv$profils, {
    shiny::req(rv$profils)
    toutes_variables <- unique(unlist(lapply(rv$profils, function(p) p$name)))
    nature_defaut <- stats::setNames(rep(NA_character_, length(toutes_variables)), toutes_variables)
    for (p in rv$profils) {
      for (i in seq_len(nrow(p))) {
        if (is.na(nature_defaut[[p$name[i]]])) {
          nature_defaut[[p$name[i]]] <- p$inferred_nature[i]
        }
      }
    }
    rv$dictionnaire_df <- data.frame(
      variable = toutes_variables,
      nature = unname(nature_defaut[toutes_variables]),
      libelle = toutes_variables,
      unite = "",
      modalites = "",
      stringsAsFactors = FALSE
    )
  })

  output$table_dictionnaire <- DT::renderDT(
    {
      shiny::req(rv$dictionnaire_df)
      DT::datatable(
        rv$dictionnaire_df,
        rownames = FALSE, selection = "none",
        editable = list(target = "cell", disable = list(columns = 0)),
        options = list(dom = "t", paging = FALSE, scrollX = TRUE)
      )
    },
    server = TRUE
  )

  shiny::observeEvent(input$table_dictionnaire_cell_edit, {
    rv$dictionnaire_df <- DT::editData(rv$dictionnaire_df, input$table_dictionnaire_cell_edit, rownames = FALSE)
  })

  variables_disponibles <- shiny::reactive({
    if (is.null(rv$dictionnaire_df)) character(0) else rv$dictionnaire_df$variable
  })

  shiny::observeEvent(variables_disponibles(), {
    shiny::updateSelectInput(session, "tableau1_variables", choices = variables_disponibles())
    shiny::updateSelectInput(session, "tableau1_stratification", choices = c("(aucune)" = "", variables_disponibles()))
  })

  # Reconciliation : seule l'operation "joindre" est proposee dans
  # l'interface (le cas le plus courant : assemblage de deux sources sur un
  # identifiant commun). "empiler"/"pivoter_long" restent disponibles en
  # editant config.yml directement (le moteur les prend en charge sans
  # restriction ; seul le constructeur graphique est simplifie).
  output$blocs_reconciliation <- shiny::renderUI({
    n <- input$n_operations_reconciliation
    shiny::req(!is.null(n))
    if (n == 0) {
      return(NULL)
    }
    ids_sources <- vapply(rv$sources, function(s) s$id, character(1))

    blocs <- lapply(seq_len(n), function(i) {
      resultats_precedents <- if (n > 1) sprintf("resultat_%d", seq_len(n)) else character(0)
      ids_disponibles <- unique(c(ids_sources, resultats_precedents))
      shiny::wellPanel(
        shiny::h5(sprintf("Operation %d (jointure)", i)),
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput(paste0("recon_gauche_", i), "Gauche", choices = ids_disponibles)),
          shiny::column(3, shiny::selectInput(paste0("recon_droite_", i), "Droite", choices = ids_disponibles)),
          shiny::column(2, shiny::textInput(paste0("recon_cle_", i), "Cle")),
          shiny::column(2, shiny::selectInput(paste0("recon_type_jointure_", i), "Type", choices = c("gauche", "interieure", "complete"))),
          shiny::column(2, shiny::textInput(paste0("recon_resultat_", i), "Resultat", value = sprintf("resultat_%d", i)))
        )
      )
    })
    shiny::tagList(blocs)
  })

  output$blocs_comparaisons <- shiny::renderUI({
    n <- input$n_comparaisons
    shiny::req(!is.null(n))
    if (n == 0) {
      return(NULL)
    }
    choix <- variables_disponibles()
    blocs <- lapply(seq_len(n), function(i) {
      shiny::wellPanel(
        shiny::h5(sprintf("Comparaison %d", i)),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(paste0("comp_variables_", i), "Variables a comparer", choices = choix, multiple = TRUE)),
          shiny::column(4, shiny::selectInput(paste0("comp_groupe_", i), "Groupe", choices = choix)),
          shiny::column(2, shiny::checkboxInput(paste0("comp_apparie_", i), "Appariee", value = FALSE))
        )
      )
    })
    shiny::tagList(blocs)
  })

  config_courant <- shiny::reactive(.app_build_config_list(rv, input))

  validation_resultat <- shiny::reactive({
    cfg <- config_courant()
    config_obj <- structure(
      cfg,
      class = "statlab_config", project_dir = rv$project_dir,
      path = file.path(rv$project_dir, "config.yml"), valid = FALSE
    )
    tryCatch(statlab::st_validate_config(config_obj), error = function(e) e)
  })

  output$statut_validation <- shiny::renderUI({
    resultat <- validation_resultat()
    if (inherits(resultat, "error")) {
      .app_alert(conditionMessage(resultat), "danger")
    } else {
      .app_alert("Configuration valide.", "success")
    }
  })

  output$yaml_apercu <- shiny::renderPrint(cat(yaml::as.yaml(config_courant())))

  shiny::observeEvent(input$btn_enregistrer_config, {
    resultat <- tryCatch(
      {
        cfg <- config_courant()
        chemin <- file.path(rv$project_dir, "config.yml")
        yaml::write_yaml(cfg, chemin)
        chemin
      },
      error = function(e) e
    )
    if (inherits(resultat, "error")) {
      shiny::showNotification(conditionMessage(resultat), type = "error", duration = NULL)
    } else {
      shiny::showNotification(sprintf("config.yml enregistre : %s", resultat), type = "message")
    }
  })

  # --- 4. Resultats -----------------------------------------------------------------

  shiny::observeEvent(input$btn_calculer_resultats, {
    resultat <- tryCatch(
      {
        cfg <- config_courant()
        sources_data <- statlab:::.pipeline_read_sources(cfg$sources, rv$project_dir)
        donnees <- statlab:::.pipeline_run_reconciliation(sources_data, cfg$reconciliation)
        donnees_finales <- statlab:::.pipeline_run_preparation(donnees, cfg$preparation)
        table1_obj <- if (!is.null(cfg$analyse$tableau_1)) {
          statlab:::.pipeline_run_table1(donnees_finales, cfg$dictionnaire, cfg$analyse$tableau_1, cfg$rendu)
        } else {
          NULL
        }
        comparaisons <- if (!is.null(cfg$analyse$comparaisons)) {
          statlab:::.pipeline_run_comparaisons(donnees_finales, cfg$dictionnaire, cfg$analyse$comparaisons)
        } else {
          list()
        }
        list(donnees_finales = donnees_finales, table1_obj = table1_obj, comparaisons = comparaisons, dictionnaire = cfg$dictionnaire)
      },
      error = function(e) e
    )

    if (inherits(resultat, "error")) {
      output$statut_resultats <- shiny::renderUI(.app_alert(conditionMessage(resultat), "danger"))
      return(invisible(NULL))
    }

    rv$donnees_finales <- resultat$donnees_finales
    rv$table1_obj <- resultat$table1_obj
    rv$comparaisons_resultats <- resultat$comparaisons
    output$statut_resultats <- shiny::renderUI(.app_alert("Resultats calcules.", "success"))

    choix_graphiques <- vapply(rv$comparaisons_resultats, function(item) sprintf("%s selon %s", item$variable, item$group), character(1))
    shiny::updateSelectInput(session, "graphique_select", choices = choix_graphiques)
  })

  output$table_tableau1 <- DT::renderDT({
    shiny::req(rv$table1_obj)
    DT::datatable(statlab::st_table1_csv(rv$table1_obj), rownames = FALSE, filter = "top", options = list(scrollX = TRUE))
  })

  output$table_comparaisons <- DT::renderDT({
    shiny::req(length(rv$comparaisons_resultats) > 0)
    lignes <- lapply(rv$comparaisons_resultats, function(item) {
      r <- item$result
      data.frame(
        Variable = item$variable, Groupe = item$group,
        Test = sprintf('<span title="%s">%s</span>', .app_html_escape(r$justification), r$test_name),
        p_value = signif(r$p_value, 3),
        stringsAsFactors = FALSE
      )
    })
    DT::datatable(do.call(rbind, lignes), escape = FALSE, rownames = FALSE)
  })

  shiny::observeEvent(rv$comparaisons_resultats, {
    choix <- vapply(rv$comparaisons_resultats, function(item) sprintf("%s selon %s", item$variable, item$group), character(1))
    shiny::updateSelectInput(session, "graphique_select", choices = choix)
  })

  output$graphique_affichage <- shiny::renderPlot({
    shiny::req(input$graphique_select, length(rv$comparaisons_resultats) > 0, rv$donnees_finales)
    choix <- vapply(rv$comparaisons_resultats, function(item) sprintf("%s selon %s", item$variable, item$group), character(1))
    idx <- match(input$graphique_select, choix)
    shiny::req(!is.na(idx))

    item <- rv$comparaisons_resultats[[idx]]
    cfg <- config_courant()
    config_plot <- structure(list(dictionnaire = cfg$dictionnaire), class = "statlab_config", valid = TRUE)
    nature <- statlab:::.lookup_nature(item$variable, config_plot)

    graphique <- if (nature %in% c("continue", "entiere")) {
      statlab::st_plot_box(rv$donnees_finales, item$variable, item$group, config_plot, test_result = item$result)
    } else {
      statlab::st_plot_bar(rv$donnees_finales, item$variable, item$group, config_plot, test_result = item$result)
    }
    statlab::st_apply_variant(graphique, input$declinaison_graphique)
  })

  # --- 5. Rapport -----------------------------------------------------------------

  shiny::observeEvent(input$btn_generer_rapport, {
    shiny::validate(shiny::need(length(input$rapport_formats) > 0, "Choisir au moins un format."))

    resultat <- tryCatch(
      statlab::st_report(
        file.path(rv$project_dir, "config.yml"),
        output_dir = file.path(rv$project_dir, "sorties", "rapport"),
        formats = input$rapport_formats
      ),
      error = function(e) e
    )

    if (inherits(resultat, "error")) {
      output$statut_rapport <- shiny::renderUI(.app_alert(conditionMessage(resultat), "danger"))
      return(invisible(NULL))
    }

    rv$rapport_dir <- file.path(rv$project_dir, "sorties", "rapport")
    shiny::addResourcePath("rapport_files", rv$rapport_dir)
    output$statut_rapport <- shiny::renderUI(.app_alert("Rapport genere.", "success"))
  })

  output$rapport_liens <- shiny::renderUI({
    shiny::req(rv$rapport_dir, dir.exists(rv$rapport_dir))
    fichiers <- list.files(rv$rapport_dir)
    shiny::req(length(fichiers) > 0)
    shiny::tags$ul(lapply(fichiers, function(f) {
      shiny::tags$li(shiny::tags$a(href = file.path("rapport_files", f), f, target = "_blank"))
    }))
  })
}

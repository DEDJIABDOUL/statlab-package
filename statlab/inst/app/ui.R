# =============================================================================
# Interface (presentation uniquement). Aucune logique metier ici : chaque
# element affiche une donnee deja calculee par le serveur (lui-meme une
# couche mince sur le package), ou transmet une saisie operateur.
# =============================================================================

ui <- shiny::navbarPage(
  title = "statlab",
  id = "nav_principal",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

  # --- 1. Projet --------------------------------------------------------------
  shiny::tabPanel(
    "1. Projet", value = "projet",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::h4("Dossier du projet"),
        shinyFiles::shinyDirButton("choisir_dossier", "Choisir le dossier...", "Selectionner le dossier du projet"),
        shiny::verbatimTextOutput("dossier_actuel"),
        shiny::tags$hr(),
        shiny::textInput("nom_projet", "Nom du projet", placeholder = "Etude ACTIF - suivi a 12 mois"),
        shiny::tags$hr(),
        shiny::h4("Fichiers sources"),
        shiny::fileInput(
          "fichiers_sources", "Ajouter des fichiers (glisser-deposer)",
          multiple = TRUE, accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
        ),
        shiny::tags$hr(),
        shiny::actionButton("btn_auditer", "Auditer", class = "btn-primary", width = "100%")
      ),
      shiny::mainPanel(
        width = 8,
        shiny::h4("Sources declarees"),
        DT::DTOutput("table_sources"),
        shiny::tags$br(),
        shiny::uiOutput("statut_audit")
      )
    )
  ),

  # --- 2. Audit -----------------------------------------------------------------
  shiny::tabPanel(
    "2. Audit", value = "audit",
    shiny::h4("Rapport d'audit"),
    shiny::uiOutput("audit_rapport_cadre"),
    shiny::tags$hr(),
    shiny::h4("Profil des variables"),
    shiny::selectInput("audit_source_select", "Source", choices = NULL, width = "300px"),
    DT::DTOutput("table_profil"),
    shiny::tags$hr(),
    shiny::h4("Anomalies detectees"),
    shiny::uiOutput("anomalies_accordeon")
  ),

  # --- 3. Configuration -----------------------------------------------------------
  shiny::tabPanel(
    "3. Configuration", value = "configuration",
    shiny::h4("Dictionnaire des variables"),
    shiny::p("Cliquer sur une cellule (nature, libelle, unite, modalites) pour la modifier. Modalites : valeurs separees par des virgules."),
    DT::DTOutput("table_dictionnaire"),
    shiny::tags$hr(),

    shiny::h4("Reconciliation"),
    shiny::p("Necessaire des que plusieurs sources sont declarees ; sans effet si une seule source existe."),
    shiny::numericInput("n_operations_reconciliation", "Nombre d'operations", value = 0, min = 0, max = 5, step = 1, width = "250px"),
    shiny::uiOutput("blocs_reconciliation"),
    shiny::tags$hr(),

    shiny::h4("Plan d'analyse"),
    shiny::checkboxInput("tableau1_actif", "Inclure un Tableau 1", value = FALSE),
    shiny::conditionalPanel(
      "input.tableau1_actif",
      shiny::selectInput("tableau1_variables", "Variables a decrire", choices = NULL, multiple = TRUE, width = "100%"),
      shiny::selectInput("tableau1_stratification", "Stratification (optionnelle)", choices = c("(aucune)" = ""), width = "300px"),
      shiny::selectInput("tableau1_denominateur", "Denominateur des pourcentages",
        choices = c("Exclure les manquants" = "exclure_manquants", "Inclure les manquants" = "inclure_manquants"),
        width = "300px"
      )
    ),
    shiny::tags$br(),
    shiny::numericInput("n_comparaisons", "Nombre de comparaisons", value = 0, min = 0, max = 10, step = 1, width = "250px"),
    shiny::uiOutput("blocs_comparaisons"),
    shiny::tags$hr(),

    shiny::h4("Validation"),
    shiny::uiOutput("statut_validation"),
    shiny::actionButton("btn_enregistrer_config", "Enregistrer config.yml", class = "btn-primary"),
    shiny::tags$hr(),
    shiny::h4("Apercu du YAML (lecture seule)"),
    shiny::verbatimTextOutput("yaml_apercu")
  ),

  # --- 4. Resultats -----------------------------------------------------------------
  shiny::tabPanel(
    "4. Resultats", value = "resultats",
    shiny::actionButton("btn_calculer_resultats", "Calculer les resultats", class = "btn-primary"),
    shiny::uiOutput("statut_resultats"),
    shiny::tags$hr(),
    shiny::h4("Tableau 1"),
    DT::DTOutput("table_tableau1"),
    shiny::tags$hr(),
    shiny::h4("Comparaisons"),
    shiny::p("Survoler la colonne « Test » pour afficher la justification methodologique."),
    DT::DTOutput("table_comparaisons"),
    shiny::tags$hr(),
    shiny::h4("Graphiques"),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput("graphique_select", "Comparaison", choices = NULL)),
      shiny::column(4, shiny::selectInput("declinaison_graphique", "Declinaison",
        choices = c("ecran", "impression", "projection")
      ))
    ),
    shiny::plotOutput("graphique_affichage", height = "450px")
  ),

  # --- 5. Rapport -----------------------------------------------------------------
  shiny::tabPanel(
    "5. Rapport", value = "rapport",
    shiny::checkboxGroupInput("rapport_formats", "Formats",
      choices = c("Word (docx)" = "docx", "PDF" = "pdf"), selected = c("docx", "pdf")
    ),
    shiny::actionButton("btn_generer_rapport", "Generer", class = "btn-primary"),
    shiny::uiOutput("statut_rapport"),
    shiny::tags$hr(),
    shiny::h4("Livrables"),
    shiny::uiOutput("rapport_liens")
  )
)

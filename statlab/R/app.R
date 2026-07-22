# =============================================================================
# Interface graphique locale (Shiny). Ce fichier ne fait que lancer
# l'application definie dans inst/app/ (ui.R, server.R) : toute la logique
# d'interface vit la-bas, et elle-meme ne fait qu'appeler les fonctions
# du package (aucune regle, aucun calcul, aucune decision propre a
# l'interface -- cf. inst/app/server.R).
#
# Execution strictement locale : host = "127.0.0.1" (jamais "0.0.0.0"),
# aucune authentification, aucun envoi de donnees. Tout ce qui est produit
# via l'interface (config.yml, rapports) est strictement identique a ce que
# produirait la ligne de commande : l'interface n'est qu'une facon
# alternative de remplir config.yml et d'appeler st_audit()/st_report()/
# st_pipeline(), jamais un chemin d'acces exclusif a une fonctionnalite.
# =============================================================================

#' Lancer l'interface graphique locale
#'
#' Lance une application Shiny locale permettant de piloter un projet
#' statlab de bout en bout : declaration des sources, audit, construction
#' de `config.yml` (dictionnaire, reconciliation, plan d'analyse),
#' consultation des resultats, generation du rapport.
#'
#' L'interface est une couche mince au-dessus du package : elle lit et
#' ecrit `config.yml`, et appelle les memes fonctions que la ligne de
#' commande. Tout projet traite via l'interface produit un `config.yml`
#' qui, rejoue en ligne de commande (`statlab rapporter --config=...`, ou
#' [st_report()]), produit un resultat identique.
#'
#' L'application n'ecoute que sur l'interface reseau locale (`127.0.0.1`)
#' et ne met en oeuvre aucune authentification : elle n'est pas concue pour
#' etre exposee au-dela de la machine de l'operateur.
#'
#' @param project_dir Repertoire (chr) du projet a ouvrir au demarrage.
#'   Par defaut, le repertoire courant. Cree s'il n'existe pas encore un
#'   `config.yml` (le projet peut demarrer vide, sources incluses).
#'
#' @return Invisiblement, `NULL`. La fonction bloque jusqu'a la fermeture
#'   de l'application (comportement standard de [shiny::runApp()]).
#' @export
st_app <- function(project_dir = getwd()) {
  checkmate::assert_string(project_dir, min.chars = 1)
  if (!dir.exists(project_dir)) {
    cli::cli_abort("Repertoire de projet introuvable : {.path {project_dir}}")
  }

  app_dir <- system.file("app", package = "statlab")
  if (!nzchar(app_dir)) {
    cli::cli_abort("Interface graphique introuvable dans le package (inst/app/).")
  }

  options(statlab.app.project_dir = normalizePath(project_dir, winslash = "/"))
  shiny::runApp(app_dir, launch.browser = TRUE, host = "127.0.0.1")

  invisible(NULL)
}

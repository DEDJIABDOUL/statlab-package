# =============================================================================
# Plan {targets} d'un projet statlab.
#
# Fichier gabarit (inst/templates/_targets.R) : copie a la racine du projet
# (aux cotes de config.yml) par statlab::st_pipeline()/st_status(), avec
# "__CONFIG_PATH__" remplace par le chemin absolu du config.yml du projet.
# NE PAS EDITER LA COPIE A LA MAIN : elle est regeneree a chaque appel.
#
# La logique de chaque etape vit dans le package (R/pipeline.R, fonctions
# .pipeline_*), pas ici : ce fichier ne fait que declarer le graphe de
# dependances. Voir R/pipeline.R pour le detail du principe d'invalidation
# fine par section de configuration.
# =============================================================================

library(targets)

targets::tar_option_set(packages = "statlab")

.config_path <- "__CONFIG_PATH__"
.project_dir <- dirname(.config_path)

list(
  # --- Configuration (une cible par section, pour une invalidation fine) ----
  targets::tar_target(config_file, .config_path, format = "file"),
  targets::tar_target(config, statlab::st_validate_config(statlab::st_read_config(config_file))),

  targets::tar_target(sources_spec, config$sources),
  targets::tar_target(dictionnaire, config$dictionnaire),
  targets::tar_target(preparation_spec, config$preparation),
  targets::tar_target(analyse_spec, config$analyse),
  targets::tar_target(rendu_spec, config$rendu),
  targets::tar_target(reconciliation_spec, config$reconciliation),

  # --- config + sources -> sources -> profil -> anomalies -> audit ---------
  targets::tar_target(source_files, statlab:::.pipeline_source_paths(sources_spec, .project_dir), format = "file"),
  targets::tar_target(sources, {
    source_files
    statlab:::.pipeline_read_sources(sources_spec, .project_dir)
  }),
  targets::tar_target(profil, statlab:::.pipeline_all_profiles(sources)),
  targets::tar_target(anomalies, statlab:::.pipeline_all_anomalies(sources, profil, dictionnaire)),
  targets::tar_target(audit, {
    sources
    anomalies
    statlab:::.pipeline_run_audit(config_file, .project_dir)
  }),

  # --- config + sources -> reconciliation -> preparation -> donnees_finales -
  targets::tar_target(reconciliation, statlab:::.pipeline_run_reconciliation(sources, reconciliation_spec)),
  targets::tar_target(donnees_finales, statlab:::.pipeline_run_preparation(reconciliation, preparation_spec)),

  # --- donnees_finales -> tableau_1, comparaisons, graphiques ---------------
  targets::tar_target(tableau_1, statlab:::.pipeline_run_table1(donnees_finales, dictionnaire, analyse_spec$tableau_1, rendu_spec)),
  targets::tar_target(comparaisons, statlab:::.pipeline_run_comparaisons(donnees_finales, dictionnaire, analyse_spec$comparaisons)),
  targets::tar_target(graphiques, statlab:::.pipeline_run_graphiques(
    donnees_finales, comparaisons, dictionnaire, .project_dir,
    if (is.null(rendu_spec$declinaisons)) "ecran" else rendu_spec$declinaisons
  )),

  # --- tout -> script, attestation, rapport ---------------------------------
  targets::tar_target(script, {
    donnees_finales
    statlab::st_export_script(config, output = file.path(.project_dir, "sorties", "rapport", "analyse.R"))
  }),
  targets::tar_target(attestation, {
    donnees_finales
    statlab::st_attestation(config, output = file.path(.project_dir, "sorties", "rapport", "attestation.txt"))
  }),
  targets::tar_target(rapport, {
    tableau_1
    comparaisons
    graphiques
    script
    attestation
    statlab::st_report(config_file, output_dir = file.path(.project_dir, "sorties", "rapport"))
  })
)

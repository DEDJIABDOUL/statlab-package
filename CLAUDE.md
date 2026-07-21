# Projet — Chaîne locale d'analyse statistique pour la recherche en santé

## Nature du projet

Outil de production **local**, en R, opéré en ligne de commande par un
statisticien-prestataire. Il transforme des fichiers de données de recherche
bruts et désordonnés en rapport d'analyse publiable (Word + PDF).

Ce n'est pas une application grand public. L'utilisateur est un expert.
Priorité à la fiabilité, la reproductibilité et la transparence, pas à
l'accompagnement pédagogique.

## Principes non négociables

1. **Le fichier source n'est jamais modifié.** Toute transformation est
   appliquée en mémoire.
2. **Tout est déclaratif.** Aucune décision n'existe en dehors du fichier
   `config.yml` du projet.
3. **Le moteur ne devine jamais.** En cas d'ambiguïté, il s'arrête avec un
   message explicite plutôt que de choisir par défaut.
4. **Aucune erreur silencieuse.** Toute anomalie produit un message.
5. **Tout est local.** Aucun appel réseau à l'exécution.
6. **Traçabilité systématique.** Toute opération est journalisée.

## Conventions de code

- Package R interne nommé `statlab`, structure standard (`R/`, `inst/`,
  `tests/testthat/`, `DESCRIPTION`, `NAMESPACE`).
- Documentation avec **roxygen2** sur chaque fonction exportée.
- Tests avec **testthat** (edition 3). Chaque fonction exportée a au moins
  un test nominal et un test d'erreur.
- Validation des arguments avec **checkmate** en début de chaque fonction
  exportée.
- Messages utilisateur avec **cli** (`cli_alert_info`, `cli_alert_warning`,
  `cli_abort`). Jamais `print()`, jamais `cat()`, jamais `message()`.
- Noms de fonctions et de variables **en anglais**, snake_case.
  Messages destinés à l'opérateur et libellés de sortie **en français**.
- Préfixe `st_` sur toutes les fonctions exportées.
- Pas de `library()` dans le code du package : utiliser `pkg::fun()` et
  déclarer dans `Imports`.
- Pas de dépendance nouvelle sans nécessité. Toute dépendance externe est
  isolée derrière une fonction interne de `statlab` pour pouvoir être
  remplacée sans refonte.

## Structure de sortie d'un projet client

    projet_client/
    ├── donnees_brutes/   lecture seule, jamais écrit
    ├── config.yml        configuration du projet
    ├── sorties/
    │   ├── audit/
    │   ├── tableaux/
    │   ├── graphiques/
    │   └── rapport/
    ├── journal.log
    └── attestation.txt

## Ce qu'il ne faut pas faire

- Ne pas créer d'interface web, de serveur, d'authentification.
- Ne pas ajouter de fonctionnalité non demandée dans le prompt courant.
- Ne pas modifier du code validé par un prompt antérieur sans le signaler
  explicitement et en justifier la nécessité.
- Ne pas écrire dans `donnees_brutes/` ni dans `corpus/`.
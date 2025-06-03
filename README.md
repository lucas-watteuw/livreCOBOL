# 📚 Projet Gestion de Bibliothèque - COBOL/PostgreSQL

## 👥 Équipe
**Contributeurs :** Lucas, Sibory et Vincent

## 📝 Description du Projet

Concevoir un programme COBOL interfacé avec une base de données PostgreSQL pour gérer une bibliothèque, en mettant l'accent sur une phase de conception approfondie permettant aux groupes de définir librement les colonnes des tables, avec des relations via des clés secondaires.

## 🎯 Contexte du Projet

En tant que développeur·se COBOL, vous travaillez en équipe pour concevoir un programme de gestion de bibliothèque. La phase de conception en amont est cruciale !

### 🗃️ Tables Minimales Requises
- **LIVRES** : ISBN, titre, ID_auteur, ID_type
- **AUTEURS** : ID_auteur, nom
- **TYPES** : ID_type, catégorie
- **EMPRUNTS** : ID_emprunt, ISBN, date_emprunt

### ⚙️ Fonctionnalités du Programme

✅ **Fonctionnalités de Base :**
- Lire un fichier séquentiel (`livres-input.dat`) pour enregistrer des livres dans PostgreSQL
- Permettre l'ajout manuel de livres (via saisie ou fichier supplémentaire)

🎁 **Bonus :**
- Traiter les données pour gérer emprunts/retours
- Recherches (par auteur, type, etc.)
- Statistiques (livres par type)

🎁+ **Bonus++ :**
- Générer un rapport formaté (`*.dat`) listant :
  - Livres disponibles
  - Emprunts en cours
  - Statistiques

## 📋 Modalités Pédagogiques

### 🔄 Phases du Projet

1. **🎨 Conception en Amont**
   - Définir les colonnes des tables, leurs types et relations
   - Produire un schéma relationnel justifiant les choix
   - Inclure la gestion du fichier `livres-input.dat`

2. **💻 Développement**
   - Écrire un programme COBOL interfacé avec PostgreSQL
   - Lire et enregistrer les livres du fichier
   - Gérer l'ajout manuel de livres
   - Effectuer traitements et rapport

3. **🎤 Présentation**
   - Exposé oral de 10 minutes (7 min + 3 min questions)
   - Conception, choix techniques, défis et démonstration

### 🛠️ Verbes COBOL Requis
- **Obligatoires :** `STRING`, `MOVE`, `COMPUTE`, `ROUNDED`
- **Bonus :** `CALL`, `SORT`, `SEARCH`, requêtes SQL avancées

## 📊 Modalités d'Évaluation

### 🎯 Critères d'Évaluation
- **Conception :** Pertinence des colonnes, justification des choix, clarté du schéma
- **Fonctionnalité :** Interaction PostgreSQL, enregistrement, ajout manuel
- **Lisibilité :** Rapport clair et bien formaté
- **Présentation :** Clarté, justification, démonstration convaincante

## 📦 Livrables

### 🗂️ Dépôt GitHub
- 📊 Schéma relationnel (diagramme ou `.md`/`.pdf`)
- 💾 Fichier COBOL (`*.cbl`) fonctionnel
- 🗄️ Scripts SQL (`*.sql`) pour initialiser/tester la base
- 📄 Rapport généré (`*.dat`)
- 🎯 Support de présentation (`*.pdf` ou `*.ppt`)
- 📖 Documentation (`*.md` ou `*.pdf`)

### 🎙️ Présentation Orale
- ⏰ **Durée :** 10 minutes (7 min exposé + 3 min questions)
- 📋 **Contenu :** 
  - Schéma des tables
  - Justification des colonnes
  - Démonstration fonctionnelle
  - Défis rencontrés

## 🎯 Critères de Performance

- ✅ **Conception :** Schéma relationnel clair et colonnes justifiées
- ⚙️ **Fonctionnalité :** Interaction correcte avec PostgreSQL
- 🛡️ **Robustesse :** Gestion des cas particuliers
- 💻 **Syntaxe COBOL :** Conforme avec verbes requis
- 🎤 **Présentation :** Clarté et démonstration fonctionnelle
- 📚 **Documentation :** Claire et concise avec analyse réflexive

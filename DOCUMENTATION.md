# 📚 Système de Gestion de Bibliothèque - Documentation Technique

## 🎯 Vue d'Ensemble du Projet

Ce projet consiste en un système complet de gestion de bibliothèque développé en COBOL avec une base de données relationnelle. Le système permet la gestion des livres, auteurs, genres et emprunts à travers une interface utilisateur interactive.

---

## 👥 Répartition des Tâches

### Sibory - Architecture Base de Données
- **Responsabilité** : Conception et création des tables SQL
- **Missions** : Définition du schéma relationnel et gestion des contraintes

### Vincent - Traitement des Données
- **Responsabilité** : Lecture et transformation des données
- **Missions** : 
  - Parsing du fichier `.dat` d'entrée
  - Génération du fichier `.sql` de sortie
  - Gestion de la déduplication des données

### Lucas - Interface Utilisateur
- **Responsabilité** : Développement du programme COBOL principal
- **Missions** : Création du système CRUD avec interface menu

---

## 🏗️ Architecture du Système

### 📊 Structure de la Base de Données

#### 📖 Table `livres`
**Résumé :** 
C'est la table principale de la base de données, elle stocke toutes les informations principales de chaque livre, elle est liée aux tables via des clés étrangères. Ces clés permettent d'obtenir des informations complémentaires sur chaque livre ou de vérifier sa disponibilité.

**Colonnes :** 
- id_livres : de type NUMERIC, c'est le numéro ISBN, il s'agit également de la clé primaire de la table livres, étant donné que le numéro ISBN est unique, il permet de référencer efficacement les livres
- titre : de type VARCHAR il a une longueur de 38 qui correspond à la longueur du fichier d'enregistrement
- date_parution : de type NUMERIC, elle correspond à l'année de parution de chaque livre, la longueur est de 4 pour faciliter l'enregistrement des années
- editions : de type VARCHAR, il s'agit de la boîte d'éditions dans laquelle est sorti le livre, la longueur est de 23 ce qui correspond à la longueur dans le fichier d'enregistrement
- fk_auteur : Identifiant auteur (clé étrangère)
- fk_genre : Identifiant genre (clé étrangère)
- fk_emprunt : Identifiant emprunt (clé étrangère)

#### ✍️ Table `auteurs`
**Résumé :**
C'est une table qui gère les informations des écrivains, elle permet également via des requêtes de savoirs quels livres chaque écrivains à écrit. Une possibilité d'évolution est d'intégrer plus d'informations sur l'écrivain pour enrichir le contenu.

**Colonnes :**
- id_auteurs : de type SERIAL c'est une clé primaire auto-générée qui va s'incrémenter à chaque nouveau enregistrement, elle permet de référencer chaque auteur avec précision
- nom : c'est le nom de famille de l'auteur, de type VARCHAR et de longueur 22
- prenom : le prénom, il peut être optionnel, il est alors marqué par "-" si absent ou non renseigné, il est de type VARCHAR et de longueur 22 comme dans le fichier

#### 🏷️ Table `genres`
**Résumé :** 
Cette table catégorise les livres par type et par genre, une possibilité d'amélioration est d'intégrer des sous-genres et/ou de différencier les différents types de livres et les divers genres qui y sont rattachés.

**Colonnes :**
- id_genre : une clé primaire auto-générée de type SERIAL, elle s'auto-incrémente à chaque nouvel enregistrement, elle permet de catégoriser les livres selon leur genre ou type
- nom : le nom du genre rattaché à l'ID

#### 📚 Table `emprunts` (en cours de conception)
**Résumé :** 
La table emprunt permet de suivre la disponibilité,les emprunts et les retours des livres. Bien que non sollicitée actuellement dans la version du programme, elle permettra dans une évolution future de gérer cette feature. D'autres informations pourront également y être ajoutées comme le nom et prénom de l'emprunteur et la durée d'emprunt.

**Colonnes :**
- id_emprunt : c'est le numéro d'emprunt qui permet d'identifier les informations propres à chaque emprunt, c'est une clé primaire auto-générée de type SERIAL, elle s'auto-incrémentera à chaque nouvel enregistrement
- statut : de type BOOLEAN, c'est une colonne qui permettra de définir si le livre est disponible ou non
- date_emprunt : c'est la date d'emprunt, de type DATE, elle est renseignée dans le programme grâce à l'instruction CURRENT DATE
- date_retour : c'est la date de retour, elle est renseignée manuellement mais pourra être fixée, via une évolution future, par une durée d'emprunt, elle est de type DATE

---

## 💻 Interface Utilisateur - Menu CRUD

Le programme COBOL principal offre un menu interactif avec les fonctionnalités suivantes :

### ➕ Gestion des Livres
- **Créer** : Ajout d'un nouveau livre
- **Modifier** : Mise à jour des informations existantes
- **Supprimer** : Retrait d'un livre du système
- **Rechercher** : Consultation et affichage des livres (en cours de conception)

### 📚 Gestion des Emprunts (en cours de conception)
- **Emprunter** : Enregistrement d'un prêt
- **Retourner** : Traitement du retour d'un livre

### 📊 Fonctionnalités Avancées
- **Statistiques** : Rapports et analyses
- **Quitter** : Sortie propre du programme

---

## 📄 Spécifications du Fichier d'Entrée `.dat`

### 📋 Format des Données

| **Champ** | **Taille** | **Type COBOL** | **Description** |
|-----------|------------|----------------|-----------------|
| ISBN | 13 caractères | `PIC X(13)` | Identifiant unique du livre |
| Titre | 38 caractères | `PIC X(38)` | Titre complet de l'ouvrage |
| Nom | 22 caractères | `PIC X(22)` | Nom de famille de l'auteur |
| Prénom | 22 caractères | `PIC X(22)` | Prénom de l'auteur (ou "-") |
| Genre | 16 caractères | `PIC X(16)` | Catégorie littéraire |
| Date | 4 caractères | `PIC X(4)` | Année de publication |
| Éditeur | 23 caractères | `PIC X(23)` | Maison d'édition |

### ⚠️ Notes Importantes
- Les prénoms absents sont représentés par le caractère "-"
- Les données sont lues séquentiellement ligne par ligne
- Chaque enregistrement suit un format fixe

---

## 🔄 Processus de Traitement des Données

### 📥 Phase 1 : Lecture et Extraction
Vincent a développé un système de lecture séquentielle qui :
- Lit chaque ligne du fichier `.dat`
- Extrait les informations selon la structure définie
- Stocke les données dans des tableaux temporaires

### 🎯 Phase 2 : Déduplication et Organisation

#### Gestion des Auteurs
- **Méthode** : Tableau unidimensionnel avec recherche
- **Logique** : Vérification d'existence avant ajout
- **Résultat** : Liste unique d'auteurs avec identifiants auto-générés

#### Gestion des Genres
- **Méthode** : Tableau unidimensionnel avec recherche
- **Logique** : Élimination des doublons
- **Résultat** : Catalogue unique des genres littéraires

### 📤 Phase 3 : Génération SQL
Le programme produit un fichier `.sql` contenant :
- Instructions `CREATE TABLE` pour chaque entité (en projet)
- Commandes `INSERT` avec les données extraites
- Contraintes de clés étrangères pour maintenir l'intégrité

---

## 🔗 Relations et Intégrité des Données

### 🎲 Système d'Identifiants
- **Auteurs** : ID unique généré automatiquement
- **Genres** : ID unique généré automatiquement
- **Livres** : ISBN comme clé primaire naturelle

### 🔐 Contraintes Relationnelles
- Chaque livre est lié à un auteur via `auteur_id`
- Chaque livre appartient à un genre via `genre_id`
- Les emprunts référencent les livres via ISBN

---

## 🚀 Flux de Données Global

```
Fichier .dat → Lecture COBOL → Tableaux temporaires → Déduplication → Fichier .sql → Base de données → Interface CRUD
```

### Étapes Détaillées :
1. **Ingestion** : Lecture du fichier source
2. **Parsing** : Extraction des champs structurés
3. **Normalisation** : Création des entités distinctes
4. **Export** : Génération des scripts SQL
5. **Déploiement** : Création de la base de données
6. **Utilisation** : Interface interactive COBOL

---

## 📈 Avantages de l'Architecture

### 🎯 Efficacité
- Élimination des doublons dès la phase d'import
- Structure relationnelle optimisée
- Identifiants uniques pour des jointures rapides

### 🔒 Intégrité
- Contraintes de clés étrangères
- Validation des données à l'entrée
- Gestion cohérente des relations

### 🛠️ Maintenabilité
- Séparation claire des responsabilités
- Code modulaire et réutilisable
- Documentation technique complète

---

*Cette documentation sera enrichie au fur et à mesure de l'avancement du projet.*

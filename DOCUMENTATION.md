## 🚀 Notre Plan d'Action

### 🗃️ Structure des Tables
- 📖 Table pour livre
- ✍️ Table pour auteur

### 💻 Programme COBOL - Menu CRUD
- ➕ Créer un livre
- ❌ Supprimer un livre
- ✏️ Modifier un livre
- 🔍 Afficher / rechercher les livres
- 📚 Emprunter / retourner livre
  - 📅 Date emprunt
  - 🔄 Date retour

## 📄 Description du Fichier `.dat`

### 📋 Structure des Données
| Champ | Taille | Type COBOL | Notes |
|-------|--------|------------|-------|
| ISBN | 13 caractères | `PIC X(13)` | |
| Titre | 38 caractères | `PIC X(38)` | |
| Nom | 22 caractères | `PIC X(22)` | |
| Prénom | 22 caractères | `PIC X(22)` | ⚠️ Absence marquée par "-" |
| Genre | 16 caractères | `PIC X(16)` | |
| Date de publication | 4 caractères | `PIC X(4)` | |
| Éditeur | 23 caractères | `PIC X(23)` | |


Sibory s'est occupé de la gestion du sql et de la création des tables
Vincent s'est occupé de la lecture, de l'enregistrement des données et la création du fichier .sql
Lucas s'est occupé de la création de la partie CRUD du programme COBOL







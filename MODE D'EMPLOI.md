# 📘 Guide d'Utilisation - Système de Gestion de Bibliothèque

## 🎯 Objectif

Ce guide vous accompagne dans l'installation et l'utilisation du système de gestion de bibliothèque développé en COBOL avec base de données PostgreSQL.

---

## 📋 Prérequis

Avant de commencer, assurez-vous d'avoir installé :

- **PostgreSQL** (version 12 ou supérieure)
- **GnuCOBOL** (compilateur COBOL)
- **OCESQL** (interface COBOL-SQL)
- **Accès administrateur** à la base de données PostgreSQL

---

## 🔧 Installation et Configuration

### 📊 Étape 1 : Mise en Place de la Base de Données

#### Connexion à PostgreSQL

Connectez-vous à votre instance PostgreSQL en tant qu'administrateur :

```bash
psql -U postgres
```

*Note : Vous devrez saisir le mot de passe administrateur PostgreSQL*

#### Création de la Base de Données et des Tables

Exécutez le script de création de la structure de base de données :

```sql
\i exeBDD.sql
```

Ce script va :
- Créer la base de données `bibliotheque`
- Générer les tables : `livres`, `auteurs`, `genres`, `emprunts`
- Définir les contraintes et relations entre tables

#### Alimentation des Tables

Importez les données initiales dans la base :

```sql
\i livres-output.sql
```

Ce fichier contient :
- Les données d'auteurs dédupliquées
- La liste des genres littéraires
- L'ensemble des livres avec leurs références

#### Vérification de l'Installation

Pour vérifier que tout s'est bien passé :

```sql
-- Vérifier le nombre d'enregistrements
SELECT COUNT(*) FROM livres;
SELECT COUNT(*) FROM auteurs;
SELECT COUNT(*) FROM genres;

-- Quitter PostgreSQL
\q
```

---

## 🚀 Lancement du Programme CRUD

### 💻 Étape 2 : Compilation du Programme COBOL

#### Génération du Code Intermédiaire

Transformez le code COBOL avec OCESQL :

```bash
ocesql menu.cbl menu.cob
```

Cette commande :
- Traite les instructions SQL intégrées
- Génère le fichier `menu.cob` compilable

#### Compilation avec GnuCOBOL

Compilez le programme avec les options nécessaires :

```bash
cobc -locesql -x -v -o run menu.cob
```

**Détail des options :**
- `-locesql` : Lien avec la bibliothèque OCESQL
- `-x` : Génère un exécutable
- `-v` : Mode verbeux pour diagnostics
- `-o run` : Nom de l'exécutable de sortie

#### Exécution du Programme

Lancez l'interface CRUD :

```bash
./run
```

---

## ⚠️ Résolution des Problèmes Courants

### 🔧 Erreur de Module OCESQL

**Symptôme :**
```
libcob: erreur: module 'OCESQLConnect' not found
```

**Solution :**

1. **Localiser la bibliothèque OCESQL :**
```bash
find /usr -name "libocesql.so" 2>/dev/null
```

2. **Définir la variable d'environnement :**
```bash
export LD_PRELOAD=/usr/local/lib/libocesql.so
```

*Note : Adaptez le chemin selon votre installation*

3. **Rendre permanent (optionnel) :**
```bash
echo 'export LD_PRELOAD=/usr/local/lib/libocesql.so' >> ~/.bashrc
source ~/.bashrc
```

### 🗄️ Problèmes de Connexion Base de Données

**Vérifications à effectuer :**

1. **Service PostgreSQL actif :**
```bash
sudo systemctl status postgresql
```

2. **Paramètres de connexion :**
   - Nom de la base : `bibliotheque`
   - Utilisateur : `postgres`
   - Port : `5432` (par défaut)

3. **Permissions utilisateur :**
```sql
-- Dans psql
GRANT ALL PRIVILEGES ON DATABASE bibliotheque TO votre_utilisateur;
```

---

## 🖥️ Utilisation de l'Interface CRUD

### 📚 Menu Principal

Une fois le programme lancé, vous accédez au menu principal :

```
========================================
    SYSTÈME DE GESTION DE BIBLIOTHÈQUE
========================================

1. Ajouter un livre
2. Supprimer un livre  
3. Modifier un livre
4. Rechercher un livre
5. Statistiques (en développement)
6. Quitter

Votre choix : _
```

### ➕ Ajouter un Livre

**Procédure :**
1. Sélectionnez l'option `1`
2. Saisissez les informations demandées :
   - ISBN (13 caractères)
   - Titre du livre
   - Nom de l'auteur
   - Prénom de l'auteur (ou "-" si absent)
   - Genre littéraire
   - Année de publication
   - Éditeur

**Validation :**
Le système vérifie automatiquement :
- L'existence de l'auteur dans la base
- La validité du genre
- L'unicité de l'ISBN

### ❌ Supprimer un Livre

**Procédure :**
1. Sélectionnez l'option `2`
2. Entrez l'ISBN du livre à supprimer
3. Confirmez la suppression

**⚠️ Attention :** Cette action est irréversible

### ✏️ Modifier un Livre

**Procédure :**
1. Sélectionnez l'option `3`
2. Entrez l'ISBN du livre à modifier
3. Saisissez les nouvelles informations

**Note importante :** Pour modifier l'ISBN, vous devez :
1. Supprimer l'ancien livre
2. Créer un nouvel enregistrement

### 🔍 Rechercher un Livre

**Options disponibles :**
- **Par ISBN** : Recherche exacte (fonctionnelle)
- **Par titre** : Recherche par mot-clé (en cours de correction)
- **Par auteur** : En développement
- **Par genre** : En développement

---

## 📊 Informations Techniques

### 🔍 État des Fonctionnalités

| Fonctionnalité | Statut | Notes |
|----------------|--------|-------|
| Connexion BDD | ✅ Opérationnelle | - |
| Ajout livre | ✅ Opérationnelle | Avec validation |
| Suppression livre | ✅ Opérationnelle | - |
| Modification livre | ✅ Opérationnelle | Sauf ISBN |
| Recherche par ISBN | ✅ Opérationnelle | - |
| Recherche par titre | ⚠️ Problématique | En correction |
| Recherche par auteur/genre | ❌ Non implémentée | Planifiée |
| Système d'emprunt | ❌ Non implémentée | Future version |

### 🔄 Sauvegarde et Maintenance

**Sauvegarde des données :**
```bash
pg_dump -U postgres bibliotheque > sauvegarde_bibliotheque.sql
```

**Restauration :**
```bash
psql -U postgres -d bibliotheque < sauvegarde_bibliotheque.sql
```

---

## 📞 Support et Dépannage

### 🆘 En Cas de Problème

1. **Vérifiez les logs système**
2. **Consultez la section résolution des problèmes**
3. **Vérifiez la connexion réseau à PostgreSQL**
4. **Contrôlez les permissions de fichiers**

### 📋 Informations à Fournir en Cas de Bug

- Version du système d'exploitation
- Version de PostgreSQL
- Version de GnuCOBOL et OCESQL
- Message d'erreur exact
- Étapes pour reproduire le problème

---

## 🔮 Évolutions Futures

### 📈 Fonctionnalités Prévues

- **Système d'emprunt complet**
- **Recherche avancée multi-critères**
- **Rapports et statistiques**
- **Interface graphique**
- **Export des données**

### 🛠️ Améliorations Techniques

- **Optimisation des performances**
- **Gestion avancée des erreurs**
- **Journalisation des opérations**
- **Sauvegarde automatique**

---

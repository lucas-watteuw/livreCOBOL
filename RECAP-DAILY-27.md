## 📅 Objectifs Journaliers - MS COBOL P4

### 🗓️ Jour 1 – Mardi 3 Juin 2025

#### 📖 Définitions Clés
- **Clé primaire :** identifiant unique d'un enregistrement (ex. `ISBN` dans `LIVRES`)
- **Clé secondaire :** champ reliant deux tables (ex. `ID_auteur` dans `LIVRES`)
- **Schéma relationnel :** organisation des tables, colonnes et relations
- **EXEC SQL :** requête SQL intégrée dans COBOL
- **COMMIT / ROLLBACK :** validation ou annulation de transaction SQL

#### 🎯 Objectifs du Jour
**Conception initiale et insertion automatisée**

- [ ] Concevoir le schéma relationnel des tables (`LIVRES`, `AUTEURS`, `TYPES`...)
- [ ] Définir les colonnes principales et optionnelles (`Editeur`...)
- [ ] Justifier les choix de colonnes dans un document
- [ ] Écrire un script SQL (`init-biblio.sql`) pour créer les tables dans PostgreSQL
- [ ] Écrire un programme COBOL pour lire le fichier `livres-input.dat`
- [ ] Implémenter l'insertion des données avec `EXEC SQL INSERT`
- [ ] Gérer les transactions avec `COMMIT` / `ROLLBACK`

#### 🏋️ Exercices Pratiques
- [ ] Produire un schéma relationnel clair (texte ou diagramme)
- [ ] Créer les tables avec les bonnes clés primaires / secondaires
- [ ] Définir le `RECORD` COBOL du fichier `livres-input.dat`
- [ ] Vérifier l'existence de l'auteur / type avec `SELECT INTO` avant insertion
- [ ] Tester les insertions via `psql` et le programme COBOL

#### 💡 Conseils
- Utiliser un outil de modélisation (ex. Draw.io)
- Définir des `PIC X()` adaptés aux longueurs de champs
- Documenter les choix de conception dès le départ

#### ⚙️ Compilation COBOL
```bash
export COB_LDFLAGS=-Wl,--no-as-needed
ocesql biblio.cbl biblio.cob
cobc -locesql -x -v -o run biblio.cob
```

#### 🎁 Bonus
- [ ] Créer la table `EMPRUNTS`
- [ ] Insérer un emprunt test dans `EMPRUNTS` via `psql`
- [ ] Ajouter une contrainte `CHECK` sur `Annee_Publication`
- [ ] Valider le format `ISBN` dans le programme COBOL

---

🎯 **Bonne chance pour votre projet !** 💪
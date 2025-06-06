/*
*
*
*
*/


/*Création de la base de données "exobibli"*/
CREATE DATABASE exobibli;

/*Instruction sur POSTGRESQL afin de se placer dans la base de données, une telle manoeuvre est nécessaire pour pouvoir y insérer 
des données*/
\c exobibli;

/*L'instruction DROP TABLE sert de vérification avant la création des tables, elle permet d'éviter les doublons et donc la 
création de conflits*/
DROP TABLE IF EXISTS livres;
DROP TABLE IF EXISTS auteurs;
DROP TABLE IF EXISTS genre;
DROP TABLE IF EXISTS emprunt;

/*Instruction pour la création de la table auteurs, elle est composée de 3 colonnes. La première colonne est celle de l'ID, il
permet de rendre chaque enregistrement unique. L'ID est de type SERIAL PRIMARY KEY, c'est une clé primaire qui s'auto-incrémente
à chaque nouvel enregistrement.*/
CREATE TABLE auteurs(id_auteurs SERIAL PRIMARY KEY NOT NULL, nom VARCHAR(22), prenom VARCHAR(22)
);

/*Instruction pour la création de la table genre, elle est composée de deux colonnes. La première colonne est celle de l'ID, il
permet de rendre chaque enregistrement unique. L'ID est de type SERIAL PRIMARY KEY, c'est une clé primaire qui s'auto-incrémente 
à chaque nouvel enregistrement.*/
CREATE TABLE genre(id_genre SERIAL PRIMARY KEY NOT NULL, nom VARCHAR(16)
);


/*Instruction pour la création de la table genre, elle est composée de quatre colonnes. La première colonne est celle de l'ID, il
permet de rendre chaque enregistrement unique. L'ID est de type SERIAL PRIMARY KEY, c'est une clé primaire qui q'auto-incrémente
à chaque nouvel enregistrement*/
CREATE TABLE emprunt(id_emprunt SERIAL PRIMARY KEY NOT NULL, statut BOOLEAN, date_emprunt DATE, date_retour DATE
);

/*Instruction pour la création de la table livres, elle est composée de sept colonnes, à préciser que les trois dernières sont des
clés étrangères des autres tables. L'ID est de type NUMERIC car c'est le numéro ISBN de chaque livre. */
CREATE TABLE livres(id_livres NUMERIC(13) PRIMARY KEY NOT NULL, titre VARCHAR(38), date_parution NUMERIC(4), editions VARCHAR(23), 
fk_auteur INTEGER, fk_genre INTEGER, fk_emprunt INTEGER
);

/*Iinstruction pour la dlécaration des trois dernières colonnes de la table livres. C'est trois colonnes sont déclarées comme étant 
des clés étrangères.*/
ALTER TABLE livres ADD CONSTRAINT fk_genre FOREIGN KEY (fk_genre) REFERENCES genre(id_genre);
ALTER TABLE livres ADD CONSTRAINT fk_emprunt FOREIGN KEY (fk_emprunt) REFERENCES emprunt(id_emprunt);
ALTER TABLE livres ADD CONSTRAINT fk_auteur FOREIGN KEY (fk_auteur) REFERENCES auteurs (id_auteurs);

CREATE DATABASE exobibli;
\c exobibli;

DROP TABLE IF EXISTS livres;
DROP TABLE IF EXISTS auteurs;
DROP TABLE IF EXISTS genre;
DROP TABLE IF EXISTS emprunt;

CREATE TABLE auteurs(id_auteurs SERIAL PRIMARY KEY NOT NULL, nom VARCHAR(22), prenom VARCHAR(22)
);

CREATE TABLE genre(id_genre SERIAL PRIMARY KEY NOT NULL, nom VARCHAR(16)
);

CREATE TABLE emprunt(id_emprunt SERIAL PRIMARY KEY NOT NULL, statut BOOLEAN, date_emprunt DATE, date_retour DATE
);

CREATE TABLE livres(id_livres NUMERIC(13) PRIMARY KEY NOT NULL, titre VARCHAR(38), date_parution NUMERIC(4), editions VARCHAR(23), 
fk_auteur INTEGER, fk_genre INTEGER, fk_emprunt INTEGER
);

ALTER TABLE livres ADD CONSTRAINT fk_genre FOREIGN KEY (fk_genre) REFERENCES genre(id_genre);
ALTER TABLE livres ADD CONSTRAINT fk_emprunt FOREIGN KEY (fk_emprunt) REFERENCES emprunt(id_emprunt);
ALTER TABLE livres ADD CONSTRAINT fk_auteur FOREIGN KEY (fk_auteur) REFERENCES auteurs (id_auteurs);

CREATE DATABASE IF NOT EXISTS bibli;
USE bibli;

DROP TABLE IF EXISTS livres;
DROP TABLE IF EXISTS auteurs;
DROP TABLE IF EXISTS genre;
DROP TABLE IF EXISTS emprunt;

CREATE TABLE livres(id_livres NUMERIC(13) PRIMARY KEY NOT NULL, titre VARCHAR(38), date_parution NUMERIC(4), editions VARCHAR(23), id_genre NUMERIC);

CREATE TABLE auteurs(id_auteurs NUMERIC PRIMARY KEY NOT NULL AUTO_INCREMENT, nom VARCHAR(22), prenom VARCHAR(22), livres VARCHAR(38),
    CONSTRAINT livres FOREIGN KEY (livres) REFERENCES livres (id_livres));

CREATE TABLE genre(id_genre NUMERIC PRIMARY KEY NOT NULL AUTO_INCREMENT, nom VARCHAR(16),
    CONSTRAINT id_genre FOREIGN KEY (id_genre) REFERENCES livres (id_genre)
);

CREATE TABLE emprunt(id_emprunt NUMERIC PRIMARY KEY NOT NULL AUTO_INCREMENT, statut BOOLEAN, date_emprunt DATE, date_retour DATE,
    CONSTRAINT statut FOREIGN KEY (statut) REFERENCES livres (id_livres)
);

CREATE TABLE auteurs-livres(id_auteurs NUMERIC PRIMARY KEY NOT NULL, id_livres NUMERIC PRIMARY KEY NOT NULL, 
    CONSTRAINT id_auteurs FOREIGN KEY (id_auteurs) REFERENCES auteurs (id_auteurs), 
    CONSTRAINT id_livres FOREIGN KEY (id_livres) REFERENCES livres (id_livres));

INSERT INTO auteurs (Nom, Prenom) VALUES ('Orwell', 'George');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Lee', 'Harper');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Fitzgerald', 'F. Scott');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Tolkien', 'J.R.R.');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Dostoevsky', 'Fyodor');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Salinger', 'J.D.');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Colfer', 'Eoin');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Austen', 'Jane');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Huxley', 'Aldous');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Rowling', 'J.K.');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Coelho', 'Paulo');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Dostoevsky', 'Ivan');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Bronte', 'Charlotte');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Morrison', 'Toni');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Hurston', 'Zora Neale');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Woolf', 'Virginia');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Walker', 'Alice');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Bronte', 'Emily');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Bradbury', 'Ray');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Plath', 'Sylvia');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Flaubert', 'Gustave');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Asimov', 'Isaac');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Miura', 'Kentaro');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Hugo', 'Victor');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Stendhal', '-');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Zola', 'Emile');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Balzac', 'Honore de');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Maupassant', 'Guy de');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Gautier', 'Theophile');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Diderot', 'Denis');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Lafayette', 'Madame de');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Laclos', 'Choderlos de');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Prevost', 'Abbe');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Verne', 'Jules');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Voltaire', '-');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Corneille', 'Pierre');
INSERT INTO auteurs (Nom, Prenom) VALUES ('Racine', 'Jean');
INSERT INTO genre (Nom) VALUES ('Roman');
INSERT INTO genre (Nom) VALUES ('Fantasy');
INSERT INTO genre (Nom) VALUES ('Science-Fiction');
INSERT INTO genre (Nom) VALUES ('Fable');
INSERT INTO genre (Nom) VALUES ('Essai');
INSERT INTO genre (Nom) VALUES ('Seinen');
INSERT INTO genre (Nom) VALUES ('Conte');
INSERT INTO genre (Nom) VALUES ('Theatre');
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780141185064, '1984', 1949, 'Penguin Books', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780061120084, 'To Kill a Mockingbird', 1960, 'HarperCollins', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780446310789, 'The Great Gatsby', 1925, 'Scribner', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780547928227, 'The Hobbit', 1937, 'Houghton Mifflin', 3);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780140449266, 'Crime and Punishment', 1866, 'Penguin Classics', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780684830490, 'The Catcher in the Rye', 1951, 'Little Brown', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780786849567, 'Artemis Fowl', 2001, 'Disney Hyperion', 3);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780786817085, 'Artemis Fowl  The Arctic Incident', 2002, 'Disney Hyperion', 3);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780786819140, 'Artemis Fowl  The Eternity Code', 2003, 'Disney Hyperion', 3);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780142437247, 'Pride and Prejudice', 1813, 'Penguin Books', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780060850524, 'Brave New World', 1932, 'Harper Perennial', 5);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780439554930, 'Harry Potter Philosopher Stone', 1997, 'Bloomsbury', 3);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780062315007, 'The Alchemist', 1988, 'HarperOne', 2);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780140449136, 'The Brothers Karamazov', 1880, 'Penguin Classics', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780553213690, 'Jane Eyre', 1847, 'Penguin Books', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780679720201, 'Beloved', 1987, 'Vintage', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780060935467, 'Their Eyes Were Watching God', 1937, 'Harper Perennial', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780140186390, 'A Room of Ones Own', 1929, 'Penguin Books', 1);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780385333849, 'The Color Purple', 1982, 'Harcourt', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780141393032, 'Wuthering Heights', 1847, 'Penguin Classics', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780061122415, 'Fahrenheit 451', 1953, 'Simon & Schuster', 5);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780316769532, 'The Bell Jar', 1963, 'Harper & Row', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780140449723, 'Madame Bovary', 1857, 'Penguin Classics', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780553293357, 'Foundation', 1951, 'Bantam Books', 5);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780553294385, 'I  Robot', 1950, 'Spectra', 5);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780385177715, 'The Gods Themselves', 1972, 'Doubleday', 5);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9780618640157, 'The Lord of the Rings', 1954, 'Houghton Mifflin', 3);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782723449021, 'Berserk Tome 1', 1990, 'Glenat', 6);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782723449038, 'Berserk Tome 2', 1991, 'Glenat', 6);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782723449045, 'Berserk Tome 3', 1992, 'Glenat', 6);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782723449052, 'Berserk Tome 4', 1993, 'Glenat', 6);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782723449069, 'Berserk Tome 5', 1994, 'Glenat', 6);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070360024, 'Les Miserables', 1862, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070409341, 'Le Rouge et le Noir', 1830, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782253004226, 'Germinal', 1885, 'Le Livre de Poche', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070315009, 'Madame Bovary', 1857, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070408504, 'Le Pere Goriot', 1835, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070360536, 'Notre Dame de Paris', 1831, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782080700728, 'La Chartreuse de Parme', 1839, 'Flammarion', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070413119, 'L Education sentimentale', 1869, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782253002178, 'Bel Ami', 1885, 'Le Livre de Poche', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070382033, 'La Cousine Bette', 1846, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070368631, 'Le Capitaine Fracasse', 1863, 'Gallimard', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782253006060, 'La Fortune des Rougon', 1871, 'Le Livre de Poche', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782253083474, 'Jacques le fataliste', 1796, 'Le Livre de Poche', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782080702647, 'La Princesse de Cleves', 1678, 'Flammarion', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782080712776, 'Les Liaisons dangereuses', 1782, 'Flammarion', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782253004233, 'Manon Lescaut', 1731, 'Le Livre de Poche', 4);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782070360369, 'Voyage au centre de la Terre', 1864, 'Gallimard', 5);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782253083450, 'Candide', 1759, 'Le Livre de Poche', 0);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782253086215, 'Le Cid', 1636, 'Le Livre de Poche', 7);
INSERT INTO livres (ID_Livres, Titre, Date_parution, Editions, ID_Genre) VALUES (9782080705211, 'Phedre', 1677, 'Flammarion', 7);
CREATE DATABASE IF NOT EXISTS bibli;
USE blog;

DROP TABLE IF EXISTS Livres;
DROP TABLE IF EXISTS Auteurs;
DROP TABLE IF EXISTS Genre;
DROP TABLE IF EXISTS Emprunt;

CREATE TABLE Livres(
    ID-Livres INT(13) PRIMARY KEY NOT NULL,
    Titre VARCHAR(38),
    Date_parution INT(4),
    Editions VARCHAR(23)
)

CREATE TABLE Auteurs(
    Nom VARCHAR(22) PRIMARY KEY NOT NULL,
    Prenom VARCHAR(22),
    Livres VARCHAR(38)?
    CONSTRAINT Livres FOREIGN KEY (Livres) REFERENCES Livres (ID-Livres)
)

CREATE TABLE Genre(
    ID-Genre INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
    Nom VARCHAR(16) PRIMARY KEY NOT NULL
)

CREATE TABLE Emprunt(
    Statut BOOLEAN,
    Date_emprunt DATE,
    Date_retour DATE,
    CONSTRAINT Statut FOREIGN KEY (Statut) REFERENCES Livres (ID_Livres)
)

INSERT INTO Genre (Nom) VALUES 
('Roman'),
('Fantasy'),
('Science-Fiction'),
('Seinen'),
('Fable'),
('Essai'),
('Theatre'),
('Conte');

-- LIVRES
INSERT INTO Livres (ID_Livre, Titre, Date_parution, Editions) VALUES
(9780141185064, '1984', 1949, 'Penguin Books'),
(9780061120084, 'To Kill a Mockingbird', 1960, 'HarperCollins'),
(9780446310789, 'The Great Gatsby', 1925, 'Scribner'),
(9780547928227, 'The Hobbit', 1937, 'Houghton Mifflin'),
(9780140449266, 'Crime and Punishment', 1866, 'Penguin Classics'),
(9780684830490, 'The Catcher in the Rye', 1951, 'Little Brown'),
(9780786849567, 'Artemis Fowl', 2001, 'Disney Hyperion'),
(9780786817085, 'Artemis Fowl  The Arctic Incident', 2002, 'Disney Hyperion'),
(9780786819140, 'Artemis Fowl  The Eternity Code', 2003, 'Disney Hyperion'),
(9780142437247, 'Pride and Prejudice', 1813, 'Penguin Books'),
(9780060850524, 'Brave New World', 1932, 'Harper Perennial'),
(9780439554930, 'Harry Potter Philosopher Stone', 1997, 'Bloomsbury'),
(9780062315007, 'The Alchemist', 1988, 'HarperOne'),
(9780140449136, 'The Brothers Karamazov', 1880, 'Penguin Classics'),
(9780553213690, 'Jane Eyre', 1847, 'Penguin Books'),
(9780679720201, 'Beloved', 1987, 'Vintage'),
(9780060935467, 'Their Eyes Were Watching God', 1937, 'Harper Perennial'),
(9780140186390, 'A Room of Ones Own', 1929, 'Penguin Books'),
(9780385333849, 'The Color Purple', 1982, 'Harcourt'),
(9780141393032, 'Wuthering Heights', 1847, 'Penguin Classics'),
(9780061122415, 'Fahrenheit 451', 1953, 'Simon & Schuster'),
(9780316769532, 'The Bell Jar', 1963, 'Harper & Row'),
(9780140449723, 'Madame Bovary', 1857, 'Penguin Classics'),
(9780553293357, 'Foundation', 1951, 'Bantam Books'),
(9780553294385, 'I  Robot', 1950, 'Spectra'),
(9780385177715, 'The Gods Themselves', 1972, 'Doubleday'),
(9780618640157, 'The Lord of the Rings', 1954, 'Houghton Mifflin'),
(9782723449021, 'Berserk Tome 1', 1990, 'Glenat'),
(9782723449038, 'Berserk Tome 2', 1991, 'Glenat'),
(9782723449045, 'Berserk Tome 3', 1992, 'Glenat'),
(9782723449052, 'Berserk Tome 4', 1993, 'Glenat'),
(9782723449069, 'Berserk Tome 5', 1994, 'Glenat'),
(9782070360024, 'Les Miserables', 1862, 'Gallimard'),
(9782070409341, 'Le Rouge et le Noir', 1830, 'Gallimard'),
(9782253004226, 'Germinal', 1885, 'Le Livre de Poche'),
(9782070315009, 'Madame Bovary', 1857, 'Gallimard'),
(9782070408504, 'Le Pere Goriot', 1835, 'Gallimard'),
(9782070360536, 'Notre Dame de Paris', 1831, 'Gallimard'),
(9782080700728, 'La Chartreuse de Parme', 1839, 'Flammarion'),
(9782070413119, 'L Education sentimentale', 1869, 'Gallimard'),
(9782253002178, 'Bel Ami', 1885, 'Le Livre de Poche'),
(9782070382033, 'La Cousine Bette', 1846, 'Gallimard'),
(9782070368631, 'Le Capitaine Fracasse', 1863, 'Gallimard'),
(9782253006060, 'La Fortune des Rougon', 1871, 'Le Livre de Poche'),
(9782253083474, 'Jacques le fataliste', 1796, 'Le Livre de Poche'),
(9782080702647, 'La Princesse de Cleves', 1678, 'Flammarion'),
(9782080712776, 'Les Liaisons dangereuses', 1782, 'Flammarion'),
(9782253004233, 'Manon Lescaut', 1731, 'Le Livre de Poche'),
(9782070360369, 'Voyage au centre de la Terre', 1864, 'Gallimard'),
(9782253083450, 'Candide', 1759, 'Le Livre de Poche'),
(9782253086215, 'Le Cid', 1636, 'Le Livre de Poche'),
(9782080705211, 'Phedre', 1677, 'Flammarion');

-- AUTEURS
INSERT INTO Auteurs (Nom, Prenom, ID_Livre) VALUES
('Orwell', 'George', 9780141185064),
('Lee', 'Harper', 9780061120084),
('Fitzgerald', 'F. Scott', 9780446310789),
('Tolkien', 'J.R.R.', 9780547928227),
('Dostoevsky', 'Fyodor', 9780140449266),
('Salinger', 'J.D.', 9780684830490),
('Colfer', 'Eoin', 9780786849567),
('Colfer', 'Eoin', 9780786817085),
('Colfer', 'Eoin', 9780786819140),
('Austen', 'Jane', 9780142437247),
('Huxley', 'Aldous', 9780060850524),
('Rowling', 'J.K.', 9780439554930),
('Coelho', 'Paulo', 9780062315007),
('Dostoevsky', 'Ivan', 9780140449136),
('Bronte', 'Charlotte', 9780553213690),
('Morrison', 'Toni', 9780679720201),
('Hurston', 'Zora Neale', 9780060935467),
('Woolf', 'Virginia', 9780140186390),
('Walker', 'Alice', 9780385333849),
('Bronte', 'Emily', 9780141393032),
('Bradbury', 'Ray', 9780061122415),
('Plath', 'Sylvia', 9780316769532),
('Flaubert', 'Gustave', 9780140449723),
('Asimov', 'Isaac', 9780553293357),
('Asimov', 'Isaac', 9780553294385),
('Asimov', 'Isaac', 9780385177715),
('Tolkien', 'J.R.R.', 9780618640157),
('Miura', 'Kentaro', 9782723449021),
('Miura', 'Kentaro', 9782723449038),
('Miura', 'Kentaro', 9782723449045),
('Miura', 'Kentaro', 9782723449052),
('Miura', 'Kentaro', 9782723449069),
('Hugo', 'Victor', 9782070360024),
('Stendhal', '-', 9782070409341),
('Zola', 'Emile', 9782253004226),
('Flaubert', 'Gustave', 9782070315009),
('Balzac', 'Honore de', 9782070408504),
('Hugo', 'Victor', 9782070360536),
('Stendhal', '-', 9782080700728),
('Flaubert', 'Gustave', 9782070413119),
('Maupassant', 'Guy de', 9782253002178),
('Balzac', 'Honore de', 9782070382033),
('Gautier', 'Theophile', 9782070368631),
('Zola', 'Emile', 9782253006060),
('Diderot', 'Denis', 9782253083474),
('Lafayette', 'Madame de', 9782080702647),
('Laclos', 'Choderlos de', 9782080712776),
('Prevost', 'Abbe', 9782253004233),
('Verne', 'Jules', 9782070360369),
('Voltaire', '-', 9782253083450),
('Corneille', 'Pierre', 9782253086215),
('Racine', 'Jean', 9782080705211);


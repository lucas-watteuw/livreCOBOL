       IDENTIFICATION DIVISION.
       PROGRAM-ID. menu.
       AUTHOR. lucas & vincent & sibory.


       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      *champ de la table Livres
       01  ID_LIVRES      PIC 9(13).
       01  TITRE          PIC X(38).
       01  DATE_PARUTION  PIC X(04).
       01  EDITEUR        PIC X(23).
       01  FK_GENRE       PIC 9(13).
       01  FK_AUTEUR      PIC 9(13).
       01  FK_EMPRUNT     PIC 9(13) VALUE ZEROES.
      *pour se connecter à la database
       01  USERNAME       PIC X(30) VALUE "postgres". *> le nom de l'utilisateur pour postgres
       01  PASSWD         PIC X(30) VALUE "mdp". *> le mot de passe de l'utilisateur
       01  DBNAME         PIC X(10) VALUE "exobibli". *> le nom de la base de donnée
       EXEC SQL END DECLARE SECTION END-EXEC.
       EXEC SQL INCLUDE SQLCA END-EXEC.
      
      *la commande entre par l'utilisateur
       01 WS-COMMANDE-UTILISATEUR PIC 9.

      *la manière de chercher un livre sélectionner par l'utilisateur
       01 WS-SELECTION-LIVRE PIC 9.
      
      *le mot-clé entré par l'utilisateur pour chercher des livres dans la bibliothèque 
       01 WS-TITRE-VOULUE PIC X(5).
       01 WS-TITRE-VOULUE-CORRECTE PIC X(7). *>ce paramètre contiendra le paramètre précédent avec % au début et à la fin


       PROCEDURE DIVISION.

      *la première étape est de se connecter à la base de donnée
           DISPLAY "Connexion à la base de données...".
           
           EXEC SQL
                CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.
           
      *si on n'arrive pas à se connecter
           IF SQLCODE NOT = 0
               DISPLAY "Erreur de connexion SQLCODE: " SQLCODE
               STOP RUN
           END-IF.

      *on vient de se connecter à la base de donnée

      *le programme boucle tant que l'utilisateur le souhaite
           PERFORM UNTIL WS-COMMANDE-UTILISATEUR EQUAL 9 
      *on créer le menu
              DISPLAY "entrer 1 pour ajouter un livre"
              DISPLAY "entrer 2 pour perdre un livre"
              DISPLAY "entrer 3 pour prêter ou rendre un livre"
              DISPLAY "entrer 4 pour modifier un livre"
              DISPLAY "entrer 5 pour chercher un livre"
              DISPLAY "entrer 9 pour quitter le programme"
              ACCEPT WS-COMMANDE-UTILISATEUR

      *l'utilisateur vient d'entrer sa commande, on regarde ce qu'il veut faire
              EVALUATE WS-COMMANDE-UTILISATEUR 
              
      *on ajoute un livre
                 WHEN EQUAL 1
                    PERFORM 0100-ECRIT-LIVRE THRU 0100-FIN-ECRIT-LIVRE 
      *on perd un livre
                 WHEN EQUAL 2 
                    PERFORM 0200-PERDRE-LIVRE THRU 0200-FIN-PERDRE-LIVRE
      
      *on prête ou rend un livre
                 WHEN EQUAL 3 
                    DISPLAY "vous voulez prêter ou rendre un livre"

      *on modifie un livre
                 WHEN EQUAL 4 
                    PERFORM 0400-MODIFIER-LIVRE 
                    THRU 0400-FIN-MODIFIER-LIVRE
      *on cherche un livre
                 WHEN EQUAL 5 
                    PERFORM 0500-CHERCHER-LIVRE
                    THRU 0500-FIN-CHERCHER-LIVRE
      *on arrête le programme
                 WHEN EQUAL 9 
                    DISPLAY "vous voulez quitter la bibliothèque"
                    STOP RUN
      *si l'utilisateur entre une commande que l'on n'arrive pas à gérer
                 WHEN OTHER 
                    DISPLAY "votre commande n'a pas été comprise"

              END-EVALUATE
   
           END-PERFORM.
          
           STOP RUN.
      

       0100-ECRIT-LIVRE.

           DISPLAY "vous voulez ajouter un livre".
      *on récupère les donnés à insérer
           DISPLAY "Entrez l'id du livre(13 chiffres) : ".
           ACCEPT ID_LIVRES.
           DISPLAY "Entrez le titre du livre : ".
           ACCEPT TITRE.
           DISPLAY "Entrez la date de parution du livre : ".
           ACCEPT DATE_PARUTION.
           DISPLAY "Entrez l'éditeur du livre : ".
           ACCEPT EDITEUR.
           DISPLAY "entrer le numéro du genre".
           ACCEPT FK_GENRE.
           DISPLAY "entrer le numéro de l'auteur".
           ACCEPT FK_AUTEUR.
      *     DISPLAY "entrer 1 pour emprunt et 0 pour libre".
      *     ACCEPT FK_EMPRUNT.
      *on essaie d'insérer les données dans la table
           EXEC SQL
               INSERT INTO livres (id_livres, titre, 
               date_parution, editions, fk_auteur, fk_genre
      *        , fk_emprunt
               )
               VALUES (:ID_LIVRES, 
               :TITRE, 
               :DATE_PARUTION,
               :EDITEUR,
               :FK_GENRE,
               :FK_AUTEUR
      *         ,:FK_EMPRUNT
               )
           END-EXEC.
      *on vérifie si les données ont été insérées dans la table
           IF SQLCODE = 0
               DISPLAY "Insertion réussie."
           ELSE
               DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
               DISPLAY "vérifiez que l'auteur ou le genre exite déjà"
               DISPLAY "vérifiez si vous n'utilisez pas un ID déjà "
      -        "pris par un autre livre"
           END-IF.
               
           EXEC SQL COMMIT END-EXEC.
       0100-FIN-ECRIT-LIVRE.


       0200-PERDRE-LIVRE.

           DISPLAY "vous voulez perdre un livre".
           DISPLAY "entrer un id de livre(13 chiffres)".
           ACCEPT ID_LIVRES.

           EXEC SQL
           DELETE FROM Livres
           WHERE ID_Livres = :ID_LIVRES 
           END-EXEC.
      *on vérifie si la suppression est réussi
           IF SQLCODE = 0
              DISPLAY "suppression réussie."
           ELSE
              DISPLAY "Erreur de suppression SQLCODE: " SQLCODE
           END-IF.
                  
           EXEC SQL COMMIT END-EXEC.

       0200-FIN-PERDRE-LIVRE.


       0400-MODIFIER-LIVRE.
           DISPLAY "vous voulez modifier un livre".
           DISPLAY "entrer un id".
           ACCEPT ID_LIVRES.
           DISPLAY "entrer le nouveau titre du livre".
           ACCEPT TITRE.
           DISPLAY "entrer la date de parution du livre".
           ACCEPT DATE_PARUTION.
           DISPLAY "entrer l'éditeur".
           ACCEPT EDITEUR.
           DISPLAY "entrer le numéro du genre".
           ACCEPT FK_GENRE.
           DISPLAY "entrer le numéro de l'auteur".
           ACCEPT FK_AUTEUR.

      *on essaie de modifier une donné dans la table
           EXEC SQL
           UPDATE Livres 
           SET Titre = :TITRE, Date_Parution = :DATE_PARUTION, 
           Editions = :EDITEUR,
           fk_genre = :FK_GENRE,
           fk_auteur = :FK_AUTEUR
           WHERE ID_Livres = :ID_LIVRES
           END-EXEC.
      *on vérifie si la modification est réussi
           IF SQLCODE = 0
              DISPLAY "modification réussie."
           ELSE
              DISPLAY "Erreur de modification SQLCODE: " SQLCODE
           END-IF.
                  
           EXEC SQL COMMIT END-EXEC.
       0400-FIN-MODIFIER-LIVRE.

       0500-CHERCHER-LIVRE.
           DISPLAY "vous voulez chercher un livre".
      *on demande à l'utilisateur comment il veut chercher son livre
           DISPLAY "1 si vous connaissez l'id du livre".
           DISPLAY "2 si vous connaissez un des mots du titre".   
           ACCEPT WS-SELECTION-LIVRE.  
          
           EVALUATE WS-SELECTION-LIVRE
           WHEN EQUAL 1
              DISPLAY "quelle est l'id du livre?"
              ACCEPT ID_LIVRES
              EXEC SQL
                     SELECT Titre
                     INTO :TITRE
                     FROM Livres
                     WHERE ID_Livres = :ID_LIVRES
                 END-EXEC
      *on vérifie si la lecture est réussi
                 IF SQLCODE = 0
                     DISPLAY "lecture réussie."
                     DISPLAY "le titre est " TITRE
                 ELSE
                     DISPLAY "Erreur de lecture SQLCODE: " SQLCODE
                 END-IF
                     
                 EXEC SQL COMMIT END-EXEC

           WHEN EQUAL 2 
              DISPLAY "quelle mot-clé est dans le titre du livre?"
              ACCEPT WS-TITRE-VOULUE 
              MOVE FUNCTION CONCATENATE(
               "%", FUNCTION TRIM(WS-TITRE-VOULUE), "%") 
               TO WS-TITRE-VOULUE-CORRECTE
      *on déclare un cursor
              EXEC SQL
              DECLARE curseur CURSOR FOR
              SELECT Titre
              FROM Livres
              WHERE Titre LIKE WS-TITRE-VOULUE-CORRECTE
                 FOR READ ONLY
              END-EXEC
      *on ouvre le curseur
              EXEC SQL
                 OPEN curseur
              END-EXEC
      *on lit le curseur tant que le sqlcode n'est pas à 100 ou à -400
      *si le sqlcode est à -400, on n'arrive pas à lire la table
              PERFORM UNTIL SQLCODE = 100 OR SQLCODE = -400
                 EXEC SQL
                    FETCH curseur into :TITRE
                 END-EXEC
      *on affiche le résultat ligne par ligne
                 DISPLAY "un titre"
                 DISPLAY TITRE 
                 DISPLAY "comparer à "
                 DISPLAY WS-TITRE-VOULUE-CORRECTE
                 DISPLAY "le sqlcode est" SQLCODE
              END-PERFORM
      *on ferme le curseur
              EXEC SQL
                 CLOSE curseur
              END-EXEC
   
           WHEN OTHER 
              DISPLAY "abandon de la rechercher de livre"

           END-EVALUATE.

       0500-FIN-CHERCHER-LIVRE.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. menu.
       AUTHOR. lucas & vincent & sibory.


       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      *champ de la table Livres
       01  ID_LIVRES     PIC X(13).
       01  TITRE          PIC X(38).
       01  DATE_PARUTION  PIC X(10).
       01  EDITEUR        PIC 9(24).
      *pour se connecter à la database
       01  USERNAME       PIC X(30) VALUE "postgres". *> le nom de l'utilisateur pour postgres
       01  PASSWD         PIC X(30) VALUE "mdp". *> le mot de passe de l'utilisateur
       01  DBNAME         PIC X(10) VALUE "testdb". *> le nom de la base de donnée
       EXEC SQL END DECLARE SECTION END-EXEC.
       EXEC SQL INCLUDE SQLCA END-EXEC.
      
      *la commande entre par l'utilisateur
       01 WS-COMMANDE-UTILISATEUR PIC 9.

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
              
                 WHEN EQUAL 1
                    PERFORM 0100-ECRIT-LIVRE THRU 0100-FIN-ECRIT-LIVRE 

                 WHEN EQUAL 2 
                    PERFORM 0200-PERDRE-LIVRE THRU 0200-FIN-PERDRE-LIVRE

                 WHEN EQUAL 3 
                    DISPLAY "vous voulez prêter ou rendre un livre"

                 WHEN EQUAL 4 
                    PERFORM 0400-MODIFIER-LIVRE 
                    THRU 0400-FIN-MODIFIER-LIVRE
                 
                 WHEN EQUAL 5 
                    DISPLAY "vous voulez chercher un livre"

                 WHEN EQUAL 9 
                    DISPLAY "vous voulez quitter la bibliothèque"
                    STOP RUN

                 WHEN OTHER 
                    DISPLAY "votre commande n'a pas été comprise"

              END-EVALUATE
   
           END-PERFORM.
          
           STOP RUN.
      

       0100-ECRIT-LIVRE.

           DISPLAY "vous voulez ajouter un livre".
      *on récupère les donnés à insérer
           DISPLAY "Entrez le titre du livre : ".
           ACCEPT TITRE.
           DISPLAY "Entrez la date de parution du livre : ".
           ACCEPT DATE_PARUTION.
           DISPLAY "Entrez l'éditeur du livre : ".
           ACCEPT EDITEUR.
      *on essaie d'insérer les données dans la table
           EXEC SQL
               INSERT INTO livres (Titre, Date_Parution, Editions)
               VALUES (:TITRE, 
               :DATE_PARUTION,
               :EDITEUR)
           END-EXEC.
      *on vérifie si les données ont été insérées dans la table
           IF SQLCODE = 0
               DISPLAY "Insertion réussie."
           ELSE
               DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
           END-IF.
               
           EXEC SQL COMMIT END-EXEC.
       0100-FIN-ECRIT-LIVRE.

       0200-PERDRE-LIVRE.

           DISPLAY "vous voulez perdre un livre".
           DISPLAY "entrer un id de livre".
           ACCEPT ID_LIVRES.

           EXEC SQL
           DELETE FROM livres
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

      *on essaie de modifier une donné dans la table
           EXEC SQL
           UPDATE livres 
           SET Titre = :TITRE, Date_Parution = :DATE_PARUTION, 
           EDITIONS = :EDITEUR
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
       IDENTIFICATION DIVISION.
       PROGRAM-ID. menu.
       AUTHOR. lucas & vincent & sibory.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *la commande entre par l'utilisateur
       01 WS-COMMANDE-UTILISATEUR PIC 9.

       PROCEDURE DIVISION.

      *le programme boucle tant que l'utilisateur le souhaite
           PERFORM UNTIL WS-COMMANDE-UTILISATEUR EQUAL 9 
      *on créer le menu
              DISPLAY "entrer 1 pour ajouter un livre"
              DISPLAY "entrer 2 pour perdre un livre"
              DISPLAY "entrer 3 pour prêter un livre"
              DISPLAY "entrer 4 pour modifier un livre"
              DISPLAY "entrer 5 pour chercher un livre"
              DISPLAY "entrer 9 pour quitter le programme"
              ACCEPT WS-COMMANDE-UTILISATEUR

              EVALUATE WS-COMMANDE-UTILISATEUR 
                 WHEN EQUAL 1
                    DISPLAY "vous voulez ajouter un livre"

                 WHEN EQUAL 2 
                    DISPLAY "vous voulez perdre un livre"

                 WHEN EQUAL 3 
                    DISPLAY "vous voulez prêter un livre"

                 WHEN EQUAL 4 
                    DISPLAY "vous voulez modifier un livre"
                 
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

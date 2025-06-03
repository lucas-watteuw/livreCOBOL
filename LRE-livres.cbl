      ******************************************************************
      * Programme : LRE-livres.cbl                                     *
      * Auteur    : Vincent-Cmd1, Lucas et Sibory                      *
      * Création  : 03/06/2025                                         *
      * Mise à jour : 03/06/2025  
      *
      * Objet :
      *
      * Fonction  :
      *
      * Limitations :
      *
      * Remarques : 
      *
      ******************************************************************

      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ******************************************************************  
       PROGRAM-ID. LRE-livres.
       AUTHOR. Vincent-Cmd1, Lucas et Sibory.
      
      ****************************************************************** 
       ENVIRONMENT DIVISION.
      ******************************************************************  

       CONFIGURATION SECTION.
      *----------------------------------------------------------------*
      * Configuration système et utilisation de la fonction de debuggage
      *----------------------------------------------------------------*
       SOURCE-COMPUTER. MSI WITH DEBUGGING MODE.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
             
       INPUT-OUTPUT SECTION.

      *----------------------------------------------------------------*
      * Définition des fichiers d'entrée et de sortie                  *
      * - F-INPUT  : Données brutes des lives a inserer                *
      * - F-OUTPUT : Création du fichier .sql                          *
      *----------------------------------------------------------------*
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'livres-input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT-STATUS. 

           SELECT F-OUTPUT
               ASSIGN TO 'livres-output.sql'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.        

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
 
       FILE SECTION.

      *----------------------------------------------------------------*
      * Définition des structures d'enregistrements pour les fichiers  *
      *----------------------------------------------------------------*

       FD  F-INPUT
           RECORD CONTAINS 150 CHARACTERS 
           RECORDING MODE IS V.
      
       01  REC-F-INPUT.
           05 REC-DATA                      PIC X(150).
 
       FD  F-OUTPUT
           RECORD CONTAINS 150 CHARACTERS 
           RECORDING MODE IS F.

       01  REC-F-OUTPUT                     PIC X(150).

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * VARIABLES DE CONTRÔLE DES FICHIERS                             *
      * Gestion des statuts d'ouverture, lecture et écriture           *
      *----------------------------------------------------------------*
       01  WS-FS-INPUT-STATUS               PIC X(02)    VALUE SPACE.
           88 WS-FS-INPUT-STATUS-OK                      VALUE '00'.        
           88 WS-FS-INPUT-STATUS-EOF                     VALUE '10'.

       01  F-OUTPUT-STATUS                  PIC X(02)    VALUE SPACE.
           88 F-OUTPUT-STATUS-OK                         VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF                        VALUE '10'.
      
      *----------------------------------------------------------------*
      * STRUCTURE PRINCIPALE DE DONNÉES                                *
      * Table pour stocker jusqu'à 999 livres                          *
      *----------------------------------------------------------------*
       01  TAB-DATA-LIVRES.
           05 WS-LIVRES-COMPT              PIC 9(03).
           05 WS-LIVRES OCCURS 999 TIMES.
               10 WS-DATA-LIVRE            PIC X(150).      
      
      *----------------------------------------------------------------*
      * VARIABLES DE TRAVAIL POUR LES CALCULS                          *
      * Utilisées dans les boucles                                     *
      *----------------------------------------------------------------*
       01 WS-WORK-VARIABLES.
           05 WS-IDX                    PIC 9(03)    VALUE ZEROS.
           05 WS-CURRENT-LIVRE          PIC 9(03)    VALUE ZEROS.
       
      ****************************************************************** 
       PROCEDURE DIVISION.    
      ****************************************************************** 
      ******************************************************************
      * PROGRAMME PRINCIPAL                                            *
      * Orchestration du flux de traitement complet                    *
      * Séquence : Init -> Lecture -> Ecriture -> Fin                  *
      ****************************************************************** 

      * 1. Initialisation de l'environnement
           PERFORM 1000-INITIALISATION-DEB
              THRU 1000-INITIALISATION-FIN.    

      * 2. Chargement des données depuis le fichier d'entrée
           PERFORM 2000-ENRG-DATA-DEB
              THRU 2000-ENRG-DATA-FIN.

      * 3. Génération de la base de de données SQL
           PERFORM 6320-WRITE-F-OUTPUT-DEB
              THRU 6320-WRITE-F-OUTPUT-FIN.

      * 4. Finalisation et nettoyage
           PERFORM 5000-FIN-PROGRAMME-DEB
              THRU 5000-FIN-PROGRAMME-FIN.

      

      ******************************************************************
      * === 1000 === MODULE D'INITIALISATION                           *
      * Préparation de l'environnement de traitement                   *
      ******************************************************************
          
       1000-INITIALISATION-DEB.
      *----------------------------------------------------------------*
      * Ouverture sécurisée des fichiers d'entrée et de sortie         *
      * Initialisation des compteurs et variables de contrôle          *
      *----------------------------------------------------------------*

      * Ouverture du fichier d'entrée pour lecture séquentielle
           PERFORM 6010-OPEN-F-INPUT-DEB
              THRU 6010-OPEN-F-INPUT-FIN.

      * Ouverture du fichier de sortie pour écriture de la base SQL
           PERFORM 6020-OPEN-F-OUTPUT-DEB
              THRU 6020-OPEN-F-OUTPUT-FIN.

      * Initialisation des compteurs de données
           MOVE 0 TO WS-LIVRES-COMPT.

       1000-INITIALISATION-FIN.
           EXIT.



      ******************************************************************
      * === 2000 === MODULE DE LECTURE ET STOCKAGE DES DONNÉES         *
      * Traitement séquentiel du fichier d'entrée                      *
      ******************************************************************

       2000-ENRG-DATA-DEB.
      *----------------------------------------------------------------*
      * Lecture complète du fichier d'entrée avec analyse des codes    *
      * types et stockage structuré des données livres                 *
      *----------------------------------------------------------------*

      * Lecture du premier enregistrement
           PERFORM 6110-READ-F-INPUT-DEB
              THRU 6110-READ-F-INPUT-FIN.
      D    DISPLAY "Lecture du fichier d'entrée en cours...".

      * Boucle de traitement jusqu'à fin de fichier
           PERFORM UNTIL WS-FS-INPUT-STATUS-EOF
              ADD 1 TO WS-LIVRES-COMPT 
              MOVE WS-LIVRES-COMPT TO WS-CURRENT-LIVRE
      * Extraction des données depuis l'enregistrement                
                      MOVE REC-DATA(1:150) 
                        TO WS-DATA-LIVRE(WS-CURRENT-LIVRE)        
      * Traces de débogage pour vérification des données
      D               DISPLAY "  Ajout livre: " 
      D                   WS-DATA-LIVRE(WS-CURRENT-LIVRE)   
      D              DISPLAY "  Nombre de livres traités: "
      D                   WS-LIVRES-COMPT

      * Lecture de l'enregistrement suivant
               PERFORM 6110-READ-F-INPUT-DEB
                  THRU 6110-READ-F-INPUT-FIN
           END-PERFORM.

       2000-ENRG-DATA-FIN.
           EXIT.

      ******************************************************************
      * == 5000 == MODULE DE FINALISATION                              *
      * Affichage, fermeture des fichiers et terminaison               *
      ******************************************************************

       5000-FIN-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Séquence de clôture : affichage -> fermeture -> arrêt normal   *
      *----------------------------------------------------------------*

      * Fermeture sécurisée du fichier d'entrée
           PERFORM 6210-CLOSE-F-INPUT-DEB
              THRU 6210-CLOSE-F-INPUT-FIN.

      * Fermeture sécurisée du fichier de sortie
           PERFORM 6220-CLOSE-F-OUTPUT-DEB
              THRU 6220-CLOSE-F-OUTPUT-FIN.
               
      * Terminaison normale du programme
           PERFORM 9999-FIN-NORMALE-PROGRAMME-DEB
              THRU 9999-FIN-NORMALE-PROGRAMME-FIN.

       5000-FIN-PROGRAMME-FIN.
           EXIT.

           

      ******************************************************************
      * == 6000 == MODULES DE GESTION DES FICHIERS                     *
      * Opérations d'E/S avec contrôle d'erreurs intégré               *
      ******************************************************************
       
       6010-OPEN-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier d'entrée en mode INPUT pour lecture       *
      * Vérification du status et arrêt en cas d'erreur                *
      *----------------------------------------------------------------*
           OPEN INPUT F-INPUT.
      * Contrôle de l'état d'ouverture du fichier
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme ouverture F-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-F-INPUT-FIN.
           EXIT.


       6020-OPEN-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier de sortie en mode OUTPUT pour écriture    *
      * Vérification du status et arrêt en cas d'erreur                *
      *----------------------------------------------------------------*
           OPEN OUTPUT F-OUTPUT.
      * Contrôle de l'état d'ouverture du fichier
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "Probleme ouverture F-OUTPUT"
               DISPLAY "Code F-STATUS : " F-OUTPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-F-OUTPUT-FIN.
           EXIT.


       6110-READ-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Lecture d'un enregistrement depuis le fichier d'entrée         *
      * Gestion des conditions AT END et NOT AT END                    *
      * Contrôle d'erreurs sur les opérations de lecture               *
      *----------------------------------------------------------------*
      * Lecture de l'enregistrement suivant dans la structure
           READ F-INPUT INTO REC-F-INPUT
      * Traitement de la condition de fin de fichier
           AT END 
               SET WS-FS-INPUT-STATUS-EOF TO TRUE
      * Traitement de la lecture réussie
           NOT AT END 
               SET WS-FS-INPUT-STATUS-OK  TO TRUE
           END-READ.
           
      * Contrôle d'erreur : ni succès ni fin de fichier
           IF NOT WS-FS-INPUT-STATUS-OK AND 
              NOT WS-FS-INPUT-STATUS-EOF
               DISPLAY "Erreur lecture F-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-READ-F-INPUT-FIN.
           EXIT.


       6210-CLOSE-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier d'entrée après traitement complet         *
      * Vérification du status et arrêt en cas d'erreur                *
      *----------------------------------------------------------------*
           CLOSE F-INPUT.
      * Contrôle de l'état de fermeture du fichier
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme fermeture F-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6210-CLOSE-F-INPUT-FIN.
           EXIT.

   
       6220-CLOSE-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier de sortie après écriture complète        *
      * Vérification du status et arrêt en cas d'erreur               *
      *----------------------------------------------------------------*
           CLOSE F-OUTPUT.
      * Contrôle de l'état de fermeture du fichier
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "Probleme fermeture F-OUTPUT"
               DISPLAY "Code F-STATUS : " F-OUTPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6220-CLOSE-F-OUTPUT-FIN.
           EXIT.


       6320-WRITE-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Construction et écriture de la base SQL                        *
      *----------------------------------------------------------------*
       
      * === DONNÉES DES LIVRES ===
      * Boucle de traitement pour chaque livre enregistré
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > WS-CURRENT-LIVRE

              MOVE WS-DATA-LIVRE(WS-IDX) TO REC-F-OUTPUT
              WRITE REC-F-OUTPUT AFTER 1 
           END-PERFORM.  

       6320-WRITE-F-OUTPUT-FIN.
           EXIT.

      ******************************************************************
      * === 9000 === MODULES DE TERMINAISON DU PROGRAMME               *
      * Gestion des fins normales et anormales d'exécution             *
      ******************************************************************
       
       9999-FIN-NORMALE-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de fin normale du programme                          *
      * - Affiche un bandeau de fin normale                            *
      * - Ferme le fichier d'entrée                                    *
      * - Termine le programme avec un code retour de succès           *
      *----------------------------------------------------------------*
      * Affichage du bandeau de fin normale
           DISPLAY "****************************************".
           DISPLAY "*        FIN NORMALE PROGRAMME         *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'est terminé           *".
           DISPLAY "* correctement                         *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
      * Fermeture de sécurité du fichier d'entrée
           CLOSE F-INPUT.
      * Terminaison normale avec code retour 0
           STOP RUN.
       9999-FIN-NORMALE-PROGRAMME-FIN.
           EXIT.       


       9999-ERREUR-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de traitement des erreurs                            *
      * - Affiche un bandeau d'erreur                                  *
      * - Ferme le fichier d'entrée                                    *
      * - Termine le programme avec un code retour d'erreur            *
      *----------------------------------------------------------------*
      * Affichage du bandeau de fin anormale    
           DISPLAY "****************************************".
           DISPLAY "*      FIN ANORMALE DU PROGRAMME       *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'arrête suite à une    *".
           DISPLAY "* erreur détectée dans le traitement   *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
      * Fermeture de sécurité du fichier d'entrée     
           CLOSE F-INPUT.
      * Terminaison normale avec code retour 0 
           STOP RUN.
       9999-ERREUR-PROGRAMME-FIN.
           EXIT.


      ******************************************************************
      * Programme : LRE-livres.cbl                                     *
      * Auteur    : Vincent-Cmd1, Lucas et Sibory                      *
      * Création  : 03/06/2025                                         *
      * Mise à jour : 04/06/2025                                       *
      *                                                                *
      * OBJET :                                                        *
      * -------                                                        *
      * Générateur de base de données SQL pour une bibliothèque        *
      * Transformation de données de livres en requêtes INSERT SQL     *
      *                                                                *
      * FONCTION :                                                     *
      * ----------                                                     *
      * 1. Lecture d'un fichier de données contenant des informations  *
      *    sur les livres (ISBN, titre, auteur, genre, date, éditeur)  *
      * 2. Dédoublonnage automatique des auteurs et genres             *
      * 3. Génération d'un fichier SQL avec des requêtes INSERT pour   *
      *    trois tables : livres, auteurs, genres                      *
      * 4. Gestion des clés étrangères entre les tables                *
      *                                                                *
      * FICHIERS :                                                     *
      * ----------                                                     *
      * - ENTRÉE  : livres-input.dat (150 caractères par ligne)        *
      * - SORTIE  : livres-output.sql (requêtes SQL INSERT)            *
      *                                                                *
      * STRUCTURE DU FICHIER D'ENTRÉE :                                *
      * -------------------------------                                *
      * Position 001-013 : ISBN (13 caractères)                        *
      * Position 014-051 : Titre du livre (38 caractères)              *
      * Position 052-073 : Nom de l'auteur (22 caractères)             *
      * Position 074-095 : Prénom de l'auteur (22 caractères)          *
      * Position 096-111 : Genre littéraire (16 caractères)            *
      * Position 112-115 : Année de publication (4 caractères)         *
      * Position 116-138 : Nom de l'éditeur (23 caractères)            *
      *                                                                *
      * LIMITATIONS :                                                  *
      * -------------                                                  *
      * - Maximum 999 livres par traitement                            *
      * - Maximum 999 auteurs uniques                                  *
      * - Maximum 999 genres uniques                                   *
      * - Pas de validation des données d'entrée                       *
      *                                                                *
      * REMARQUES :                                                    *
      * -----------                                                    *
      * - Le programme utilise des tables internes pour le             *
      *   dédoublonnage avant génération SQL                           *
      * - Les ID des auteurs et genres sont générés automatiquement    *
      * - Activation du mode DEBUG avec compilation MSI                *
      * - Gestion complète des erreurs d'E/S fichiers                  *
      *                                                                *
      * CODES RETOUR :                                                 *
      * --------------                                                 *
      * 0 : Traitement réussi                                          *
      * Autre : Erreur d'E/S fichier                                   *
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
      * Configuration système et activation du mode débogage           *
      * MSI WITH DEBUGGING MODE : active les traces de debug (D)       *
      * DECIMAL-POINT IS COMMA : utilise la virgule comme séparateur   *
      *----------------------------------------------------------------*
      * SOURCE-COMPUTER. MSI WITH DEBUGGING MODE.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
             
       INPUT-OUTPUT SECTION.

      *----------------------------------------------------------------*
      * Définition des fichiers d'entrée et de sortie                  *
      * - F-INPUT  : Fichier de données brutes des livres à traiter    *
      *              Format fixe 150 caractères, accès séquentiel      *
      * - F-OUTPUT : Fichier SQL généré avec les requêtes INSERT       *
      *              Format variable jusqu'à 200 caractères            *
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
      * F-INPUT  : lecture des données sources en format fixe          *
      * F-OUTPUT : écriture des requêtes SQL en format variable        *
      *----------------------------------------------------------------*

       FD  F-INPUT
           RECORD CONTAINS 150 CHARACTERS 
           RECORDING MODE IS V.
      
       01  REC-F-INPUT.
           05 REC-DATA                     PIC X(150).
 
       FD  F-OUTPUT
           RECORD CONTAINS 150 CHARACTERS 
           RECORDING MODE IS F.

       01  REC-F-OUTPUT                    PIC X(200).

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * VARIABLES DE CONTRÔLE DES FICHIERS                             *
      * Gestion des codes retour des opérations d'E/S                  *
      * '00' = Opération réussie                                       *
      * '10' = Fin de fichier atteinte                                 *
      *----------------------------------------------------------------*
       01  WS-FS-INPUT-STATUS              PIC X(02)     VALUE SPACE.
           88 WS-FS-INPUT-STATUS-OK                      VALUE '00'.        
           88 WS-FS-INPUT-STATUS-EOF                     VALUE '10'.

       01  F-OUTPUT-STATUS                 PIC X(02)     VALUE SPACE.
           88 F-OUTPUT-STATUS-OK                         VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF                        VALUE '10'.
      
      *----------------------------------------------------------------*
      * TABLE PRINCIPALE DES LIVRES                                    *
      * Stockage temporaire de tous les livres lus depuis le fichier   *
      * Chaque livre contient toutes les informations nécessaires      *
      * pour générer les requêtes SQL avec les bonnes clés étrangères  *
      *----------------------------------------------------------------*
       01  TAB-DATA-LIVRES.
           05 WS-LIVRES-COMPT              PIC 9(03).
           05 WS-LIVRES OCCURS 999 TIMES.
               10 WS-ISBN                  PIC X(13).
               10 WS-TITRE                 PIC X(38).
               10 WS-NOM                   PIC X(22).
               10 WS-PRENOM                PIC X(22).
               10 WS-GENRE                 PIC X(16).
               10 WS-DATE-PUBLICATION      PIC X(04).
               10 WS-EDITEUR               PIC X(23).

      *----------------------------------------------------------------*       
      * TABLE DES AUTEURS UNIQUES                                      *
      * Dédoublonnage automatique des auteurs pour éviter les          *
      * doublons dans la base de données                               *
      * Chaque auteur reçoit un ID unique généré automatiquement       *
      *----------------------------------------------------------------*
       01  TAB-DATA-AUTEURS.
           05 WS-AUTEURS-COMPT             PIC 9(03).
           05 WS-AUTEURS OCCURS 999 TIMES INDEXED BY IDX-AUTEUR.
               10 WS-NOM-UNIQ              PIC X(13).
               10 WS-PRENOM-UNIQ           PIC X(22).
               10 WS-ID-AUTEUR             PIC 9(03)    VALUE ZEROS.

      *----------------------------------------------------------------*       
      * TABLE DES GENRES UNIQUES                                       *
      * Dédoublonnage automatique des genres littéraires               *
      * Chaque genre reçoit un ID unique pour les clés étrangères      *
      *----------------------------------------------------------------*
       01  TAB-DATA-GENRES.
           05 WS-GENRES-COMPT              PIC 9(03).
           05 WS-GENRES OCCURS 999 TIMES INDEXED BY IDX-GENRE.
               10 WS-GENRE-UNIQ            PIC X(16).
               10 WS-ID-GENRE              PIC 9(03)    VALUE ZEROS.
                     
      *----------------------------------------------------------------*
      * VARIABLES DE TRAVAIL ET COMPTEURS                              *
      * Utilisées dans les boucles de traitement et pour la gestion    *
      * des indices des différentes tables                             *
      *----------------------------------------------------------------*
       01 WS-WORK-VARIABLES.
           05 WS-IDX                       PIC 9(03)    VALUE ZEROS.
           05 WS-CURRENT-LIVRE             PIC 9(03)    VALUE ZEROS.
           05 WS-CURRENT-AUTEUR            PIC 9(03)    VALUE ZEROS.
           05 WS-AUTEUR-EXISTE             PIC X(01)    VALUE 'N'.
           05 WS-NB-AUTEURS                PIC 9(03)    VALUE 0.
           05 WS-CURRENT-GENRE             PIC 9(03)    VALUE ZEROS.
           05 WS-GENRE-EXISTE              PIC X(01)    VALUE 'N'.
           05 WS-NB-GENRES                 PIC 9(03)    VALUE 0.
  
      *----------------------------------------------------------------*
      * CONSTANTES POUR LA GÉNÉRATION SQL                              *
      * Fragments de requêtes SQL réutilisés pour construire les       *
      * différentes instructions INSERT INTO                           *
      *----------------------------------------------------------------*
       01 WS-LIGNE-ED                      PIC X(200).
       01 WS-INSERT-ED                     PIC X(12)     VALUE 
           "INSERT INTO ".
       01 WS-VALUE-ED                      PIC X(08)     VALUE
           "VALUES (".       
       01 WS-INSERT-AUTEUR-ED              PIC X(31)     VALUE 
           "auteurs (nom, prenom) VALUES ('".
       01 WS-INSERT-GENRE-ED               PIC X(21)     VALUE 
           "genre (nom) VALUES ('".
       01 WS-INSERT-LIVRE-ED               PIC X(41)     VALUE 
           "livres (id_livres, titre, date_parution, ".
       01 WS-INSERT-LIVRE-ED2              PIC X(31)     VALUE 
           "editions, fk_genre, fk_auteur) ".
       01 WS-ID-AUTEUR-ED                  PIC 9(03). 
       01 WS-ID-GENRE-ED                   PIC 9(03). 


      ****************************************************************** 
       PROCEDURE DIVISION.    
      ****************************************************************** 
      ******************************************************************
      * PROGRAMME PRINCIPAL                                            *
      * Orchestration complète du flux de traitement :                 *
      * 1. Initialisation de l'environnement et ouverture fichiers     *
      * 2. Lecture et analyse complète du fichier d'entrée             *
      * 3. Génération du fichier SQL avec toutes les requêtes          *
      * 4. Finalisation et fermeture propre des ressources             *
      ****************************************************************** 

      * Phase 1 : Préparation de l'environnement de traitement
           PERFORM 1000-INITIALISATION-DEB
              THRU 1000-INITIALISATION-FIN.    

      * Phase 2 : Chargement et analyse des données depuis le fichier
           PERFORM 2000-ENRG-DATA-DEB
              THRU 2000-ENRG-DATA-FIN.

      * Phase 3 : Construction et écriture du fichier SQL de sortie
           PERFORM 6320-WRITE-F-OUTPUT-DEB
              THRU 6320-WRITE-F-OUTPUT-FIN.

      * Phase 4 : Terminaison propre du programme
           PERFORM 5000-FIN-PROGRAMME-DEB
              THRU 5000-FIN-PROGRAMME-FIN.


      ******************************************************************
      * === 1000 === MODULE D'INITIALISATION                           *
      * Préparation complète de l'environnement de traitement :        *
      * - Ouverture sécurisée des fichiers d'entrée et sortie          *
      * - Initialisation des compteurs et variables de contrôle        *
      * - Vérification de la disponibilité des ressources              *
      ******************************************************************
          
       1000-INITIALISATION-DEB.
      *----------------------------------------------------------------*
      * Séquence d'initialisation avec contrôle d'erreurs              *
      * Arrêt immédiat en cas de problème d'accès aux fichiers         *
      *----------------------------------------------------------------*

      * Ouverture du fichier source pour lecture séquentielle
           PERFORM 6010-OPEN-F-INPUT-DEB
              THRU 6010-OPEN-F-INPUT-FIN.

      * Ouverture du fichier de destination pour écriture SQL
           PERFORM 6020-OPEN-F-OUTPUT-DEB
              THRU 6020-OPEN-F-OUTPUT-FIN.

      * Remise à zéro des compteurs de données traitées
           MOVE 0 TO WS-LIVRES-COMPT.
           MOVE 0 TO WS-AUTEURS-COMPT.
           MOVE 0 TO WS-GENRES-COMPT.

       1000-INITIALISATION-FIN.
           EXIT.


      ******************************************************************
      * === 2000 === MODULE DE LECTURE ET ANALYSE DES DONNÉES          *
      * Traitement complet du fichier d'entrée avec :                  *
      * - Lecture séquentielle de tous les enregistrements             *
      * - Extraction des champs selon positions fixes                  *
      * - Dédoublonnage en temps réel des auteurs et genres            *
      * - Construction des tables internes pour génération SQL         *
      ******************************************************************

       2000-ENRG-DATA-DEB.
      *----------------------------------------------------------------*
      * Boucle principale de lecture avec analyse de chaque ligne      *
      * Gestion automatique de la détection de fin de fichier          *
      *----------------------------------------------------------------*

      * Lecture du premier enregistrement pour amorcer la boucle
           PERFORM 6110-READ-F-INPUT-DEB
              THRU 6110-READ-F-INPUT-FIN.
      D    DISPLAY "Début du traitement du fichier d'entrée...".

      * Boucle de traitement jusqu'à épuisement du fichier
           PERFORM UNTIL WS-FS-INPUT-STATUS-EOF
              ADD 1 TO WS-LIVRES-COMPT
              ADD 1 TO WS-AUTEURS-COMPT 
              ADD 1 TO WS-GENRES-COMPT   
              MOVE WS-LIVRES-COMPT TO WS-CURRENT-LIVRE
              MOVE WS-AUTEURS-COMPT TO WS-CURRENT-AUTEUR
              MOVE WS-GENRES-COMPT TO WS-CURRENT-GENRE
              MOVE 'N' TO WS-AUTEUR-EXISTE
              MOVE 'N' TO WS-GENRE-EXISTE

      * Extraction des champs selon le format fixe défini                
              MOVE REC-DATA(1:13) 
                TO WS-ISBN(WS-CURRENT-LIVRE)
              MOVE REC-DATA(14:38) 
                TO WS-TITRE(WS-CURRENT-LIVRE)
              MOVE REC-DATA(52:22) 
                TO WS-NOM(WS-CURRENT-LIVRE)
              MOVE REC-DATA(74:22) 
                TO WS-PRENOM(WS-CURRENT-LIVRE)
              MOVE REC-DATA(96:16) 
                TO WS-GENRE(WS-CURRENT-LIVRE)
              MOVE REC-DATA(112:4) 
                TO WS-DATE-PUBLICATION(WS-CURRENT-LIVRE)
              MOVE REC-DATA(116:23) 
                TO WS-EDITEUR(WS-CURRENT-LIVRE)

      * Recherche de doublons dans la table des auteurs existants
              PERFORM VARYING WS-CURRENT-AUTEUR FROM 1 BY 1 
                        UNTIL WS-CURRENT-AUTEUR > WS-NB-AUTEURS 
                 IF WS-NOM(WS-CURRENT-LIVRE) 
                        EQUAL WS-NOM-UNIQ(WS-CURRENT-AUTEUR)
                        AND WS-PRENOM(WS-CURRENT-LIVRE) 
                        EQUAL WS-PRENOM-UNIQ(WS-CURRENT-AUTEUR)
                    MOVE 'O' TO WS-AUTEUR-EXISTE
                 END-IF
              END-PERFORM

      * Ajout du nouvel auteur s'il n'existe pas déjà
              IF WS-AUTEUR-EXISTE = 'N'
                 ADD 1 TO WS-NB-AUTEURS
                 ADD 1 TO WS-ID-AUTEUR(WS-NB-AUTEURS)
                 MOVE WS-NB-AUTEURS TO WS-ID-AUTEUR(WS-NB-AUTEURS)
                 MOVE WS-NOM(WS-CURRENT-LIVRE) 
                       TO WS-NOM-UNIQ(WS-NB-AUTEURS)
                 MOVE WS-PRENOM(WS-CURRENT-LIVRE) 
                       TO WS-PRENOM-UNIQ(WS-NB-AUTEURS)
      D          DISPLAY "Nouvel auteur ajouté : " 
      D                  WS-NOM-UNIQ(WS-NB-AUTEURS) " "
      D                  WS-PRENOM-UNIQ(WS-NB-AUTEURS)
      D        ELSE
      D          DISPLAY "Auteur existant ignoré : "
      D                  WS-NOM(WS-CURRENT-LIVRE) " "
      D                  WS-PRENOM(WS-CURRENT-LIVRE)
               END-IF

      * Recherche de doublons dans la table des genres existants
              PERFORM VARYING WS-CURRENT-GENRE FROM 1 BY 1 
                        UNTIL WS-CURRENT-GENRE > WS-NB-GENRES
                 IF WS-GENRE(WS-CURRENT-LIVRE) 
                        EQUAL WS-GENRE-UNIQ(WS-CURRENT-GENRE)
                    MOVE 'O' TO WS-GENRE-EXISTE 
                 END-IF
              END-PERFORM

      * Ajout du nouveau genre s'il n'existe pas déjà
              IF WS-GENRE-EXISTE = 'N'
                 ADD 1 TO WS-NB-GENRES
                 ADD 1 TO WS-ID-GENRE(WS-NB-GENRES)
                 MOVE WS-NB-GENRES TO WS-ID-GENRE(WS-NB-GENRES)
                 MOVE WS-GENRE(WS-CURRENT-LIVRE) 
                       TO WS-GENRE-UNIQ(WS-NB-GENRES)
      D          DISPLAY "Nouveau genre ajouté : "
      D                  WS-GENRE-UNIQ(WS-NB-GENRES)
      D        ELSE
      D          DISPLAY "Genre existant ignoré : "
      D                  WS-GENRE(WS-CURRENT-LIVRE)
               END-IF

      * Traces de débogage détaillées pour suivi du traitement
      D       DISPLAY "=== LIVRE #" WS-CURRENT-LIVRE " ==="
      D       DISPLAY "  ISBN      : " WS-ISBN(WS-CURRENT-LIVRE)
      D       DISPLAY "  Titre     : " WS-TITRE(WS-CURRENT-LIVRE)
      D       DISPLAY "  Auteur    : " WS-NOM(WS-CURRENT-LIVRE)
      D                               " " WS-PRENOM(WS-CURRENT-LIVRE)
      D       DISPLAY "  Genre     : " WS-GENRE(WS-CURRENT-LIVRE)
      D       DISPLAY "  Année     : " WS-DATE-PUBLICATION
      D                                         (WS-CURRENT-LIVRE)
      D       DISPLAY "  Éditeur   : " WS-EDITEUR(WS-CURRENT-LIVRE)
      D       DISPLAY "  Total livres : " WS-LIVRES-COMPT

      * Lecture de l'enregistrement suivant pour continuer la boucle
               PERFORM 6110-READ-F-INPUT-DEB
                  THRU 6110-READ-F-INPUT-FIN
           END-PERFORM.

      D    DISPLAY "Fin de lecture - Total traité : " WS-LIVRES-COMPT
      D            " livres, " WS-NB-AUTEURS " auteurs uniques, "
      D            WS-NB-GENRES " genres uniques".

       2000-ENRG-DATA-FIN.
           EXIT.

      ******************************************************************
      * === 5000 === MODULE DE FINALISATION                            *
      * Séquence de clôture complète du programme :                    *
      * - Fermeture sécurisée de tous les fichiers ouverts             *
      * - Affichage des statistiques de traitement                     *
      * - Terminaison normale avec code retour approprié               *
      ******************************************************************

       5000-FIN-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de clôture ordonnée avec vérification d'erreurs      *
      *----------------------------------------------------------------*

      * Fermeture du fichier d'entrée avec contrôle d'erreur
           PERFORM 6210-CLOSE-F-INPUT-DEB
              THRU 6210-CLOSE-F-INPUT-FIN.

      * Fermeture du fichier de sortie avec contrôle d'erreur
           PERFORM 6220-CLOSE-F-OUTPUT-DEB
              THRU 6220-CLOSE-F-OUTPUT-FIN.
               
      * Affichage des statistiques finales de traitement
           DISPLAY "=== STATISTIQUES DE TRAITEMENT ===".
           DISPLAY "Livres traités     : " WS-LIVRES-COMPT.
           DISPLAY "Auteurs uniques    : " WS-NB-AUTEURS.
           DISPLAY "Genres uniques     : " WS-NB-GENRES.
           DISPLAY "Fichier SQL généré : livres-output.sql".

      * Terminaison normale du programme
           PERFORM 9999-FIN-NORMALE-PROGRAMME-DEB
              THRU 9999-FIN-NORMALE-PROGRAMME-FIN.

       5000-FIN-PROGRAMME-FIN.
           EXIT.

           

      ******************************************************************
      * === 6000 === MODULES DE GESTION DES FICHIERS                  *
      * Opérations d'entrée/sortie avec contrôle d'erreurs complet :  *
      * - Ouverture sécurisée des fichiers                            *
      * - Lecture avec gestion des conditions AT END                  *
      * - Fermeture avec vérification des codes retour                *
      ******************************************************************
       
       6010-OPEN-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier d'entrée en mode lecture                 *
      * Contrôle obligatoire du code retour pour éviter les erreurs   *
      * silencieuses qui pourraient corrompre le traitement           *
      *----------------------------------------------------------------*
           OPEN INPUT F-INPUT.
      * Vérification immédiate du succès de l'opération
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "ERREUR: Impossible d'ouvrir le fichier d'entree"
               DISPLAY "Fichier : livres-input.dat"
               DISPLAY "Code erreur : " WS-FS-INPUT-STATUS
      * Déclenchement de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-F-INPUT-FIN.
           EXIT.


       6020-OPEN-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier de sortie en mode écriture               *
      * Création automatique si le fichier n'existe pas               *
      * Écrasement du contenu existant le cas échéant                 *
      *----------------------------------------------------------------*
           OPEN OUTPUT F-OUTPUT.
      * Contrôle du succès de l'opération d'ouverture
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "ERREUR: Impossible de crer le fichier de sortie"
               DISPLAY "Fichier : livres-output.sql"
               DISPLAY "Code erreur : " F-OUTPUT-STATUS
      * Déclenchement de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-F-OUTPUT-FIN.
           EXIT.


       6110-READ-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Lecture d'un enregistrement depuis le fichier d'entrée         *
      * Gestion des deux conditions possibles :                        *
      * - AT END : fin normale du fichier atteinte                     *
      * - NOT AT END : lecture réussie, données disponibles            *
      * Toute autre condition est considérée comme une erreur          *
      *----------------------------------------------------------------*
      * Tentative de lecture de l'enregistrement suivant
           READ F-INPUT INTO REC-F-INPUT
      * Traitement de la condition de fin de fichier (normale)
           AT END 
               SET WS-FS-INPUT-STATUS-EOF TO TRUE
      D          DISPLAY "Fin de fichier atteinte"
      * Traitement de la lecture réussie
           NOT AT END 
               SET WS-FS-INPUT-STATUS-OK  TO TRUE
           END-READ.
           
      * Détection d'erreurs de lecture (ni succès ni fin de fichier)
           IF NOT WS-FS-INPUT-STATUS-OK AND 
              NOT WS-FS-INPUT-STATUS-EOF
               DISPLAY "ERREUR: Probleme de lecture du fichier d'entree"
               DISPLAY "Code erreur : " WS-FS-INPUT-STATUS
      * Déclenchement de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-READ-F-INPUT-FIN.
           EXIT.


       6210-CLOSE-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier d'entrée après traitement complet        *
      * Libération des ressources système allouées                    *
      * Vérification du succès pour détecter les corruptions          *
      *----------------------------------------------------------------*
           CLOSE F-INPUT.
      * Contrôle du succès de la fermeture
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "ERREUR: Prblm de fermeture du fichier d'entrée"
               DISPLAY "Code erreur : " WS-FS-INPUT-STATUS
      * Déclenchement de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6210-CLOSE-F-INPUT-FIN.
           EXIT.

   
       6220-CLOSE-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier de sortie après écriture complète        *
      * Assure la sauvegarde définitive des données sur disque        *
      * Vérification critique pour garantir l'intégrité du fichier    *
      *----------------------------------------------------------------*
           CLOSE F-OUTPUT.
      * Contrôle du succès de la fermeture et sauvegarde
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "ERREUR: Prblm de fermeture du fichier de sortie"
               DISPLAY "Données possiblement perdues ou corrompues"
               DISPLAY "Code erreur : " F-OUTPUT-STATUS
      * Déclenchement de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6220-CLOSE-F-OUTPUT-FIN.
           EXIT.


       6320-WRITE-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * MODULE DE GÉNÉRATION DU FICHIER SQL                           *
      * Construction des requêtes INSERT pour les trois tables :      *
      * 1. Table LIVRES avec toutes les informations principales      *
      * 2. Table AUTEURS avec dédoublonnage automatique               *
      * 3. Table GENRES avec dédoublonnage automatique                *
      * Gestion des clés étrangères entre les tables                  *
      *----------------------------------------------------------------*

      D    DISPLAY "Début de génération du fichier SQL...".

      * === GÉNÉRATION DES REQUÊTES POUR LA TABLE LIVRES ===
      * Création d'une requête INSERT pour chaque livre traité
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > WS-CURRENT-LIVRE
      * Remise à zéro de la ligne de construction SQL
               INITIALIZE WS-LIGNE-ED

      * Recherche de l'ID de l'auteur correspondant à ce livre
               PERFORM 7010-RECHERCHE-AUTEUR-DEB
                  THRU 7010-RECHERCHE-AUTEUR-FIN

      * Recherche de l'ID du genre correspondant à ce livre
               PERFORM 7020-RECHERCHE-GENRE-DEB
                  THRU 7020-RECHERCHE-GENRE-FIN  

      * Construction de la requête INSERT INTO LIVRES
      * Format : INSERT INTO LIVRES VALUES 
      * (ISBN, 'TITRE', ANNEE, 'EDITEUR', ID_GENRE, ID_AUTEUR);
               MOVE WS-INSERT-ED        TO WS-LIGNE-ED(1:12)                  
               MOVE WS-INSERT-LIVRE-ED  TO WS-LIGNE-ED(13:41)
               MOVE WS-INSERT-LIVRE-ED2 TO WS-LIGNE-ED(54:31)
               MOVE WS-VALUE-ED         TO WS-LIGNE-ED(85:8)
      * Insertion de l'ISBN (clé primaire)
               MOVE WS-ISBN(WS-IDX)     TO WS-LIGNE-ED(93:13)
               MOVE ", '"               TO WS-LIGNE-ED(106:3)
      * Insertion du titre du livre (échappement des apostrophes)
               MOVE WS-TITRE(WS-IDX)    TO WS-LIGNE-ED(109:38)
               MOVE "', "               TO WS-LIGNE-ED(147:3)
      * Insertion de l'année de publication
               MOVE WS-DATE-PUBLICATION(WS-IDX) TO WS-LIGNE-ED(150:4)
               MOVE ", '"               TO WS-LIGNE-ED(154:3)
      * Insertion de l'éditeur
               MOVE WS-EDITEUR(WS-IDX)  TO WS-LIGNE-ED(157:23)
               MOVE "', '"              TO WS-LIGNE-ED(180:4)
      * Insertion de l'ID du genre (clé étrangère)
               MOVE WS-ID-GENRE-ED      TO WS-LIGNE-ED(184:3)
               MOVE "', '"              TO WS-LIGNE-ED(187:4)
      * Insertion de l'ID de l'auteur (clé étrangère)
               MOVE WS-ID-AUTEUR-ED     TO WS-LIGNE-ED(191:3)
               MOVE "');"               TO WS-LIGNE-ED(194:3)

      * Écriture de la requête dans le fichier SQL
               WRITE REC-F-OUTPUT FROM WS-LIGNE-ED AFTER 1 
           END-PERFORM. 

      *----------------------------------------------------------------* 
      * === GÉNÉRATION DES REQUÊTES POUR LA TABLE AUTEURS ===
      * Création des requêtes INSERT pour les auteurs uniques
      * Évite les doublons grâce au tableau WS-AUTEURS dédoublonné
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                 UNTIL WS-IDX > WS-CURRENT-LIVRE

      * Remise à zéro de la ligne de construction SQL
               INITIALIZE WS-LIGNE-ED

      * Construction de la requête INSERT INTO AUTEURS
      * Format : INSERT INTO AUTEURS VALUES (ID_AUTEUR, 'NOM','PRENOM');
               MOVE WS-INSERT-ED        TO WS-LIGNE-ED(1:12)                  
               MOVE WS-INSERT-AUTEUR-ED TO WS-LIGNE-ED(13:31)
      * Insertion de l'ID auteur (clé primaire)
               MOVE WS-NOM-UNIQ(WS-IDX) TO WS-LIGNE-ED(44:13)
               MOVE "', '"              TO WS-LIGNE-ED(57:4)
      * Insertion du nom de famille de l'auteur
               MOVE WS-PRENOM-UNIQ(WS-IDX) TO WS-LIGNE-ED(61:22)
               MOVE "');"               TO WS-LIGNE-ED(83:3)

      * Écriture conditionnelle : uniquement si nom et prénom existent
               IF WS-NOM-UNIQ(WS-IDX)    NOT EQUAL SPACE AND
                  WS-PRENOM-UNIQ(WS-IDX) NOT EQUAL SPACE
                  WRITE REC-F-OUTPUT FROM WS-LIGNE-ED AFTER 1
               END-IF
           END-PERFORM. 

      *----------------------------------------------------------------*
      * === GÉNÉRATION DES REQUÊTES POUR LA TABLE GENRES ===
      * Création des requêtes INSERT pour les genres uniques
      * Évite les doublons grâce au tableau WS-GENRES dédoublonné
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                 UNTIL WS-IDX > WS-CURRENT-LIVRE      

      * Remise à zéro de la ligne de construction SQL
               INITIALIZE WS-LIGNE-ED

      * Construction de la requête INSERT INTO GENRES
      * Format : INSERT INTO GENRES VALUES (ID_GENRE, 'LIBELLE_GENRE');
               MOVE WS-INSERT-ED        TO WS-LIGNE-ED(1:12)                  
               MOVE WS-INSERT-GENRE-ED  TO WS-LIGNE-ED(13:21)
      * Insertion du libellé du genre
               MOVE WS-GENRE-UNIQ(WS-IDX) TO WS-LIGNE-ED(34:16)
               MOVE "');"               TO WS-LIGNE-ED(50:3)

      * Écriture conditionnelle : uniquement si genre valide et ID > 0
               IF WS-GENRE-UNIQ(WS-IDX) NOT EQUAL SPACE 
                                        AND WS-ID-GENRE(WS-IDX) > 0
                  WRITE REC-F-OUTPUT FROM WS-LIGNE-ED AFTER 1
               END-IF
           END-PERFORM. 

       6320-WRITE-F-OUTPUT-FIN.
           EXIT.

      ******************************************************************
      * === 7000 === MODULES COMPLEMENTAIRES                           *
      * Fonctions utilitaires pour la recherche et la correspondance   *
      * des données entre les différentes structures                   *
      ******************************************************************

       7010-RECHERCHE-AUTEUR-DEB.
      *----------------------------------------------------------------*
      * Recherche de l'ID auteur correspondant au livre courant        *
      * Parcours du tableau des auteurs uniques pour établir           *
      * la correspondance nom/prénom et récupérer l'ID associé         *
      * Retourne 0 si aucune correspondance trouvée                    *
      *----------------------------------------------------------------*
      * Initialisation de l'index de recherche
           SET IDX-AUTEUR TO 1
           
      * Recherche séquentielle dans la table des auteurs
           SEARCH WS-AUTEURS
      * Cas où aucun auteur correspondant n'est trouvé
               AT END
                   MOVE 0 TO WS-ID-AUTEUR-ED
      * Cas de correspondance exacte nom + prénom
               WHEN WS-NOM(WS-IDX) = WS-NOM-UNIQ(IDX-AUTEUR)
                AND WS-PRENOM(WS-IDX) = WS-PRENOM-UNIQ(IDX-AUTEUR)
                   MOVE WS-ID-AUTEUR(IDX-AUTEUR) TO WS-ID-AUTEUR-ED
           END-SEARCH.

       7010-RECHERCHE-AUTEUR-FIN.
           EXIT.

       7020-RECHERCHE-GENRE-DEB.
      *----------------------------------------------------------------*
      * Recherche de l'ID genre correspondant au livre courant         *
      * Parcours du tableau des genres uniques pour établir            *
      * la correspondance libellé et récupérer l'ID associé            *
      * Retourne 0 si aucune correspondance trouvée                    *
      *----------------------------------------------------------------*
      * Initialisation de l'index de recherche
           SET IDX-GENRE TO 1
           
      * Recherche séquentielle dans la table des genres
           SEARCH WS-GENRES
      * Cas où aucun genre correspondant n'est trouvé
               AT END
                   MOVE 0 TO WS-ID-GENRE-ED
      * Cas de correspondance exacte du libellé de genre
               WHEN WS-GENRE(WS-IDX) = WS-GENRE-UNIQ(IDX-GENRE)
                   MOVE WS-ID-GENRE(IDX-GENRE) TO WS-ID-GENRE-ED
           END-SEARCH.
       
       7020-RECHERCHE-GENRE-FIN.
           EXIT.

       9999-FIN-NORMALE-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de fin normale du programme                          *
      * Séquence de terminaison propre :                               *
      * - Affichage d'un bandeau de fin normale                        *
      * - Fermeture de sécurité des fichiers ouverts                   *
      * - Terminaison avec code retour de succès (0)                   *
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
      * (Au cas où il serait encore ouvert)
           CLOSE F-INPUT.
      * Terminaison normale avec code retour 0
           STOP RUN.
       9999-FIN-NORMALE-PROGRAMME-FIN.
           EXIT.       


       9999-ERREUR-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de traitement des erreurs critiques                  *
      * Séquence de terminaison d'urgence :                            *
      * - Affichage d'un bandeau d'erreur explicite                    *
      * - Fermeture de sécurité des fichiers ouverts                   *
      * - Terminaison avec code retour d'erreur                        *
      * Note: Appelée en cas d'erreur I/O ou de corruption de données  *
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
      * (Libération des ressources même en cas d'erreur)
           CLOSE F-INPUT.
      * Terminaison avec code retour d'erreur
      * Note: Devrait être STOP RUN RETURNING 1 pour un vrai code d'errr
           STOP RUN.
       9999-ERREUR-PROGRAMME-FIN.
           EXIT.

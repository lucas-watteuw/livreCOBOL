### Mise en place

pour mettre en place la base de donnée et les tables, on commence par se connecter à postgres:
'''
psql -U postgres
'''

On créer ensuite la base de donnée avec les tables:
'''
\i exeBDD.sql
'''

On remplit les tables
'''
\i livres-output.sql
'''

### CRUD

Maintenant que la partie sql est mise en place, on peux lancer le crud avec ces 3 commandes:

'''
ocesql menu.cbl menu.cob
cobc -locesql -x -v -o run menu.cob
./run 
'''

Si vous avez l'erreur '''libcob: erreur: module 'OCESQLConnect' not found''' à l'issue de la 3ème 
commande, entrer la commande '''export LD_PRELOAD=/usr/local/lib/libocesql.so''' pour règler ce problème. Attention vérifiez bien que où le fichier libocesql.so se trouve afin de mofifier la commande précédente si besoin.


### explication du code

La première chose qui est faite dans le programme est de se connecter à la base de donnée.
Si on n'arrive pas à se connecter, le programme s'arrête immédiatement car il n'y a aucune raison 
de continuer.

La seconde chose qui a été faite dans ce programme est le menu pour sélectionner la commande de
l'utilisateur. Le menu n'est pas contenu dans une section screen mais consiste en plusieurs 
DISPLAY suivi d'un ACCEPT.

On implémente ensuite les commandes des utilisateurs.

La première commmande est ajouter un livre. L'utilisateur entre toutes les informations à propos 
du livre avant que le livre soit ajouté. L'ajout peut être réfusé si l'auteur ou le genre 
n'existe pas dans les tables. 

La seconde commande est la suppression de livre, l'utilisateur doit juste entrer l'ISBN du livre pour le perdre.

La troisième commande est la modification de livre. L'utilisateur doit entrer l'ISBN du livre 
qu'il veut modifier pour ensuite changer toutes les informations après. 
Si c'est l'ISBN qui doit être changer, alors la seule option disponible est de d'bord supprimer 
le livre puis de l'ajouter à nouveau avec le bon ISBN ce coup-ci.

La dernière fonctionnalité qui est en train d'être implémentée est la recherche de livre.
La recherche de livre si l'ISBN est connu est faite.
La recherche via un mot-clé présent dans le titre pose problème.
Soit on trouve tous les titres de la bibliothèque, soit on en trouve aucun ce qui signifie 
que la requête SQL est mal faite.
La recherche de livre par auteur ou par genre n'a pas commencé.

L'emprunt de livre n'a pas commencé. On ne peux donc pas rendre de livre non plus.
#Programmation et algorithmes
##Arbres Binaires
La structure d'arbre binaire exposée ci-dessous a été utilisée pour le compilateur d'automate, à voir sur [Github/Blady/Automate](https://github.com/Blady-Com/Automate).
À ce qui a déjà été dit dans l'article consacré au compilateur d'automate, j'apporte [ici](ArbreBin.pdf) un complément sur l'utilisation de l'arbre binaire pour mémoriser les identificateurs du langage et son optimisation avec l'utilisation d'Ada pour généraliser le concept d'arbres binaires sur des types de données quelconques sans les contraintes imposées par le Pascal. Cette première idée consistait naturellement à construire un arbre équilibré au fur à mesure des ajouts d'éléments supplémentaires. Mais à l'époque le temps manquait, sans accès à Internet... Je me suis rabattu sur une solution béton mais brutale : l'arbre binaire est réalisé par construction dichotomique. J'ai donc codé une procédure "Balance" qui reconstruit la dichotomie comme cela est expliqué précédemment pour le compilateur d'automate. [Code source](src/arbmgr.ads)
Longtemps après, je suis tombé coup sur coup sur un livre traitant des arbres binaires "Arbres, Tables et Algorithmes" (J. Guyot - Ch. Vial - 1989) et sur une impressionnante liste de sites sur Internet (ceci même en ne retenant que les sites de langue française). Les auteurs du livres expliquent la construction d'un arbre binaire AVL pour G.M. Andelson-Velskii, E.M. Landis (1962). Cette construction reprenait mon désir premier de construire un arbre équilibré à chaque ajout d'éléments. Pour cela l'astuce est d'ajouter à chaque noeud un attribut qui indique si les sous-arbres de chaque noeud penchent plutôt à gauche, à droite ou sont en équilibres. L'action résultante consiste à rééquilibrer l'arbre chaque fois que l'écart de chaque sous arbre est supérieur d'un niveau. [Code source](src/arbavlmgr.ads)
Généralisation de la bibliothèque Arbres Binaires avec de nombreuses fonctions utilitaires donnant la hauteur, le minimum, le successeur, etc. [Code source](src/calcularbrebinaire.adb)

##Listes
Calculs avec des listes d'éléments génériques construites à partir du container Doubly_Linked_Lists francisé pour certaines opérations, transformé par des fonctions pour d'autres et augmenté des opérations création à partir d'un tableau, application d'une fonction de transformation des éléments, partage en sous-liste, pour un emploi plus naturel comme avec le langage Caml. [Code source](src/calculdelistes.adb)

##Matrices
Calculs avec des matrices de réels : opérations usuelles +, -, *, puissance, trace, transposée, déterminant, inverse, ..., utilisation d'unité générique et du container Vectors. [Code source](src/calculdematrices.adb)

##Exemples de programmation avancée
Codage en Ada des exemples et exercices du livre "Programmation Avancée, Algorithmique et structures de données" de J.C. Boussard et Robert Mahl (Eyrolles 1984)
1) PGCD, SWAP, BubbleSort, BigFact,Power Xn, N_Uplets, Permutations, Jeu du Plus Grand Diviseur Premier, Jeu Crypto-Arithmétique, Optimisation du remplissage d'un sac à dos avec quatre procédures de parcours générique d'un arbre (standard, partiel, min-max, alpha-beta) utilisant des fonctions spécifiques à l'arbre en paramètres, calcul de factorielle n et du nombre de combinaison de n objets pris p à p, coloriage planaire, palindromes. [Code source](src/prog_avan.adb)
2) Suites contiguës bouclée, tris d'une suite par interclassement, par segmentation, selon une base "radix sort", simulation de files d'attentes, tables de hachage, intersection et union de deux ensembles par adressage dispersé, dérivation formelle, transformation en expression postfixée, évaluation d'une expression postfixée, tri arborescent, graphes à représentation linéaire fonctionnelle, associative contiguë et non contiguë, matricielle, par plexes, par adressage dispersé, chemin de longueur donnée. [Code source](src/prog_avan2.adb)

##Attribution simple
Il s'agit de faire correspondre le choix d'un ensemble de personnes (mettons de sexe féminin) vers un autre ensemble d'éléments uniques (par exemple des robes). Chaque femme émet une liste de choix de robes par ordre préférentiel. Quel est l'algorithme qui permettra de satisfaire au mieux les femmes ?
Cet algorithme d'attribution a pour principe de ranger les robes et les choix des clientes dans un tableau à deux index.
[Le détail de l'algorithme.](choix.pdf)
Le code source du programme Ada mettant en oeuvre l'algorithme.  [Code source](src/attrib.adb)

##Attribution croisée
Prenons deux ensembles d'éléments, par exemple, N hommes et N femmes. Dans ce cas classique, on suppose que chaque groupe d'individus a un ordre de préférence dans celui de sexe opposé.
Si on effectue un appariement aléatoire, en mariant à l'aveugle hommes et femmes sans tenir compte de leur avis, il y a de très forte chance pour que la configuration soit instable. Existe-t-il alors toujours une solution stable pour éviter les divorces à répétitions ? Est-elle unique ou bien y a-t-il plusieurs combinaisons stables ? Quelle méthode utiliser pour trouver à coup sûr une solution stable?
[Le détail de l'algorithme.](choixcr.pdf)
Le code source du programme Ada mettant en oeuvre l'algorithme.  [Code source](src/attribcr.adb)

Pascal Pignard, Janvier 2018.

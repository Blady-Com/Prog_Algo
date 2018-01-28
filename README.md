#Arbres Binaires
La structure d'arbre binaire exposée ci-dessous a été utilisée pour le compilateur d'automate, voir aussi sur [Github/Blady](https://github.com/Blady-Com).
À ce qui a déjà été dit dans l'article consacré au compilateur d'automate, j'apporte ici un complément sur l'utilisation de l'arbre binaire pour mémoriser les identificateurs du langage et son optimisation avec l'utilisation d'Ada pour généraliser le concept d'arbres binaires sur des types de données quelconques sans les contraintes imposées par le Pascal.
Cette première idée consistait naturellement à construire un arbre équilibré au fur à mesure des ajouts d'éléments supplémentaires. Mais à l'époque le temps manquait, sans accès à Internet... Je me suis rabattu sur une solution béton mais brutale : l'arbre binaire est réalisé par construction dichotomique. J'ai donc codé une procédure "Balance" qui reconstruit la dichotomie comme cela est expliqué précédemment pour le compilateur d'automate.
Longtemps après, je suis tombé coup sur coup sur un livre traitant des arbres binaires "Arbres, Tables et Algorithmes" (J. Guyot - Ch. Vial - 1989) et sur une impressionnante liste de sites sur Internet (ceci même en ne retenant que les sites de langue française). Les auteurs du livres expliquent la construction d'un arbre binaire AVL pour G.M. Andelson-Velskii, E.M. Landis (1962). Cette construction reprenait mon désir premier de construire un arbre équilibré à chaque ajout d'éléments. Pour cela l'astuce est d'ajouter à chaque noeud un attribut qui indique si les sous-arbres de chaque noeud penchent plutôt à gauche, à droite ou sont en équilibres. L'action résultante consiste à rééquilibrer l'arbre chaque fois que l'écart de chaque sous arbre est supérieur d'un niveau.

Pascal Pignard, Janvier 2018.
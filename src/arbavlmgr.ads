--------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : ArbAvlMgr.ads
-- AUTEUR DU CSU                    : P. Pignard
-- VERSION DU CSU                   : 1.0a
-- DATE DE LA DERNIERE MISE A JOUR  : 4 février 2002
-- ROLE DU CSU                      : Unité de gestion d'un arbre binaire AVL.
--
--
-- FONCTIONS EXPORTEES DU CSU       : Insere, Retire, Recherche, AfficheArbre, Detruit
--
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            : Arbres AVL (G.M. Andelson-Velskii,
--                                    E.M. Landis, 1962)
--
--------------------------------------------------------------------------------

generic

   -- Type de la clef de tri
   type TClef is private;

   -- Type des éléments triés suivant la clef
   type TElement is private;

   -- Element neutre indiquant notamment une recherche non aboutie
   NonDefini : TElement;

   -- Relation d'ordre de la clef de tri
   with function "<" (Left, Right : TClef) return Boolean is <>;

package ArbAvlMgr is

   -- Procédures assurant la gestion de l'arbre binaire AVL.
   procedure Insere (Clef : TClef; Element : TElement);
   procedure Retire (Clef : TClef; Element : out TElement);
   procedure Recherche (Clef : TClef; Element : out TElement);

   generic
      with function Image (Clef : TClef) return String;
   procedure AfficheArbre;

   procedure Detruit;

end ArbAvlMgr;

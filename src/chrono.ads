--------------------------------------------------------------------------------
-- NOM DU CSU (spécification)       : Chrono.ads
-- AUTEUR DU CSU                    : P. Pignard
-- VERSION DU CSU                   : 2.0b
-- DATE DE LA DERNIERE MISE A JOUR  : 10 février 2001
-- ROLE DU CSU                      : Chronométrage.
--
--
-- FONCTIONS EXPORTEES DU CSU       : Lance, Stoppe, Valeur.
--
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
-- COPYRIGHT                        : (c) Pascal Pignard 2001
-- LICENCE                          : CeCILL V2 (http://www.cecill.info)
-- CONTACT                          : http://blady.pagesperso-orange.fr
--------------------------------------------------------------------------------

package Chrono is

   -- Lance le compteur horaire.
   procedure Lance;

   -- Stoppe le compteur horaire.
   procedure Stoppe;

   -- Renvoie le compteur horaire interne en milisecondes.
   function Valeur return Natural;

end Chrono;

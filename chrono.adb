--------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : Chrono.adb
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
with Ada.Calendar; use Ada.Calendar;

package body Chrono is

   ValeurInitiale : Natural := 0;

   -- Lance le compteur horaire.
   procedure Lance is
   begin
      ValeurInitiale := Natural (Seconds (Clock) * 1000.0);
   end Lance;

   -- Stoppe le compteur horaire.
   procedure Stoppe is
   begin
      null;
   end Stoppe;

   -- Renvoie le compteur horaire interne en milisecondes.
   function Valeur return Natural is
   begin
      return Natural (Seconds (Clock) * 1000.0) - ValeurInitiale;
   end Valeur;

end Chrono;

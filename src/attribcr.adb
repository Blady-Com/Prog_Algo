---------------------------------------------------------------------------
-- NOM DU CSU (principal)           : attribcr.adb
-- AUTEUR DU CSU                    : P. Pignard
-- VERSION DU CSU                   : 1.0b
-- DATE DE LA DERNIERE MISE A JOUR  : 29 mars 2001
-- ROLE DU CSU                      : Algorithme d'attibution croisé.
--
--
-- FONCTIONS EXPORTEES DU CSU       :
--
--
-- FONCTIONS LOCALES DU CSU         :
--
--
-- NOTES                            :
--
--
---------------------------------------------------------------------------

-- Converted by (New) P2Ada v. 14-Jan-2003
-- The following with/use clauses are put graciously by P2Ada.
-- Some of them may be useless, your Ada compiler will tell it you.
--   (GNAT: with '-gnatwa')
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;       use Ada.Float_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling; -- UpCase
-- This is for Dispose. P2Ada writes automatically:
--   "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
with Ada.Unchecked_Deallocation;

procedure AttribCR is

   CMaxCouples  : constant := 10;
   CMaxNivChoix : constant := CMaxCouples + 1;
   Vilain       : constant := 0;

   type TListeSoupirantsLibre;
   type PListeSoupirantsLibre is access TListeSoupirantsLibre;
   type TListeSoupirantsLibre is record
      Suivant   : PListeSoupirantsLibre;
      Soupirant : Integer;
   end record;
   procedure Dispose is new Ada.Unchecked_Deallocation (TListeSoupirantsLibre, PListeSoupirantsLibre);
   type TTabCouples is array (1 .. CMaxCouples) of Integer;

   type TTabPrefSoupirants is array (1 .. CMaxCouples, 1 .. CMaxNivChoix) of Integer;

   type TTabPrefCandidates is array (1 .. CMaxCouples, 1 .. CMaxNivChoix) of Integer;
   type TTabPref2Candidates is array (1 .. CMaxCouples, 0 .. CMaxCouples) of Integer;

   TabCouples         : TTabCouples;
   SoupirantsLibres   : PListeSoupirantsLibre;
   TabPrefSoupirants  : TTabPrefSoupirants;
   TabPrefCandidates  : TTabPrefCandidates;
   TabPref2Candidates : TTabPref2Candidates;

   procedure Init is
--       Element,Choix:Integer;
   begin
      SoupirantsLibres := null;
      for Element in 1 .. CMaxCouples loop

         TabCouples (Element) := 0;
         for Choix in 1 .. CMaxNivChoix loop

            TabPrefSoupirants (Element, Choix) := Vilain;
            TabPrefCandidates (Element, Choix) := Vilain;
         end loop;
      end loop;
   end Init;

   procedure Pousse (Soupirant : Integer) is
      NouveauSoupirant : PListeSoupirantsLibre;
   begin
      if False then
         --  Empilage en tête

         NouveauSoupirant               := new TListeSoupirantsLibre;
         NouveauSoupirant.all.Suivant   := SoupirantsLibres;
         NouveauSoupirant.all.Soupirant := Soupirant;
         SoupirantsLibres               := NouveauSoupirant;
      else
         --  Empilage en queue
         if SoupirantsLibres = null then

            NouveauSoupirant               := new TListeSoupirantsLibre;
            NouveauSoupirant.all.Suivant   := SoupirantsLibres;
            NouveauSoupirant.all.Soupirant := Soupirant;
            SoupirantsLibres               := NouveauSoupirant;
         else
            NouveauSoupirant := SoupirantsLibres;
            while NouveauSoupirant.all.Suivant /= null loop
               NouveauSoupirant := NouveauSoupirant.all.Suivant;
            end loop;
            NouveauSoupirant.all.Suivant   := new TListeSoupirantsLibre;
            NouveauSoupirant               := NouveauSoupirant.all.Suivant;
            NouveauSoupirant.all.Soupirant := Soupirant;
            NouveauSoupirant.all.Suivant   := null;
         end if;
      end if;
   end Pousse;

   procedure Retire (Soupirant : in out Integer) is
      NouveauSoupirant : PListeSoupirantsLibre;
   begin
      NouveauSoupirant := SoupirantsLibres.all.Suivant;
      Soupirant        := SoupirantsLibres.all.Soupirant;
      Dispose (SoupirantsLibres);
      SoupirantsLibres := NouveauSoupirant;
   end Retire;

   function EstVide return Boolean is
      Result_EstVide : Boolean;
   begin
      Result_EstVide := SoupirantsLibres = null;
      return Result_EstVide;
   end EstVide;

   function RenvoiAttrib (Candidate : Integer) return Integer is
      Result_RenvoiAttrib : Integer;
   begin
      Result_RenvoiAttrib := TabCouples (Candidate);
      return Result_RenvoiAttrib;
   end RenvoiAttrib;

   procedure Attribution (Candidate : Integer; Pretendant : Integer) is
   begin
      TabCouples (Candidate) := Pretendant;
   end Attribution;

   procedure RenverseTabPref is
--       Element,Choix:Integer;
   begin
      for Element in 1 .. CMaxCouples loop

         for Choix in 1 .. CMaxNivChoix loop
            TabPref2Candidates (Element, TabPrefCandidates (Element, Choix)) := Choix;
         end loop;
         TabPref2Candidates (Element, Vilain) := CMaxNivChoix;
      end loop;
   end RenverseTabPref;

   procedure AttributionCroisee is
      Soupirant, Candidate, Choix : Integer := 0;
   begin
      while not EstVide loop
         Retire (Soupirant);
         --  Recherche d'une candidate
         Choix     := 1;
         Candidate := TabPrefSoupirants (Soupirant, Choix);
         while TabPref2Candidates (Candidate, RenvoiAttrib (Candidate)) < TabPref2Candidates (Candidate, Soupirant) loop
            Choix     := Choix + 1;
            Candidate := TabPrefSoupirants (Soupirant, Choix);
         end loop;
         if RenvoiAttrib (Candidate) /= Vilain then
            Pousse (RenvoiAttrib (Candidate));
            Attribution (Candidate, Soupirant);
         else
            Attribution (Candidate, Soupirant);
         end if;
      end loop;
   end AttributionCroisee;

   function Translate (Element, Base : Character) return Integer is
      Result_Translate : Integer;
   begin
      Result_Translate := Character'Pos (To_Upper (Element)) - Character'Pos (Base) + 1;
      return Result_Translate;
   end Translate;

--       NbCandidates,NbSoupirants,Candidate,Soupirant,Choix,Performance:Integer;
   NbCandidates, NbSoupirants, Performance : Integer;
   Element                                 : Character;

begin
   Put ("Attribution croisées.");
   New_Line;
   Init;

   loop
      Put ("Entrer le nombre de soupirants (1 .. 10) : ");
      Get (NbSoupirants);
      Skip_Line; -- [P2Ada]: !Help! Maybe (file)
      exit when (NbSoupirants >= 1) or (NbSoupirants <= CMaxCouples);
   end loop;

   loop
      Put ("Entrer le nombre de candidates (1 .. 10) : ");
      Get (NbCandidates);
      Skip_Line; -- [P2Ada]: !Help! Maybe (file)
      exit when (NbCandidates >= 1) or (NbCandidates <= CMaxCouples);
   end loop;

   Put ("Liste des soupirants : ");
   for Soupirant in 1 .. NbSoupirants loop
      Put (Character'Val (Character'Pos ('A') + Soupirant - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
      Put ("  ");
   end loop;
   New_Line;

   Put ("Liste des candidates : ");
   for Candidate in 1 .. NbCandidates loop
      Put (Character'Val (Character'Pos ('L') + Candidate - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
      Put ("  ");
   end loop;
   New_Line;

   for Soupirant in 1 .. NbSoupirants loop
      Put ("Choix du soupirant ");
      Put (Character'Val (Character'Pos ('A') + Soupirant - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
      Put (" :");
      New_Line; -- [P2Ada]: !Help! Maybe (file)
      Pousse (Soupirant);
      for Choix in 1 .. NbCandidates loop
         loop
            Put ("Choix ");
            Put (Choix); -- [P2Ada]: !Help! Maybe (file,...) on next
            Put (", entrer la candidate choisie : ");
            Get (Element);
            Skip_Line; -- [P2Ada]: !Help! Maybe (file)
            TabPrefSoupirants (Soupirant, Choix) := Translate (Element, 'L');
            exit when (TabPrefSoupirants (Soupirant, Choix) >= 1) or
              (TabPrefSoupirants (Soupirant, Choix) <= CMaxCouples);
         end loop;
      end loop;
   end loop;

   for Candidate in 1 .. NbCandidates loop
      Put ("Choix de la Candidate ");
      Put (Character'Val (Character'Pos ('L') + Candidate - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
      Put (" :");
      New_Line; -- [P2Ada]: !Help! Maybe (file)
      Attribution (Candidate, Vilain);
      for Choix in 1 .. NbSoupirants loop
         loop
            Put ("Choix ");
            Put (Choix); -- [P2Ada]: !Help! Maybe (file,...) on next
            Put (", entrer le soupirant choisi : ");
            Get (Element);
            Skip_Line; -- [P2Ada]: !Help! Maybe (file)
            TabPrefCandidates (Candidate, Choix) := Translate (Element, 'A');
            exit when (TabPrefSoupirants (Candidate, Choix) >= 1) or
              (TabPrefSoupirants (Candidate, Choix) <= CMaxCouples);
         end loop;
      end loop;
   end loop;
   New_Line;

   RenverseTabPref;
   AttributionCroisee;
   Performance := 0;
   for Candidate in 1 .. NbCandidates loop
      if TabCouples (Candidate) > Vilain then
         Put ("Attribution pour la candidate ");
         Put (Character'Val (Character'Pos ('L') + Candidate - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (" du prétendant "); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put
           (Character'Val
              (Character'Pos ('A') + TabCouples (Candidate) - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (" (Choix n° "); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (TabPref2Candidates (Candidate, TabCouples (Candidate))); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (").");
         New_Line; -- [P2Ada]: !Help! Maybe (file)
         Performance := Performance + TabPref2Candidates (Candidate, TabCouples (Candidate));
      else
         Put ("Pas d'attribution pour la candidate ");
         Put (Character'Val (Character'Pos ('L') + Candidate - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put ('.');
         New_Line; -- [P2Ada]: !Help! Maybe (file)
         Performance := Performance + NbSoupirants + 1;
      end if;
   end loop;
   New_Line;

   Put ("Performance :");
   Put (Float (Performance) / Float (NbCandidates), 4, 1, 0);
   New_Line; -- [P2Ada]: !Help! Maybe (file)
end AttribCR;
-- Converted by (New) P2Ada v. 14-Jan-2003

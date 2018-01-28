---------------------------------------------------------------------------
-- NOM DU CSU (principal)           : attrib.adb
-- AUTEUR DU CSU                    : P. Pignard
-- VERSION DU CSU                   : 1.0b
-- DATE DE LA DERNIERE MISE A JOUR  : 28 décembre 2000
-- ROLE DU CSU                      : Algorithme d'attibution.
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
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
-- This is for Dispose. P2Ada writes automatically:
--   "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
with Ada.Unchecked_Deallocation;

procedure Attrib is

   CMaxNivChoix : constant := 10;
   CMaxElements : constant := 10;
   CMaxClientes : constant := 10;

   type TlisteAttrib;
   type PListeAttrib is access TlisteAttrib;
   type TListeAttrib is record
      Suivant : PListeAttrib;
      Attrib  : Integer;
   end record;
   procedure Dispose is new Ada.Unchecked_Deallocation (TListeAttrib, PListeAttrib);
   type TTabAttrib is array (1 .. CMaxNivChoix, 1 .. CMaxElements) of PListeAttrib;

   type TTabChoix is array (1 .. CMaxClientes, 0 .. CMaxElements) of Integer;

   TabAttrib : TTabAttrib;

   procedure Init is
--       NivChoix,Element:Integer;
   begin
      for NivChoix in 1 .. CMaxNivChoix loop
         for Element in 1 .. CMaxElements loop
            if TabAttrib (NivChoix, Element) /= null then

               --  Dispose(TabAttrib[NivChoix, Element]); -- Utile pour une réinit

               TabAttrib (NivChoix, Element) := null;
            end if;
         end loop;
      end loop;
   end Init;

   procedure Add (NivChoix, Element, Attrib : Integer) is
      NouvelAttrib : PListeAttrib;
   begin
      NouvelAttrib                  := new TListeAttrib;
      NouvelAttrib.all.Suivant      := TabAttrib (NivChoix, Element);
      NouvelAttrib.all.Attrib       := Attrib;
      TabAttrib (NivChoix, Element) := NouvelAttrib;
   end Add;

   function Fini return Boolean is
      Result_Fini : Boolean;
--       NivChoix,Element:Integer;
      Trouve : Boolean;
   begin
      Trouve := False;
      for NivChoix in 1 .. CMaxNivChoix loop
         for Element in 1 .. CMaxElements loop
            if TabAttrib (NivChoix, Element) /= null then
               Trouve := True;
            end if;
         end loop;
      end loop;
      Result_Fini := not Trouve;
      return Result_Fini;
   end Fini;

   function NbAttrib (NivChoix, Element : Integer) return Integer is
      Result_NbAttrib : Integer;
      Compteur        : Integer;
      Attrib          : PListeAttrib;
   begin
      Compteur := 0;
      Attrib   := TabAttrib (NivChoix, Element);
      while Attrib /= null loop

         Compteur := Compteur + 1;
         Attrib   := Attrib.all.Suivant;
      end loop;
      Result_NbAttrib := Compteur;
      return Result_NbAttrib;
   end NbAttrib;

   procedure Elimine (Element, Attrib : Integer) is
--      NivChoix:Integer;
      Prec, Dum : PListeAttrib;
   begin
      --  Writeln('elimine : ', Element, Attrib); -- pour débug

      for NivChoix in 1 .. CMaxNivChoix loop
         while TabAttrib (NivChoix, Element) /= null loop
            Dum                           := TabAttrib (NivChoix, Element);
            TabAttrib (NivChoix, Element) := Dum.all.Suivant;
            Dispose (Dum);
         end loop;
      end loop;
      for NivChoix in 1 .. CMaxNivChoix loop
         for Element in 1 .. CMaxElements loop
            Dum  := TabAttrib (NivChoix, Element);
            Prec := Dum;
            while Dum /= null loop
               if Dum.all.Attrib = Attrib then
                  if Prec = Dum then
                     TabAttrib (NivChoix, Element) := Dum.all.Suivant;
                     Dispose (Dum);
                     Dum  := TabAttrib (NivChoix, Element);
                     Prec := Dum;
                  else
                     Prec.all.Suivant := Dum.all.Suivant;
                     Dispose (Dum);
                     Dum := Prec.all.Suivant;
                  end if;
               else
                  if Prec /= Dum then
                     Prec := Prec.all.Suivant;
                  end if;
                  Dum := Dum.all.Suivant;
               end if;
            end loop;
         end loop;
      end loop;
   end Elimine;

   function Random (Max : Integer) return Integer is
      pragma Unreferenced (Max);
      Result_Random : Integer;
   begin
      Result_Random := 10; --  Pas vraiment aléatoire, en attente d'une fonction le permettant
      return Result_Random;
   end Random;

   function RenvoiAttrib (NivChoix, Element : Integer) return Integer is
      Result_RenvoiAttrib : Integer;
--      Iterration:Integer;
      Attrib : PListeAttrib;
   begin
      Attrib := TabAttrib (NivChoix, Element);
      for Iterration in 1 .. Random (50) loop
         if Attrib = null then
            Attrib := TabAttrib (NivChoix, Element);
         else
            Attrib := Attrib.all.Suivant;
         end if;
      end loop;
      if Attrib = null then
         Result_RenvoiAttrib := 0;
      else
         Result_RenvoiAttrib := Attrib.all.Attrib;
      end if;
      return Result_RenvoiAttrib;
   end RenvoiAttrib;

   procedure Attribution
     (TabChoix   : in out TTabChoix;
      NbClientes :        Integer;
      NbElements :        Integer;
      NbNivChoix :        Integer)
   is
      procedure Resoudre (NivRes : Integer; inNivChoix : Integer; NbElements : Integer; CasResolu : in out Boolean) is
         NivChoix : Integer := inNivChoix;
--       Element,Attrib:Integer;
         Attrib : Integer;
      begin
         CasResolu := False;
         for Element in 1 .. NbElements loop
            if NbAttrib (NivChoix, Element) = NivRes then

               Attrib   := RenvoiAttrib (NivChoix, Element);
               NivChoix := 1;
               while Element /= TabChoix (Attrib, NivChoix) loop
                  NivChoix := NivChoix + 1;
               end loop;
               TabChoix (Attrib, 0) := NivChoix;
               Elimine (Element, Attrib);
               CasResolu := True;
            end if;
         end loop;
      end Resoudre;

--        Cliente,NivChoix,NivRes:Integer;
      NivChoix, NivRes : Integer;
      Resolution       : Boolean;

   begin
      Init;
      for lNivChoix in 1 .. NbNivChoix loop
         for Cliente in 1 .. NbClientes loop
            Add (lNivChoix, TabChoix (Cliente, lNivChoix), Cliente);
         end loop;
      end loop;
      Resolution := False;
      NivRes     := 1;
      loop
         NivChoix := 1;
         if not Fini then
            Resoudre (NivRes, NivChoix, NbElements, Resolution);
         end if;
         NivChoix := 2;
         if not Fini and (NivChoix <= NbNivChoix) then
            Resoudre (NivRes, NivChoix, NbElements, Resolution);
         end if;
         NivChoix := 3;
         while not Resolution and (NivChoix <= NbNivChoix) and not Fini loop

            Resoudre (NivRes, NivChoix, NbElements, Resolution);
            NivChoix := NivChoix + 1;
         end loop;
         if not Fini and not Resolution then
            NivRes := NivRes + 1;
         end if;
         exit when Fini or (NivRes > NbClientes);
      end loop;
   end Attribution;

--      NbClientes,NbRobes,Choix,Cliente,Performance:Integer;
   NbClientes, NbRobes, Performance : Integer;
   TabClientes                      : TTabChoix;

begin
   Put ("Attribution ""performante"".");
   New_Line;

   loop
      Put ("Entrer le nombre de clientes (1 .. 10) : ");
      Get (NbClientes);
      Skip_Line; -- [P2Ada]: !Help! Maybe (file)
      exit when (NbClientes >= 1) or (NbClientes <= CMaxClientes);
   end loop;

   loop
      Put ("Entrer le nombre de robes (1 .. 10) : ");
      Get (NbRobes);
      Skip_Line; -- [P2Ada]: !Help! Maybe (file)
      exit when (NbRobes >= 1) or (NbRobes <= CMaxElements);
   end loop;

   for Cliente in 1 .. NbClientes loop
      Put ("Choix de la cliente ");
      Put (Character'Val (Character'Pos ('A') + Cliente - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
      Put (" :");
      New_Line; -- [P2Ada]: !Help! Maybe (file)
      TabClientes (Cliente, 0) := 0;
      for Choix in 1 .. NbRobes loop
         loop
            Put ("Entrer la robe choisie (1 .. 10) : ");
            Get (TabClientes (Cliente, Choix));
            Skip_Line; -- [P2Ada]: !Help! Maybe (file)
            exit when (TabClientes (Cliente, Choix) >= 1) or (TabClientes (Cliente, Choix) <= CMaxElements);
         end loop;
      end loop;
   end loop;

   Attribution (TabClientes, NbClientes, NbRobes, NbRobes);
   Performance := 0;
   New_Line;

   for Cliente in 1 .. NbClientes loop
      if TabClientes (Cliente, 0) > 0 then
         Put ("Attribution de la robe ");
         Put (TabClientes (Cliente, TabClientes (Cliente, 0))); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (" a la cliente "); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (Character'Val (Character'Pos ('A') + Cliente - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (" (Choix "); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (TabClientes (Cliente, 0)); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put (").");
         New_Line; -- [P2Ada]: !Help! Maybe (file)
         Performance := Performance + TabClientes (Cliente, 0);
      else
         Put ("Pas d'attribution de robe a la cliente ");
         Put (Character'Val (Character'Pos ('A') + Cliente - 1)); -- [P2Ada]: !Help! Maybe (file,...) on next
         Put ('.');
         New_Line; -- [P2Ada]: !Help! Maybe (file)
         Performance := Performance + NbRobes + 1;
      end if;
   end loop;
   New_Line;

   Put ("Performance :");
   Put (Float (Performance) / Float (NbClientes), 4, 1, 0);
   New_Line; -- [P2Ada]: !Help! Maybe (file)
end Attrib;
-- Converted by (New) P2Ada v. 14-Jan-2003

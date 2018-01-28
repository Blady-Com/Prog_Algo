--------------------------------------------------------------------------------
-- NOM DU CSU (corps)               : ArbAvlMgr.adb
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

with Text_IO; use Text_IO;
with Ada.Unchecked_Deallocation;

package body ArbAvlMgr is

   -- Définition d'un noeud pour la gestion de l'arbre binaire.
   type TBalance is (Gauche, Egal, Droit); -- poids relatif des sous-arbres

   type TNoeud;
   type PNoeud is access TNoeud;
   type TNoeud is record
      Gauche  : PNoeud;   -- branche inférieure de l'arbre
      Droit   : PNoeud;   -- branche supérieure de l'arbre
      Balance : TBalance; -- critère d'équilibre
      Clef    : TClef;    -- clef de comparaison
      Element : TElement;
      -- stockage de l'élément à trier ou à rechercher
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (TNoeud, PNoeud);

   Arbre : PNoeud := null;

   -- Ré-équilibrage Gauche - Gauche
   procedure RGG (Noeud : in out PNoeud; Modif : in out Boolean) is
      Noeud1 : PNoeud;
   begin
      Noeud1       := Noeud.Gauche;
      Noeud.Gauche := Noeud1.Droit;
      Noeud1.Droit := Noeud;
      if Noeud.Balance = Egal then
         Noeud.Balance  := Gauche;
         Noeud1.Balance := Droit;
         Modif          := False;
      else
         Noeud.Balance  := Egal;
         Noeud1.Balance := Egal;
      end if;
      Noeud := Noeud1;
   end RGG;

   -- Ré-équilibrage Droite - Droite
   procedure RDD (Noeud : in out PNoeud; Modif : in out Boolean) is
      Noeud1 : PNoeud;
   begin
      Noeud1        := Noeud.Droit;
      Noeud.Droit   := Noeud1.Gauche;
      Noeud1.Gauche := Noeud;
      if Noeud.Balance = Egal then
         Noeud.Balance  := Droit;
         Noeud1.Balance := Gauche;
         Modif          := False;
      else
         Noeud.Balance  := Egal;
         Noeud1.Balance := Egal;
      end if;
      Noeud := Noeud1;
   end RDD;

   -- Ré-équilibrage Gauche - Droite
   procedure RGD (Noeud : in out PNoeud) is
      Noeud1 : PNoeud;
      Noeud2 : PNoeud;
   begin
      Noeud1        := Noeud.Gauche;
      Noeud2        := Noeud1.Droit;
      Noeud1.Droit  := Noeud2.Gauche;
      Noeud2.Gauche := Noeud1;
      Noeud.Gauche  := Noeud2.Droit;
      Noeud2.Droit  := Noeud;
      if Noeud2.Balance = Gauche then
         Noeud.Balance := Droit;
      else
         Noeud.Balance := Egal;
      end if;
      if Noeud2.Balance = Droit then
         Noeud1.Balance := Gauche;
      else
         Noeud1.Balance := Egal;
      end if;
      Noeud         := Noeud2;
      Noeud.Balance := Egal;
   end RGD;

   -- Ré-équilibrage Droite - Gauche
   procedure RDG (Noeud : in out PNoeud) is
      Noeud1 : PNoeud;
      Noeud2 : PNoeud;
   begin
      Noeud1        := Noeud.Droit;
      Noeud2        := Noeud1.Gauche;
      Noeud1.Gauche := Noeud2.Droit;
      Noeud2.Droit  := Noeud1;
      Noeud.Droit   := Noeud2.Gauche;
      Noeud2.Gauche := Noeud;
      if Noeud2.Balance = Droit then
         Noeud.Balance := Gauche;
      else
         Noeud.Balance := Egal;
      end if;
      if Noeud2.Balance = Gauche then
         Noeud1.Balance := Droit;
      else
         Noeud1.Balance := Egal;
      end if;
      Noeud         := Noeud2;
      Noeud.Balance := Egal;
   end RDG;

   -- Insertion dans un arbre AVL
   procedure Insere (Clef : TClef; Element : TElement) is
      PlusGrand : Boolean := False;

      procedure InsereDans (Noeud : in out PNoeud) is
      begin
         if Noeud = null then
            -- accrochage de l'élément
            Noeud     := new TNoeud'(Gauche => null, Droit => null, Balance => Egal, Clef => Clef, Element => Element);
            PlusGrand := True;
         else
            if Clef /= Noeud.Clef then
               if Clef < Noeud.Clef then
                  InsereDans (Noeud.Gauche);
                  if PlusGrand then
                     if Noeud.Balance = Gauche then
                        if Noeud.Gauche.Balance = Gauche then
                           RGG (Noeud, PlusGrand);
                        else
                           RGD (Noeud);
                           PlusGrand := False;
                        end if;
                        PlusGrand := False;
                     else
                        if Noeud.Balance = Droit then
                           Noeud.Balance := Egal;
                           PlusGrand     := False;
                        else
                           if Noeud.Balance = Egal then
                              Noeud.Balance := Gauche;
                           end if;
                        end if;
                     end if;
                  end if;
               else
                  InsereDans (Noeud.Droit);
                  if PlusGrand then
                     if Noeud.Balance = Droit then
                        if Noeud.Droit.Balance = Droit then
                           RDD (Noeud, PlusGrand);
                        else
                           RDG (Noeud);
                        end if;
                        PlusGrand := False;
                     else
                        if Noeud.Balance = Gauche then
                           Noeud.Balance := Egal;
                           PlusGrand     := False;
                        else
                           if Noeud.Balance = Egal then
                              Noeud.Balance := Droit;
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end InsereDans;

   begin
      InsereDans (Arbre);
   end Insere;

   -- Remplace renvoie dans Q un pointeur sur le noeud à déchaîner
   procedure Remplace (R : in out PNoeud; Q : in out PNoeud; Modif : in out Boolean) is
   begin
      if R.Droit /= null then
         Remplace (R.Droit, Q, Modif);
         if Modif then
            if R.Balance = Gauche then
               if R.Gauche.Balance = Droit then
                  RGD (R);
               else
                  RGG (R, Modif);
               end if;
            else
               if R.Balance = Droit then
                  R.Balance := Egal;
               else
                  if R.Balance = Egal then
                     R.Balance := Gauche;
                     Modif     := False;
                  end if;
               end if;
            end if;
         end if;
      else
         Q.Clef    := R.Clef;
         Q.Element := R.Element;
         Q         := R;
         R         := R.Gauche;
         Modif     := True;
      end if;
   end Remplace;

   -- Retrait dans un arbre AVL
   procedure Retire (Clef : TClef; Element : out TElement) is
      PlusPetit : Boolean := False;

      procedure RetireDans (Noeud : in out PNoeud) is
         NoeudQ : PNoeud;
      begin
         if Noeud = null then
            Element := NonDefini;
         else
            if Clef < Noeud.Clef then
               RetireDans (Noeud.Gauche);
               if PlusPetit then
                  if Noeud.Balance = Droit then
                     if Noeud.Droit.Balance = Gauche then
                        RDG (Noeud);
                     else
                        RDD (Noeud, PlusPetit);
                     end if;
                  else
                     if Noeud.Balance = Gauche then
                        Noeud.Balance := Egal;
                     else
                        if Noeud.Balance = Egal then
                           Noeud.Balance := Droit;
                           PlusPetit     := False;
                        end if;
                     end if;
                  end if;
               end if;
            else
               if Noeud.Clef < Clef then
                  RetireDans (Noeud.Droit);
                  if PlusPetit then
                     if Noeud.Balance = Gauche then
                        if Noeud.Gauche.Balance = Droit then
                           RGD (Noeud);
                        else
                           RGG (Noeud, PlusPetit);
                        end if;
                     else
                        if Noeud.Balance = Droit then
                           Noeud.Balance := Egal;
                        else
                           if Noeud.Balance = Egal then
                              Noeud.Balance := Gauche;
                              PlusPetit     := False;
                           end if;
                        end if;
                     end if;
                  end if;
               else
                  NoeudQ := Noeud;
                  if Noeud.Droit = null then
                     Noeud     := Noeud.Gauche;
                     PlusPetit := True;
                  else
                     if Noeud.Gauche = null then
                        Noeud     := Noeud.Droit;
                        PlusPetit := True;
                     else
                        Remplace (NoeudQ.Gauche, NoeudQ, PlusPetit);
                        if PlusPetit then
                           if Noeud.Balance = Droit then
                              if Noeud.Droit.Balance = Gauche then
                                 RDG (Noeud);
                              else
                                 RDD (Noeud, PlusPetit);
                              end if;
                           else
                              if Noeud.Balance = Gauche then
                                 Noeud.Balance := Egal;
                              else
                                 if Noeud.Balance = Egal then
                                    Noeud.Balance := Droit;
                                    PlusPetit     := False;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;
                  Element := NoeudQ.Element;
                  Free (NoeudQ);
               end if;
            end if;
         end if;
      end RetireDans;

   begin
      RetireDans (Arbre);
   end Retire;

   -- Procédure qui recherche un élément dans l'arbre binaire et qui renvoie son Element.
   procedure Recherche (Clef : TClef; Element : out TElement) is
      procedure RechercheDans (Noeud : PNoeud) is
      begin
         if Clef = Noeud.Clef then
            Element := Noeud.Element;
         elsif Clef < Noeud.Clef then
            if Noeud.Gauche /= null then
               RechercheDans (Noeud.Gauche);
            end if;
         elsif Noeud.Droit /= null then
            RechercheDans (Noeud.Droit);
         end if;
      end RechercheDans;

   begin
      Element := NonDefini;
      if Arbre /= null then
         RechercheDans (Arbre);
      end if;
   end Recherche;

   -- Procédure de destruction de l'arbre binaire.
   procedure Detruit is

      procedure Elimine (Noeud : PNoeud) is
         Dum : PNoeud := Noeud;
      begin
         if Noeud.Gauche /= null then
            Elimine (Noeud.Gauche);
         end if;
         if Noeud.Droit /= null then
            Elimine (Noeud.Droit);
         end if;
         Free (Dum);
      end Elimine;

   begin
      if Arbre /= null then
         Elimine (Arbre);
      end if;
      Arbre := null;
   end Detruit;

   -- Procédure d'affichage de l'arbre binaire sous forme d'arbre.
   procedure AfficheArbre is
      procedure Affiche (Noeud : PNoeud; Pos : Integer) is
         Bal : constant String := TBalance'Image (Noeud.Balance) & "      ";

      begin
         Put (Image (Noeud.Clef) & Bal (1 .. 6));
         if Noeud.Droit /= null then
            Affiche (Noeud.Droit, Pos + 1);
         end if;
         New_Line;
         for Tab in 1 .. Pos + 1 loop
            Put ("            ");
         end loop;
         if Noeud.Gauche /= null then
            Affiche (Noeud.Gauche, Pos + 1);
         end if;
      end Affiche;

   begin
      if Arbre /= null then
         Affiche (Arbre, 0);
      end if;
      New_Line;
   end AfficheArbre;

end ArbAvlMgr;

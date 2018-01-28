with ArbMgr;
with Chrono;  use Chrono;
with Text_IO; use Text_IO;
procedure Test_Arb_Bin is

   subtype TClef is String (1 .. 10);
   subtype TElement is Integer;

   NonDefini : constant TElement := 0;

   package Int_IO is new Integer_IO (Integer);

   -- Création de l'arbre sans appel automatique à Balance
   package MonArbre is new ArbMgr (TClef, TElement, NonDefini, False);
   use MonArbre;

   procedure AfficheListe is
      Element : TElement := MonArbre.RetournePremier;
   begin
      while Element /= NonDefini loop
         Int_IO.Put (Element, 6);
         Element := MonArbre.RetourneSuivant;
      end loop;
   end AfficheListe;

   function Format (Ind : TElement) return TClef is
      Dum   : constant String := TElement'Image (Ind);
      Blanc : constant TClef  := "          ";
   begin
      return Dum & Blanc (1 .. 10 - Dum'Last);
   end Format;

   subtype Interval is TElement range 1 .. 1000;
   Element : TElement;

begin
   Put_Line ("Essai de l'Unité : ArbreMgr.");

   Put_Line ("Création de l'arbre.");
   Lance;
   for Ind in Interval loop
      MonArbre.Ajoute (Format (Ind), Ind);
   end loop;
   Put_Line (Natural'Image (Valeur) & " ms");

   Put_Line ("""" & Format (Interval'Last) & " éléments""");
   AfficheListe;

   Lance;
   MonArbre.Recherche (Format (Interval'First), Element);
   Put_Line ("Élement recherché :" & TElement'Image (Element));
   --Stoppe;
   Put_Line (Natural'Image (Valeur) & " ms");
   Lance;
   MonArbre.Recherche (Format (Interval'Last), Element);
   Put_Line ("Élement recherché :" & TElement'Image (Element));
   --Stoppe;
   Put_Line (Natural'Image (Valeur) & " ms");

   --MonArbre.AfficheArbre;

   Put_Line ("Appel à 'Balance'.");
   Lance;
   --MonArbre.Balance;
   Put_Line (Natural'Image (Valeur) & " ms");

   --MonArbre.AfficheArbre;

   Lance;
   MonArbre.Recherche (Format (Interval'First), Element);
   Put_Line ("Élement recherché :" & TElement'Image (Element));
   --Stoppe;
   Put_Line (Natural'Image (Valeur) & " ms");
   Lance;
   MonArbre.Recherche (Format (Interval'Last), Element);
   Put_Line ("Élement recherché :" & TElement'Image (Element));
   --Stoppe;
   Put_Line (Natural'Image (Valeur) & " ms");

   Put_Line ("Le résultat est identique mais plus rapide.");
   MonArbre.Detruit;
end Test_Arb_Bin;

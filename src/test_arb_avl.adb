with ArbAvlMgr;
with Chrono;  use Chrono;
with Text_IO; use Text_IO;

procedure Test_Arb_AVL is

   subtype TClef is Integer;
   subtype TElement is Integer;

   NonDefini : constant TElement := 0;

   package MonArbre is new ArbAvlMgr (TClef, TElement, NonDefini);
   use MonArbre;

   function Image (Clef : TClef) return String is
      Dum : constant String := Integer'Image (Clef) & "      ";
   begin
      return Dum (1 .. 6);
   end Image;

   procedure MonAffArbre is new MonArbre.AfficheArbre (Image);

   subtype Interval is TElement range 1 .. 10;

   Ordre : constant array (Interval) of Interval := (5, 2, 9, 4, 1, 3, 8, 6, 7, 10);

   Element : TElement;

begin
   Put_Line ("Essai de l'Unité : ArbreMgr.");

   Put_Line ("Création de l'arbre.");
   for Ind in Interval loop
      MonArbre.Insere (Ordre (Ind), Ordre (Ind));
      Put_Line ("Élement inséré :" & TElement'Image (Ordre (Ind)));
      MonAffArbre;
   end loop;

   Lance;
   MonArbre.Recherche (Interval'Last, Element);
   Put_Line ("Élement Recherché :" & TElement'Image (Element));
   --Stoppe;
   Put_Line (Natural'Image (Valeur) & " ms");
   Lance;
   MonArbre.Recherche (Interval'First, Element);
   Put_Line ("Élement Recherché :" & TElement'Image (Element));
   --Stoppe;
   Put_Line (Natural'Image (Valeur) & " ms");

   Put_Line ("Élimination des élements de l'arbre.");
   for Ind in reverse Interval loop
      MonArbre.Retire (Ind, Element);
      Put_Line ("Élement retiré :" & TElement'Image (Element));
      MonAffArbre;
   end loop;

   MonArbre.Detruit;
end Test_Arb_AVL;

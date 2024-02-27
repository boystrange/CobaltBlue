{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of Cobalt.                                      ║
│                                                                   ║
│ Cobalt is free software: you can redistribute it and/or modify it ║
│ under the terms of the GNU General Public License as published by ║
│ the Free Software Foundation, either version 3 of the License, or ║
│ (at your option) any later version.                               ║
│                                                                   ║
│ Cobalt is distributed in the hope that it will be useful, but     ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with Cobalt.  If not, see <http://www.gnu.org/licenses/>.   ║
│                                                                   ║
│ Copyright 2016-2017 Luca Padovani                                 ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module Render
  (subscript,
   showTag,
   showMolecule,
   showClass,
   showProcess,
   showRelation)
where

import Language
import Text.PrettyPrint
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (chr, ord)
import Data.List (intersperse)

-------------
-- SCRIPTS --
-------------

subscript :: Int -> String
subscript = map convert . show
  where
    convert :: Char -> Char
    convert ch | ch >= '0' && ch <= '9' = chr (ord ch - ord '0' + ord '₀')
               | ch == '-' = '₋'

---------------
-- UTILITIES --
---------------

embrace :: Doc -> Doc -> Doc -> [Doc] -> [Doc]
embrace open _ close [] = [open <+> close]
embrace open _ close [x] = [open <+> x <+> close]
embrace open sep close (x : xs) =
  [open <+> x] ++ map (sep <+>) (init xs) ++ [sep <+> last xs <+> close]

----------
-- TAGS --
----------

renderTag :: Tag -> Doc
renderTag (Tag Nothing tag) = text tag
renderTag (Tag (Just (line, _)) tag) = text (tag ++ subscript line)

showTag :: Tag -> String
showTag = render . renderTag

-----------
-- TYPES --
-----------

nameOfTypeVariable :: Int -> String
nameOfTypeVariable n =
  [chr $ ord 'α' + n `mod` greekLetters] ++ if n < greekLetters then ""
                                            else subscript (n `div` greekLetters)
  where
    -- how many greek letters we use before turning to subscripts
    greekLetters :: Int
    greekLetters = 12

instance Show Key where
  show (Key tag arity) | arity == 0 = show tag
                       | otherwise = show tag ++ subscript arity

showMolecule :: S.Set Key -> String
showMolecule kset = inAngles (concat (intersperse ", " (map show (S.elems kset))))
  where
    inAngles :: String -> String
    inAngles s = "⟨" ++ s ++ "⟩"

instance Show TVar where
  show (TVar n) | n < 0 = "?"
                | otherwise = nameOfTypeVariable n

renderType :: Type -> Doc
renderType = auxT
  where
    aux :: Char -> Type -> Doc
    aux _ (Var α) = text (show α)
    aux _ (Basic name) = text ("`" ++ name)
    aux _ (Ref tname) = text ('#' : tname)
    aux _ Zero = text "0"
    aux _ One = text "1"
    aux _ (Message tag ts) = auxM tag ts
    aux op (t :·: s) =
      maybeParens (elem op "*") (cat [aux '·' t <> text "·", nest 2 (aux '·' s)])
    aux op (t :+: s) =
      maybeParens (elem op "·*") (sep [aux '+' t <+> text "+", aux '+' s])
    aux _ (Star t) = text "*" <> aux '*' t

    auxT = aux '_'

    maybeParens :: Bool -> Doc -> Doc
    maybeParens False = id
    maybeParens True = parens

    auxM :: Tag -> [Type] -> Doc
    auxM tag [] = renderTag tag
    auxM tag ts = renderTag tag <> parens (sep $ punctuate comma $ map auxT ts)

instance Show Type where
  show = render . renderType

-------------
-- CLASSES --
-------------

showClass :: Name -> ClassType -> String
showClass name cls = render (vcat [text name, nest 2 (vcat methods)])
  where
    methods :: [Doc]
    methods = [ renderType (Message tag ts) | ((Key tag _), ts) <- M.toList cls ]

-----------------------------
-- RELATIONS & CONSTRAINTS --
-----------------------------

renderRelation :: Char -> Type -> Type -> Doc
renderRelation rel t s = hang (renderType t) 2 (text [rel] <+> renderType s)

renderConstraint :: Constraint -> Doc
renderConstraint = aux
  where
    aux Trivial = text "True"
    aux (UpperBound α t) = renderRelation '⊑' (Var α) t
    aux (LowerBound t s) = renderRelation '⊑' t s

instance Show Constraint where
  show = render . renderConstraint

showRelation :: Char -> Type -> Type -> String
showRelation rel t s = render (renderRelation rel t s)

---------------
-- PROCESSES --
---------------

showProcess :: Process -> String
showProcess = render . auxP
  where
    ampersand :: Doc
    ampersand = text "&"

    auxM (Pattern tag args) = renderTag tag <> auxArgs args

    auxArgs [] = empty
    auxArgs args = parens (hcat (punctuate (comma <> space) (map (uncurry auxName) args)))

    auxR (Rule msgs tags p) =
      hang (pattern <> guards <+> text "▸") 2 (auxP p)
      where
        pattern = hcat (punctuate (space <> ampersand <> space) (map auxM msgs))
        guards = if tags == [] then empty
                 else text " unless " <> hcat (punctuate (comma <> space) (map renderTag tags))

    auxP Null = text "done"
    auxP (Send u (Right t) tag exprs) = auxName u t <> text "!" <> renderTag tag <> auxEs exprs
    auxP (Send u (Left _) tag exprs) = text u <> text "!" <> renderTag tag <> auxEs exprs
    auxP p@(Parallel _ _) = sep (punctuate (space <> ampersand) (map auxP (collect p)))
    auxP (Object u kind rules p) =
      vcat ([auxNameKind u kind] ++
            embrace lbrack (text "|") rbrack (map auxR rules) ++
            [auxP p])

    auxNameKind :: Name -> Kind -> Doc
    auxNameKind u (Static cls) = text "class" <+> text u
    auxNameKind u (Dynamic t) = text "object" <+> auxName u t
    auxNameKind u (Linear t) = text "linear object" <+> auxName u t

    auxEs [] = empty
    auxEs exprs = parens (hcat $ punctuate (comma <> space) (map auxE exprs))

    auxE (Int n) = text (show n)
    auxE (Double d) = text (show d)
    auxE (String s) = text (['"'] ++ s ++ ['"'])
    auxE (Name u t) = auxName u t

    auxMaybeT :: Maybe Type -> Doc
    auxMaybeT Nothing = empty
    auxMaybeT (Just t) = space <> colon <+> renderType t

    auxMaybeC :: Maybe ClassType -> Doc
    auxMaybeC Nothing = empty
    auxMaybeC (Just cls) = space <> colon <+> auxC cls

    auxC :: ClassType -> Doc
    auxC cls =
      vcat $ punctuate comma (map (\(Key tag _, ts) -> renderType $ Message tag ts) (M.toList cls))

    auxName :: Name -> Type -> Doc
    auxName u t = text u <+> renderType t

    collect :: Process -> [Process]
    collect (Parallel p q) = collect p ++ collect q
    collect p = [p]

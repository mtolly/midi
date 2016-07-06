module Sound.MIDI.Monoid where

import Data.Monoid (Monoid, mappend, mconcat, )
import Prelude hiding (concatMap, )



infixr 5 +#+

(+#+) :: Monoid m => m -> m -> m
(+#+) = mappend


genAppend :: (Monoid m) =>
   (m -> a) -> (a -> m) -> a -> a -> a
genAppend cons decons x y =
   cons $ mappend (decons x) (decons y)

genConcat :: (Monoid m) =>
   (m -> a) -> (a -> m) -> [a] -> a
genConcat cons decons =
   cons . concatMap decons

-- foldMap
concatMap :: (Monoid m) =>
   (a -> m) -> [a] -> m
concatMap f = mconcat . map f

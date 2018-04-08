{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module NH.Misc
where
import           Control.Applicative
import           Control.Monad.Plus
import           Data.Bool
import qualified Data.Char
import qualified Data.List                        as L
import qualified Data.Map                         as Map
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text                        as T
import           Data.Text                           (Text, pack, unpack, take, drop, toLower, toUpper, length)
import           Prelude                      hiding (take, drop, length)
import           Prelude.Unicode
import qualified System.Directory                 as Sys
import qualified System.IO.Temp                   as Sys
import qualified System.FilePath                  as Sys
import qualified Text.Printf                      as T
import qualified Text.Read.Lex                    as R
import qualified Text.ParserCombinators.ReadP     as R
import           Text.PrettyPrint.HughesPJClass      (Doc, renderStyle, Mode(..), Style(..))
import qualified Turtle                           as Tu

import           GHC.Stack
import qualified Debug.Trace                      as DBG



echoT ∷ Text → IO ()
echoT = putStrLn ∘ unpack

showT ∷ Show a ⇒ a → Text
showT = pack ∘ show

readT ∷ Read a ⇒ Text → a
readT = read ∘ unpack

lowerShowT ∷ Show a ⇒ a → Text
lowerShowT = T.toLower . pack . show

errorT ∷ HasCallStack ⇒ Text → a
errorT = error . unpack

every ∷ (Bounded a, Enum a) ⇒ [a]
every = enumFromTo minBound maxBound

(.:) ∷ ∀ a f g b. (b → a) → (f → g → b) → f → g → a
(.:) = (.) ∘ (.)
infixr 9 .:

takeButLast ∷ Int → Text → Text
takeButLast n t = take (length t - n) t

revLookup ∷ (Eq a) ⇒ a → [(b,a)] → Maybe b
revLookup i = let f (p,q) = (q,p)
              in lookup i ∘ map f

-- from cognimeta-utils
ifJust ∷ Bool → a → Maybe a
ifJust = bool (const Nothing) Just

-- from cognimeta-utils
justIf ∷ a → Bool → Maybe a
justIf = flip ifJust



errNothing ∷ HasCallStack ⇒ String → Maybe a → a
errNothing errMsg = fromMaybe (error errMsg)

defineMaybe ∷ a → Maybe a → Maybe a
defineMaybe x Nothing = Just x
defineMaybe _ y       = y



-- XXX: factor
readNames ∷ Text → [Text]
readNames raw = loop [] (unpack raw)
  where
    loop acc s =
      case (s, R.readP_to_S R.hsLex s) of
        ("", _)         -> reverse acc
        (_, (a, rem):_) -> loop (pack a:acc) rem

readSequence ∷ Read a ⇒ Text → [a]
readSequence raw = loop [] (unpack raw)
  where
    loop acc s =
      case (s, reads s) of
        ("", _)         -> reverse acc
        (_, (a, rem):_) -> loop (a:acc) rem

type SimpleToken a = (Bounded a, Enum a, Read a, Show a)

diagReadCaseInsensitive ∷ HasCallStack ⇒ SimpleToken a ⇒ Text → Maybe a
diagReadCaseInsensitive str = diagRead $ T.toLower str
  where mapping    = Map.fromList [ (lowerShowT x, x) | x <- enumFromTo minBound maxBound ]
        diagRead x = Just $ flip fromMaybe (Map.lookup x mapping)
                     (error $ T.printf ("Couldn't parse '%s' as one of: %s")
                                       str (unpack $ T.intercalate ", " $ Map.keys mapping))



newtype Desc  = Desc  Text deriving (Show)
newtype Exec  = Exec  Text deriving (Show)
newtype ShCmd = ShCmd Text deriving (Show)

stdoutCall ∷ HasCallStack ⇒ Desc → Exec → [Text] → IO Text
stdoutCall (Desc desc) (Exec cmd) args = do
  result ← Tu.procStrictWithErr cmd args empty
  pure $ case result of
           (Tu.ExitSuccess, out, _) → out
           (_, _, err)              → errorT ("Failed to " <> desc <> " ('" <> cmd <> " " <> T.intercalate " " args <> "'): " <> err)

stdoutCallSh ∷ HasCallStack ⇒ Desc → ShCmd → IO Text
stdoutCallSh (Desc desc) (ShCmd cmd) = do
  result ← Tu.shellStrictWithErr cmd empty
  pure $ case result of
           (Tu.ExitSuccess, out, _) → out
           (_, _, err)              → errorT ("Failed to " <> desc <> " ('" <> cmd <> "'): " <> err)



{-# INLINE charMap #-}
charMap ∷ Char → Char → Char → Char
charMap from to ((≡ from) → True) = to
charMap from to  x                = x

showDocOneLine ∷ Doc → Text
showDocOneLine = pack ∘ renderStyle (Style OneLineMode 1 1)



-- * Flag machinery
class (Bounded (Flag a), Eq (Flag a)) ⇒ CFlag a where
  data Flag a
  toBool ∷ (Flag a) → Bool
  toBool = (≡ enabled)
  fromBool ∷ Bool → (Flag a)
  fromBool x = if x then minBound else maxBound
  enabled, disabled ∷ (Flag a)
  enabled  = minBound
  disabled = maxBound
  opposite ∷ Flag a → Flag a
  opposite = fromBool . not . toBool
  flagIf ∷ (Flag a) → b → b → b
  flagIf f true false = if toBool f then true else false
  -- XXX: most should be de-TC-ised,
  -- however, using TC's as poor-man's modules is so alluring..

enabledIsJust ∷ CFlag b ⇒ a → Flag b → Maybe a
enabledIsJust x (toBool → True) = Just x
enabledIsJust _ _               = Nothing

-- flag ∷ Flag a ⇒ a → ArgName → Char → Optional HelpMessage → Parser a
-- flag effect long ch help = (\case
--                                True  → effect
--                                False → opposite effect) <$> switch long ch help



removeFileIfExists ∷ Text → IO ()
removeFileIfExists fpath =
  Sys.doesFileExist (unpack fpath) >>=
  (flip when $
    Sys.removeFile (unpack fpath))

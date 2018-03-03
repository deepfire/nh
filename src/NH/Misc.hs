{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module NH.Misc
where
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text                        as T
import           Data.Text                           (Text, pack, unpack)
import           Prelude.Unicode
import qualified Text.Read.Lex                    as R
import qualified Text.ParserCombinators.ReadP     as R



showT ∷ Show a ⇒ a → T.Text
showT = pack . show

readT ∷ Read a ⇒ T.Text → a
readT = read . unpack

lowerShowT ∷ Show a ⇒ a → T.Text
lowerShowT = T.toLower . pack . show

errorT ∷ T.Text → a
errorT = error . unpack

every ∷ (Bounded a, Enum a) ⇒ [a]
every = enumFromTo minBound maxBound

(.:) ∷ ∀ a f g b. (b → a) → (f → g → b) → f → g → a
(.:) = (.) ∘ (.)
infixr 9 .:



errNothing ∷ String → Maybe a → a
errNothing errMsg = fromMaybe (error errMsg)



-- XXX: factor
parseNames ∷ Text → [Text]
parseNames raw = loop [] (unpack raw)
  where
    loop acc s =
      case (s, R.readP_to_S R.hsLex s) of
        ("", _)         -> reverse acc
        (_, (a, rem):_) -> loop (pack a:acc) rem

parseSequence ∷ Read a ⇒ Text → [a]
parseSequence raw = loop [] (unpack raw)
  where
    loop acc s =
      case (s, reads s) of
        ("", _)         -> reverse acc
        (_, (a, rem):_) -> loop (a:acc) rem

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module NH.FS
where

import           Data.Text                           (Text, pack, unpack)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as Sys
import           Prelude.Unicode
import qualified System.Directory                 as Sys
import qualified System.FilePath                  as Sys

(</>) ∷ Text → Text → Text
l </> r = l <> T.singleton Sys.pathSeparator <> r

(<.>) ∷ Text → Text → Text
l <.> r = l <> T.singleton '.' <> r

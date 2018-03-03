{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
module NH.Github
where

import           Data.Char
import           Data.Foldable
import qualified Data.List                        as L
import           Data.Maybe
import           Data.String
import           Data.Text                           (pack, unpack)
import qualified Data.Text                        as T
import           Data.Text.Format             hiding (print)

import           NH.Types
import           NH.Config



githubURLComponents ∷ URL → (GithubUser, RepoName)
githubURLComponents (URL text) =
  let pieces = T.splitOn "/" text
  in (,)
     (GithubUser $ pieces !! 3)
     (RepoName   $ pieces !! 4)

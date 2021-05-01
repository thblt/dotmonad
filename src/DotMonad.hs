{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : DotMonad
Description : Yet another dotfiles management system.
Copyright   : © 2021 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental

-}

module DotMonad ( Action (..)
                , Rule (..)
                , ifThenElse
                , DotMonad.when
                , DotMonad.unless
                , ifMacOS
                , ifLinux
                , ifWindows
                , ifPosix ) where

import           Control.Monad.Writer
import           Data.List            (isPrefixOf)
import           Data.Semigroup       ((<>))
import           Data.Tree
import           Network.HostName     (getHostName)
import           Options.Applicative
import           System.FilePath
import           System.Info

data Action = Symlink FilePath FilePath
            | MkDir FilePath
            | Unlink FilePath
            deriving (Show, Eq, Ord)

class Rule d where
  compile :: d -> Dotfiles ()

-- | A simple file
data SimpleFile = Dotfile
  { -- | The source path to this file, related to ~/.dotfiles
    dfSource    :: Dotfiles FilePath
    -- | A computation that returns the target path relative to ~.
  , dfTarget    :: Dotfiles FilePath
  , dfCondition :: Dotfiles Bool }

instance Rule SimpleFile where
  compile d = do
    c <- dfCondition d
    s <- dfSource d
    t <- dfTarget d
    tell $ [Symlink t s | c]

instance {-# OVERLAPPING #-} Rule String where
  compile = symlink

instance {-# OVERLAPPING #-} Rule (Dotfiles ()) where
  compile = void

instance {-# OVERLAPPABLE #-} (Foldable f, Rule d) => Rule (f d) where
  compile = foldM (const compile) ()

-- | The simplest transformation. Always symlink source to target.
symlink' :: String -> SimpleFile
symlink' p = Dotfile { dfSource = return p
                     , dfTarget = return p
                     , dfCondition = return True }

-- | Compiles a rule that symlinks from dotfiles to home, under the
-- same name.
symlink :: String -> Dotfiles ()
symlink = compile . symlink'

-- | Compiles a rule that symlinks everything under a directory, but
-- not the directory itself.
cherrypick :: String -> Dotfiles ()
cherrypick = undefined

-- | Replace all occurences of key by value in string.
replaceString :: String -> String -> String -> String
replaceString k v s = step s
  where
    l = length k
    step :: String -> String
    step [] = []
    step s@(x:xs) | k `isPrefixOf` s = v ++ step (drop l s)
                  | otherwise = x:step xs

-- | A templated transformation.  Replaces k with the current hostname
-- in t.
dotTpl :: String -> String -> Dotfiles FilePath
dotTpl k t =
    liftIO
    ( do
        hn <- getHostName
        return $ replaceString k hn t)

-- * Conditional rules

-- | Compile a rule that executes cond, then ifR or elseR.
ifThenElse :: Dotfiles Bool -> Dotfiles () -> Dotfiles () -> Dotfiles ()
ifThenElse c a b = do
  x <- c
  if x then a else b
  -- @FIXME The test shouldn't be able to write anything (except by
  -- abusing IO)

-- | The utility rule
noop :: Dotfiles ()
noop = return ()

-- | Like If without else.
when :: Dotfiles Bool -> Dotfiles () -> Dotfiles ()
when c a = ifThenElse c a noop

-- | Like If without else.
unless :: Dotfiles Bool -> Dotfiles () -> Dotfiles ()
unless c a = ifThenElse c noop a

-- | Run rule iff the current system is GNU/Linux.
ifLinux :: Dotfiles () -> Dotfiles ()
ifLinux = undefined

-- | Run rule iff the current system is MacOS.
ifMacOS :: Dotfiles () -> Dotfiles ()
ifMacOS = undefined

-- | Run rule iff the current system is Windows
ifWindows :: Dotfiles () -> Dotfiles ()
ifWindows = undefined

-- | Run rule iff the current system is some sort of Unix.
ifPosix :: Dotfiles () -> Dotfiles ()
ifPosix = undefined

-- * Command-line interface

data CLI = CLI
  { dfDir :: FilePath
  , homeDir :: FilePath
  , dryRun :: Bool }

parser :: Parser CLI
parser = CLI <$> strOption
          ( long "dotfiles"
         <> short 'd'
         <> metavar "SOURCE"
         <> help "Target for the greeting" )
      <*> strOption
          ( long "home"
          <> metavar "HOME"
         <> help "Home directory." )
      <*> switch
          ( long "dry-run"
         <> help "Don't actually do anything.")

-- * Core

type Dotfiles = WriterT [Action] IO

-- | Runs a Rule under IO.
runRule :: (Rule r) => r -> IO ()
runRule d = do
  pairs <- execWriterT . compile $ d
  print pairs

main :: IO ()
main = runRule $ do
  symlink ".mbsyncrc"
  symlink ".zshrc"
  symlink ".notmuch-config"
  symlink ".profile"
  symlink ".tmux.conf"
  symlink ".mail/.notmuch/hooks"
  cherrypick ".cabal"
  cherrypick ".gnupg"
  cherrypick ".local/bin"
  cherrypick ".local/share/applications"
  cherrypick ".config"

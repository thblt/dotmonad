{-|
Module      : DotMonad.Tutorial
Description : A tutorial for DotMonad.
Copyright   : © 2021 Thibault Polge
License     : GPL-3
Maintainer  : thibault@thb.lt
Stability   : experimental

The preferred way of using DotMonad is through @stack --script@, with
your preferred resolver.  Here's what a basic dotmonad script can look
like.

@
#!\/usr\/bin\/env stack
-- stack --resolver lts-17.10 script

import DotMonad

main = dotmonad $
  symlink ".zshrc"
  symlink ".tmux"
@
-}

module DotMonad.Tutorial where

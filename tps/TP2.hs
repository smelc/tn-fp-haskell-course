-- Build me with: cabal build TP2.hs
-- Execute me with: cabal run -v0 TP2.hs

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "TP2 is running"

-- If you don't like this TP, you can do TP3 instead.

-- Implement a card game a la Magic the Gathering. Each player
-- has 3 spots available to play cards like this:
--
--       Player 1
--
--  card1   card2   card3
--
--
--  carda   cardb   cardc
--
--       Player2
--
-- Each player starts with a deck of cards. When it is the turn of a
-- player, he can take cards from his hand and put them on the board.
-- After that, the turn resolves: each card of the player attacks the
-- card in front of it. If a player has no card in front of the attacking
-- card, then the attack contributes to the player's score. Each card
-- has two stats: its hitpoints and its attack. When a card attacks, it
-- deals the corresponding hitpoint to the opponent card.
-- Consider the diagram below:
--
--      Player 1
--
-- knight   empty   soldier
--
-- empty    soldier soldier
--
--      Player 2
--
-- with the knight having 2 hitpoints and 2 attacks, and the soldier having 1 hitpoint
-- and 1 attack. In this scenario, when player 1 attacks, the left knight
-- contributes two to the score while the right soldier kills its opponent.
--
-- The game has three possible cards: the knight, the soldier and the
-- sorceress. The sorceress gives +1hp and +1attack to its allies. When the game
-- starts, each player starts with two copies of each cards.
--
-- Exercise:
--
-- 1. Define a type for cards
--    - Define a Show instance for this type
-- 2. Define a type for the part of a player. Make it implement Show.
-- 3. Define a type for the whole board: the two players parts. Make it implement Show.
-- 4. Write a function making a player play
-- 5. Write a function playing an entire game
--    Display the list of boards while the game runs.
--
-- If you want to have randomness, use the 'random' function here:
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#v:random
-- Use mkStdGen to obtain a value that satisfies the constraint "RandomGen g":
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#t:StdGen

-- Build me with: cabal build TP6.hs
-- Execute me with: cabal run -v0 TP6.hs
--
-- This lab exercice uses the following library:
--
-- - 'gloss' for rendering simple graphics. See its documentation here:
-- https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html
--
-- Goals:
--
-- 1. Write an OpenGL Pong game using the gloss library. The two players
--    play on the same keyboard.
-- 2. Make the game work over the network: one player is the server, the second
--    one connects to the server and then the game begins.
--
-- -----------------------------------
-- WARNING YOU NEED TO DO SOMETHING! -
-- -----------------------------------
--
-- To avoid requiring installation OpenGL drivers during the first TPs,
-- this one is not active by default. So you MUST:
--
-- 1. Uncomment the lines concerning TP6.hs at the end of the file tn-tp-course.cabal at the repository root.(<=)
-- 2. You need to install OpenGL libraries on your machine:
--
--    sudo apt install libgl1-mesa-dev freeglut3 freeglut3-dev
-- 3. Then you can run 'cabal build TP6.hs' at the repository root and it should work.
--
-- -----------
-- END WARNING
-- -----------

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Data carried out all along the game
data PongGame = PongGame
  { {- Your fields here -}
  } deriving Show

initialState :: PongGame
initialState = undefined

main :: IO ()
main = play
  undefined
  undefined
  60 -- Frames per second
  initialState
  render
  undefined
  undefined

-- Render the game state
render :: PongGame -> Picture
render _ = undefined
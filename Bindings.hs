module Bindings (idle,display,reshape,keyboardMouse) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
 
import Display
 
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)
 
keyboardAct delta (Char '+') Down = do
  delta' <- get delta
  delta $= 2*delta'
keyboardAct delta (Char '-') Down = do
  delta' <- get delta
  delta $= delta'/2
keyboardAct _ _ _ = return ()
 
keyboardMouse delta key state modifiers position = do
  keyboardAct delta key state
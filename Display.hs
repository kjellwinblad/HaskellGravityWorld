module Display (display,idle) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
 
import Cube
import Points
import Planet

display :: IORef [PlanetT] -> IO ()
display planets = do 
  clear [ColorBuffer,DepthBuffer] 
  loadIdentity
  preservingMatrix $ do 
    ps <- get planets
    scale 0.3 0.3 (0.01::GLfloat)
    mapM_ (\(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
      translate $ Vector3 x y z
      cube (0.1::GLfloat)
      color $ Color3 (0.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat) --set outline color to black
      cubeFrame (0.1::GLfloat) --draw the outline
      ) $ to_points ps
  swapBuffers


idle :: IORef [PlanetT] -> IORef GLfloat -> IO ()
idle planets delta = do
  ps <- get planets
  d <- get delta
  planets $= (update ps d)
  postRedisplay Nothing
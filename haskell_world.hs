import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
 
import Bindings
import Planet as P 
  
  
main = do
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer,DoubleBuffered] -- add a depth buffer
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer
  planets <- newIORef [(Planet 
                        (P.Position 0.0 0.0 0.0) 
                        (Velocity 0.0 0.0 0.0) 
                        (Radius 10.0) 
                        (Weight 10000000.0)),
                       --(Planet 
                        --(P.Position (-1.0) 0.0 0.0) 
                        --(Velocity 0.0 0.0 0.0) 
                        --(Radius 10.0) 
                        --(Weight 1000000000.0)),
                       --(Planet 
                        --(P.Position 0.0 (1.0) 0.0) 
                        --(Velocity 0.0 0.0 0.0) 
                        --(Radius 10.0) 
                        --(Weight 100000000.0)),
                       (Planet 
                        (P.Position 0.0 (3) 0.0) 
                        (Velocity (-13.5) (0.0) 0.0) 
                        (Radius 10.0) 
                        (Weight 100.0))]
  delta <- newIORef 0.0001 
  keyboardMouseCallback $= Just (keyboardMouse delta)
  idleCallback $= Just (idle planets delta)
  displayCallback $= (display planets)
  mainLoop
module Planet where

import Graphics.Rendering.OpenGL

import Data.List

import Debug.Trace

data PositionT = Position GLfloat GLfloat GLfloat deriving (Show, Eq)  

data VelocityT = Velocity GLfloat GLfloat GLfloat deriving (Show, Eq) 

data RadiusT = Radius GLfloat deriving (Show, Eq) 

data WeightT = Weight GLfloat deriving (Show, Eq) 

data PlanetT = Planet PositionT VelocityT RadiusT WeightT deriving (Show, Eq)


to_position :: (GLfloat, GLfloat, GLfloat) -> PositionT
to_position (x, y, z) = Planet.Position x y z

difference :: PositionT -> PositionT -> (GLfloat, GLfloat, GLfloat)
difference (Planet.Position a1 a2 a3) (Planet.Position b1 b2 b3) =
  (b1 - a1, b2 - a2, b3 - a3)
  
add_dist_to_pos :: PositionT -> (GLfloat, GLfloat, GLfloat) -> PositionT
add_dist_to_pos (Planet.Position a1 a2 a3) (b1, b2, b3) =
  Planet.Position (b1 + a1) (b2 + a2) (b3 + a3)
  
distance :: PositionT -> PositionT -> GLfloat
distance p1 p2 =
  let (x, y, z) = difference p1 p2 
  in sqrt(x*x + y*y + z*z)
     
unit_vector :: (GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat)
unit_vector vector =
  let (x, y, z) = vector
  in (x/distanceToOrigo, y/distanceToOrigo, z/distanceToOrigo)
  where distanceToOrigo = distance (Planet.Position 0.0 0.0 0.0) (to_position vector) 

to_points :: [PlanetT] -> [(GLfloat,GLfloat,GLfloat)]
to_points [] = []
to_points ((Planet (Planet.Position x y z) _ _ _):rest) = 
  (x, y, z):(to_points rest)

gravity_force_vecor :: PlanetT -> [PlanetT] -> (GLfloat,GLfloat,GLfloat)
gravity_force_vecor planet other_planets =
  foldl gravity_force_fun (0.0,0.0,0.0) other_planets
  where 
    gravity_constant = 0.0001
    (Planet 
     pos2 _ _ (Weight m2)) = planet 
    gravity_force_fun (xf, yf, zf) other_planet =
      let (Planet 
           pos1 _ _ (Weight m1)) = other_planet
          dist = distance pos1 pos2
          (u1, u2, u3) = unit_vector (difference pos1 pos2) 
          factor = (-1) * gravity_constant * ((m1 * m2) / (dist*dist))
      in (xf + u1 * factor, yf + u2 * factor, zf + u3 * factor)
         
         
update :: [PlanetT] -> GLfloat-> [PlanetT]  
update planets delta = 
  map update_planet planets
  where
    update_planet planet = 
      let other_planets = delete planet planets
          (Planet 
           old_pos (Velocity ovx ovy ovz) r (Weight m)) = planet
          (fx, fy, fz) = gravity_force_vecor planet other_planets
          (ax, ay, az) = (fx/m, fy/m, fz/m)
          (nvx, nvy, nvz) = (ovx + ax * delta, ovy + ay * delta, ovz + az * delta)
          new_vel = Velocity nvx nvy nvz
          new_pos = add_dist_to_pos old_pos (nvx * delta, nvy * delta, nvz * delta)
      in Planet new_pos new_vel r (Weight m)
          
          
       
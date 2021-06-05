module Projectile

open Tuple

type Projectile = { Position: Tuple; Velocity: Tuple }
type Environment = { Gravity: Tuple; Wind: Tuple }

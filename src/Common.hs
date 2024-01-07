module Common where

data Coordinate a = 
  Coordinate {
              p1 :: a,
              p2 :: a
             }
             deriving(Show, Eq)

-- Place for things I feel will be useful in the future

cartesian_product :: [a] -> [b] -> [(a,b)]
cartesian_product x y = [(x', y') | x' <- x, y' <- y]

points_surrounding_coords :: (Num a, Eq a, Enum a) => Coordinate a -> [Coordinate a]
points_surrounding_coords (Coordinate x y) = [Coordinate x' y' | x' <- [x-1 .. x+1], y' <- [y-1 .. y+1]]

points_surrounding_coords_limit :: (Num a, Enum a, Ord a) => (Coordinate a, Coordinate a) -> Coordinate a -> [Coordinate a]
points_surrounding_coords_limit  (lowerbound, upperbound) (Coordinate x y) = 
  [
  Coordinate x' y' 
    | x' <- [x-1 .. x+1], x' <? (p1 lowerbound, p1 upperbound)
    , y' <- [y-1 .. y+1], y' <? (p2 lowerbound, p2 upperbound)
  ]

-- Given a box defined by two points, return all points around the box within a limit, including the box
points_surrounding_box_limit :: (Num a, Enum a, Ord a) => (Coordinate a, Coordinate a) -> (Coordinate a, Coordinate a) -> [Coordinate a]
points_surrounding_box_limit (lowerbound, upperbound) (lowerbox, upperbox) = 
  [
  Coordinate x' y'
    | x' <- [lower_x-1 .. upper_x+1], x' <? (p1 lowerbound, p1 upperbound)
    , y' <- [lower_y-1 .. upper_y+1], y' <? (p2 lowerbound, p2 upperbound)
  ]
    where lower_x = p1 lowerbox
          upper_x = p1 upperbox
          lower_y = p2 lowerbox
          upper_y = p2 upperbox

-- Convinience method for checking range
-- lifted from SO
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

-- f
f :: Float -> Float -> Float
f x y = 5*y^6*x+2*y^2*x-4*x^2*y+3*x-4*(y*x^5)

-- df/dx
dx :: Float -> Float -> Float
dx x y = 5*y^6 - 20*x^4*y - 8*x*y + 2*y^2 + 3

-- df/dy
dy :: Float -> Float -> Float
dy x y = 30*x*y^5 - 4*x^5 - 4*x^2 + 4*x*y

-- get next coordinate given the current position and a delta
next :: Float -> Float -> Float -> (Float, Float)
next x0 y0 delta =
  let slopeX = dx x0 y0 in
  let slopeY = dy x0 y0 in
  let dynamic_delta = delta / (sqrt ( slopeX^2 + slopeY^2 )) in
  let deltaX = (slopeX * dynamic_delta) in
  let deltaY = (slopeY * dynamic_delta) in
  let x1 = x0 + deltaX in
  let y1 = y0 + deltaY in
  let x2 = x0 - deltaX in
  let y2 = y0 - deltaY in
  if f x1 y1 < f x2 y2
    then (x1, y1)
    else (x2, y2)

path :: Float -> Float -> Float -> Int -> [(Float, Float)]
path x0 y0 delta limit =
  path' x0 y0 delta limit [(x0, y0)]

path' :: Float -> Float -> Float -> Int -> [(Float, Float)] -> [(Float, Float)]
path' x0 y0 delta limit previous =
  if limit <= 0
    then previous
  else
    let coord = next x0 y0 delta in
    if coord == (x0, y0)
      then previous
      else path' (fst coord) (snd coord) delta (limit - 1) (previous ++ [coord])

wolframArray :: [String] -> String
wolframArray array = wolframArray' array ""

wolframArray' :: [String] -> String -> String
wolframArray' array string =
  if length array == 0
    then "{" ++ string ++ "}"
    else
      if length string > 0
        then wolframArray' (drop 1 array) (string ++ "," ++ (array !! 0))
        else wolframArray' (drop 1 array) (array !! 0)

wolframPoint :: (Float, Float) -> String
wolframPoint point = "{" ++ (show $ fst point) ++ "," ++ (show $ snd point) ++ "}"

wolframString :: [(Float, Float)] -> String
wolframString points = wolframArray $ map wolframPoint points
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = (i * l) + (j * m) + (k * n)

vmult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

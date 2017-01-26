-----------------
-- Typeclasses --
-----------------

Numeric = forall a . {
  multiply : a -> a -> a
}

Squarable = forall a . {
  square : a -> a
}

---------------
-- Instances --
---------------

-- Numeric Int
provide {
  multiply = multiplyInt
}

-- Squarable Int
provide {
  square = x -> @.multiply x x
}

--------------
-- Examples --
--------------

-- forall a . implicit Squarable a . a -> a
tesseract = x -> @.square (@.square x)

-- forall a . implicit Squarable a . a -> a
sixteenth = x -> tesseract (tesseract x)

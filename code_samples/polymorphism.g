-----------------
-- Typeclasses --
-----------------

Numeric = {
  multiply : a -> a -> a
}

multiply = @.multiply

Squarable = {
  square : a -> a
}

square = @.square

---------------
-- Instances --
---------------

provide {
  multiply = multiplyInt
}

provide {
  square = x -> multiply x x
}

--------------
-- Examples --
--------------

-- forall a . implicit Squarable a . a -> a
tesseract = x -> square (square x)

-- forall a . implicit Squarable a . a -> a
sixteenth = x -> tesseract (tesseract x)

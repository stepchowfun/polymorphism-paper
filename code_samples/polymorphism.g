-----------------
-- Typeclasses --
-----------------

Numeric = forall a . {
  multiply : a -> a -> a
}

multiply = @(Numeric).multiply

Squarable = forall a . {
  square : a -> a
}

square = @(Squarable).square

---------------
-- Instances --
---------------

provide {
  multiply = multiplyInt
} as Numeric Int

provide {
  square = x -> multiply x x
} as Squarable Int

--------------
-- Examples --
--------------

tesseract = x -> square (square x)

sixteenth = x -> tesseract (tesseract x)

-----------------
-- Typeclasses --
-----------------

Numeric = forall a . {
  multiply : a -> a -> a
}

multiply = @(Numeric a).multiply

Squarable = forall a . {
  square : a -> a
}

square = @(Squarable a).square

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

tesseract = x -> square (square x)

sixteenth = x -> tesseract (tesseract x)

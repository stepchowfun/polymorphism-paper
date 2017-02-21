-----------------
-- Typeclasses --
-----------------

Numeric = forall a . {
  multiply : a -> a -> a
}

multiply : forall a . Numeric a => a -> a -> a
multiply = @.multiply

Squarable = forall a . {
  square : a -> a
}

square : forall a . Numeric a => a -> a -> a
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

tesseract : forall a . Squarable a => a -> a
tesseract = x -> square (square x)

sixteenth : forall a . Squarable a => a -> a
sixteenth = x -> tesseract (tesseract x)

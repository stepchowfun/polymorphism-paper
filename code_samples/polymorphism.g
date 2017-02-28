-----------------
-- Typeclasses --
-----------------

Numeric = forall a . {
  multiply : a -> a -> a
}

multiply = @(Numeric a).multiply

Squarable = forall a . Numeric a => {
  square : a -> a
}

square = @(Squarable a).square

---------------
-- Instances --
---------------

provide {
  multiply = multiplyInt
} as Numeric Int

provide {
  square = x -> multiply x x
} as forall a . Numeric a => Squarable a

--------------
-- Examples --
--------------

tesseract = x -> square (square x)

sixteenth = x -> tesseract (tesseract x)

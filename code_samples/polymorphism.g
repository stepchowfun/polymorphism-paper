-- Typeclasses

Numeric = (a : *) -> {
  multiply : a -> a -> a
}

Squarable = (a : *) -> {
  square : a -> a
}

-- Instances

provide {
  multiply = (x : Integer) -> (y : Integer) -> ...
}

given (a : *) (instance : Numeric a) provide {
  square = x -> instance.multiply x x
}

-- Examples

tesseract : implicit (a : *) -> implicit (Squarable a) -> a -> a
tesseract a instance x = instance.square $ instance.square x

sixteenth x = tesseract $ tesseract x
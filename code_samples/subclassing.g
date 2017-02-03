Eq = forall a . {
  eq : a -> a -> a
}

Addable = forall a . Eq a -> {
  merge (Eq a)
  add : a -> a -> a
}

provide {
  eq = equateInts
}

-------------------
-- Without given --
-------------------
-- What if addInts uses an @? It would get resolved too early.
addableInt = {
  add = addInts
}

provide Addable @ addableInt

----------------
-- With given --
----------------
given @ provide {
  add = addInts
}

To make an instance of a subclass, you apply the function (`Addable`) to `@`. This pulls an instance of the superclass from the implicit context. There are two options now though, don't use given, and use given. Here's an idea for doing this without `given`. I think it doesn't work, but maybe it can be made to work.

-------------------
-- Without given --
-------------------
-- Not sure how to infer the type of inst.
addableInt = inst -> {
  add = addInts
}

provide addableInt @

If `given` is reintroduced, this gets much nicer. 
----------------
-- With given --
----------------
given @ provide {
  add = addInts
}
<Paste>

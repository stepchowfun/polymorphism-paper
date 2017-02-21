let
  x = @
in
  provide
    \x -> 1
  in
    provide
      \x -> x 0
    in
      provide
        0
      in
        @ @
      end
    end
  end
end

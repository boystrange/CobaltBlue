// non-contractive mutually recursive type definitions

type #A = Foo + #B
and  #B = Bar + #A
in

done


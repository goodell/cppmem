open Nat_num

type 'a vector = Vector of  'a array * num

val vconcat : 'a vector -> 'a vector -> 'a vector 

val vmap : ('a ->'b) ->  'a vector  -> 'b vector 

val vfold :  ('b -> 'a -> 'b) -> 'b -> 'a vector -> 'b

val vzip : 'a vector -> 'b vector -> ('a * 'b) vector 

val vmapacc : ('a -> 'c -> ('b * 'c)) -> 'a vector -> 'c -> ('b vector) * 'c

val vmapi : (num -> 'a -> 'b) -> 'a vector -> 'b vector

val extend : 'a -> num -> 'a vector -> 'a vector

val duplicate : 'a vector -> 'a vector

val vlength : 'a vector -> num

val vector_get : 'a vector -> num -> 'a

val vector_slice : 'a vector -> num -> num -> 'a vector

val make_vector : 'a list -> num -> 'a vector


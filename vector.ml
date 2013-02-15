open Nat_num

type 'a vector = Vector of 'a array * num

let vconcat (Vector (a, a_l)) (Vector (b, b_l)) = Vector(Array.append a b, a_l + b_l)

let vmap f (Vector (a,a_l)) = Vector(Array.map f a, a_l)

let vfold f base (Vector (a,a_l)) = Array.fold_left f base a

let vzip (Vector (a,l)) (Vector (b,_)) =
    Vector( Array.of_list (List.combine (Array.to_list a) (Array.to_list b)), l)

let vmapacc f (Vector(a,l)) base =
  let rec mapacc vl b = match vl with
         | [] -> ([],b)
         | v::vl -> let (v',b') = f v b in 
                    let (vl',b'') = mapacc vl b' in
                    (v'::vl',b'') in
  let vls,b = mapacc (Array.to_list a) base in
  Vector((Array.of_list vls),l),b

let vmapi f (Vector (a,al)) = Vector((Array.mapi f a),al)

let extend default size (Vector (a,al)) = Vector(Array.append (Array.make size default) a, al+size)

let duplicate (Vector(a,al)) = Vector(Array.append a (Array.copy a), 2*al)

let vlength (Vector(a,al)) = al

let vector_get (Vector(a,_)) n = a.(n)

let vector_slice (Vector(a,l)) n1 n2 = Vector(Array.sub a n1 n2, (l - (n2 - n1)))

let make_vector vs l = Vector(Array.of_list vs, l)
type num = int
let (<) = (<)
let (<=) = (<=)
let (>) = (>)
let (>=) = (>=)
let (+) = (+)
let (-) x y =
  let d = x - y in
    if d < 0 then
      0
    else
      d
let ( * ) = ( * )
let (/) = (/)
let (mod) = (mod)
let (land) = (land)
let (lor) = (lor)
let (lxor) = (lxor)
let lnot = lnot
let (lsl) = (lsl)
let (lsr) = (lsr)
let (asr) = (asr)
let string_of_num n = string_of_int n



type t

val precompute: string list -> string -> t

(* In [fill t assoc], [assoc] is an association list whose keys MUST match the
   argument of the earlier call to [precompute] which created the template
   [t]. *)
val fill: t -> (string * string) list -> string

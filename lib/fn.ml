let id x = x

(** [f << g] composes [f] with [g] right-to-left -- i.e. [f (g x)] *)
let ( << ) f g x = f (g x)

(** [f << g] composes [f] with [g] left-to-right -- i.e. [g (f x)] *)
let ( >> ) f g x = g (f x)

(** [on b u x y] runs the binary function [b] on the results of applying unary function [u] to two 
    arguments [x] and [y]. From the opposite perspective, it transforms two inputs and combines the
    outputs. *)
let on (b : 'b -> 'b -> 'c) (u : 'a -> 'b) (x : 'a) (y : 'a) : 'c =
  b (u x) (u y)

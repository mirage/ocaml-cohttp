

(* This should be Monad.Basic from core, once Monad.Basic.t is covariant. *)
module type Monad =
sig
  type +'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>)  : 'a t -> 'b t -> 'b t
end

(* The parameterised monad interface [Atkey, 2009].

   This interface generalises MONAD to give more precise types to
   computations.  In addition to the type parameter used to represent the
   result type, the type of computations now has two additional parameters.  A
   computation

      (p, q, a) t

   performs some effects which transform some state from type 'p' to type 'q',
   then returns a result of type 'a'.
*)
module type PMonad =
sig
  (* The type of computations.  The variance annotations reflect the fact that
     'p is an input type and 'q an output type.  As with vanilla monads, the
     result type 'a is also an output type. *)
  type (-'p, +'q, +'a) t

  (* The pure computation constructed by 'return' performs no effects, so the
     input and output parameters are the same. *)
  val return  : 'a -> ('m, 'm, 'a) t

  (* Bind composes state-transforming computations.  It builds a computation
     that transforms the state from 'l' to 'n' from computations that
     transform 'l' to 'm' and 'm' to 'n'.  *)
  val (>>=) :  ('l, 'm, 'a) t -> ('a -> ('m, 'n, 'b) t) -> ('l, 'n, 'b) t

  (* "Then" composes state-transforming computations.  It builds a computation
     that transforms the state from 'l' to 'n' from computations that
     transform 'l' to 'm' and 'm' to 'n'.  *)
  val (>>) :  ('l, 'm, 'a) t -> ('m, 'n, 'b) t -> ('l, 'n, 'b) t
end

(* The parameterised state monad interface.  As with the vanilla state monad,
   this represents computations that read and write a single piece of state.
   Unlike the vanilla state monad, the additional type parameters allow the
   type of the state to change over the course of a computation. 
*)
module type PState =
sig
  include PMonad

  (* Read and return the state, leaving it unchanged. *)
  val get : ('a, 'a, 'a) t

  (* Replace the state with the argument. *)
  val put : 'a -> (_, 'a, unit) t

  (* Run a state-transforming computation, returning the result and the final
     state *)
  val run : ('initial, 'final, 'result) t -> 'initial -> 'result * 'final
end

(* Parameterised state monad interface, implemented as a monad transformer for
   generality.  The (non-transformer) parameterised state monad can be recovered
   by applying PStateT to the identity monad.  The non-parameterised state monad
   can be recovered by instantiating the input and output parameters of t with
   the state type. *)
module PStateT (M : Monad) :
sig
  include PState with type (-'initial, +'final, +'a) t = 'initial -> ('a * 'final) M.t

  (* Lift computations from the underlying monad. *)
  val lift : 'a M.t -> ('b, 'b, 'a) t

  (* The type of 'run' is tweaked; running a PStateT computation now returns a
     computation in the underlying monad. *)
  val run : ('initial, 'final, 'result) t -> 'initial -> ('result * 'final) M.t
end =
struct
  type (-'initial, +'final, +'a) t = 'initial -> ('a * 'final) M.t
  let return v s = M.return (v, s)
  let (>>=) m k s = M.(m s >>= fun (a, s') -> k a s')
  let (>>) m n = m >>= fun _ -> n
  let lift m s = M.(m >>= fun a -> M.return (a, s))
  let get s = M.return (s, s)
  let put s _ = M.return ((), s)
  let run k s = k s
end

(* Restrict the type of 'run' to particular initial and final states *)
module PStateT'
  (S : sig type initial and final end)
  (M : Monad) :
sig
  include module type of PStateT (M)
  val run : (S.initial, S.final, 'a) t -> S.initial -> ('a * S.final) M.t
end = PStateT(M)

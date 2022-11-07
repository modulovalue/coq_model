(* A binary tree with nullable children. *)
Module Tree1.
  Inductive Nullable (T : Type) : Type :=
    | Some(t: T)
    | None.
  
  Inductive Tree : Type :=
    | Root (left: Nullable Tree) (right: Nullable Tree)
    | Leaf.
End Tree1.

(* A binary tree with explicit children. *)
Module Tree2.
  Inductive Tree : Type :=
    | Root (left: Tree) (right: Tree)
    | Lefty (left: Tree)
    | Righty (right: Tree)
    | Leaf.
End Tree2.

(* A binary tree whose cycle has been broken. *)
Module Tree3.
  Inductive Tree (T : Type) : Type :=
    | Root (left: T) (right: T)
    | Lefty (left: T)
    | Righty (right: T)
    | Leaf.
End Tree3.

(* TODO How to model HKTs e.g. Monads? *)
(* TODO How to model GADTs? *)
(* TODO How to model recursion schemes? *)
(* TODO model tree iterators. *)
(* TODO model tree cursors. *)
(* TODO model measures. *)
(* TODO How to require that T is of type Tree<T>? *)
(* TODO how to require T to be a monad? *)
Module Tree4.
  Inductive Tree (T : Type) (V : Type) : Type :=
    | Root (left: T) (right: T)
    | Lefty (left: T)
    | Righty (right: T)
    | Leaf (value: V).
End Tree4.

(* TODO distinguish the self from the whole type. *)
(* TODO how to have a bimeasure that implements Measure? *)
(* TODO have a unidirectional measure and a bimeasure that has two unidirectional measures? *)
(* TODO flip inverts the two sides? *)
(* TODO do the same for groups. *)
Class Measure (A: Type) := {
  (* TODO how to give parameters a name and a type?*)
  add_self: A -> A -> A;
  add_whole: A -> A -> A;
}.

(* From: https://gist.github.com/puffnfresh/11258181 *)
Class Monoid (A : Type) :=
  {
    empty : A;
    append : A -> A -> A ;

    left_neutrality : forall x, append empty x = x ;
    right_neutrality : forall x, append x empty = x ;
    associativity : forall x y z, append x (append y z) = append (append x y) z
  }.

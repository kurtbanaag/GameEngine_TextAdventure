(* [command] represents a command input by a player. *)
(* You may redefine [command] to be synonymous with whatever
 * type you wish, but its name must not be changed.
 * It is okay to expose this type rather than make it abstract,
 * because it is not the representation type of a data structure. *)
type command = {
  com: string;
  word: string;
}

(********************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will
 * use to test your submission.
 *)

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the commands forms described in the
 *   assignment writeup. *)
val parse : string -> command

(* END DO NOT CHANGE
 ********************************************************)
(* You are free to add more code below *)

val makeGoCommand: string -> command
val makeCommand: string -> string -> command
val falseCommand: command

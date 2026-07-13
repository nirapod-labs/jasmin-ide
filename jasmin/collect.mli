(** The extraction of the facts from one item that typed.

    This module is internal to the adapter: its signatures name the
    compiler's types, and the engine reads {!Facts} instead. *)

val of_item :
  file:string ->
  env:'asm Jasmin.Pretyping.Env.env ->
  item:Jasmin.Syntax.pitem Jasmin.Location.located ->
  decls:(unit, 'asm) Jasmin.Prog.pmod_item list ->
  Facts.t
(** [of_item ~file ~env ~item ~decls] is what [item] left behind. [env] is
    the environment after [item] typed and [decls] are the typed
    declarations [item] added to it. Every item of the result belongs to
    [file]: a declaration a [require] brought in is another file's fact,
    and it is not returned here. *)

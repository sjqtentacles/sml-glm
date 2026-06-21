(* entry.sml

   Defines `main : unit -> unit`, the entry point used by both compilers.
   It runs every suite, prints the harness summary, and exits with a status
   code reflecting success. *)

fun runAllSuites () =
  ( Harness.reset ()
  ; VecTests.run ()
  ; MatTests.run ()
  ; QuatTests.run ()
  ; Harness.run () )

fun main () =
  OS.Process.exit
    (if runAllSuites () then OS.Process.success else OS.Process.failure)

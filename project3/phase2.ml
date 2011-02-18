open Il
open X86
open Cunit




let compile_prog (prog:Il.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
failwith "TODO: Phase2.compile_prog unimplemented"
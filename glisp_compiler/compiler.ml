let get_output_filename s =
    let dot = String.rindex s '.' in
        String.sub s 0 dot ^ ".gcc"

let _ =
        let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
        let outbuf = open_out (get_output_filename Sys.argv.(1)) in
    try
        let result = Parser.prog Lexer.read lexbuf in
            match result with 
            | None -> print_string "No program found."; print_newline();
            | Some r -> 
                let instrs = Glisp.generate_program r outbuf in
                    List.iter (fun l -> output_string outbuf l; output_char outbuf '\n') instrs;
                    print_string "Compilation complete."; print_newline();
    with exn ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        print_string "Error at line "; print_int(line); print_string " col "; print_int cnum; print_string " token "; print_string tok; print_newline(); flush(stdout);
        Printexc.print_backtrace stderr;
      end

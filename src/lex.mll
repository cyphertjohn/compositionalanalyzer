{
    open Par        (* The type token is defined in par.mli *)
    exception Eof
    let keyword_table = Hashtbl.create 53
    let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
                      [ "if", IF;
                        "then", THEN;
                        "endif", ENDIF;
                        "else", ELSE;
                        "do", DO;
                        "while", WHILE;
                        "done", DONE;
                        "true", TRUE;
                        "false", FALSE
                        ]
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id {try 
                                                            Hashtbl.find keyword_table id
                                                           with Not_found ->
                                                             IDENT id}
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | '+'            { PLUS }
    | '*'            { TIMES }
    | '-'            { MINUS }
    | "<="           { LE }
    | '<'            { LESS }
    | '>'            { GREATER }
    | ">="           { GE }
    | "=="           { EQUAL }
    | "||"           { OR }
    | "&&"           { AND }
    | '!'            { NOT }
    | '='            { ASSIGN }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | ';'            { SC }
    | eof            { EOF }
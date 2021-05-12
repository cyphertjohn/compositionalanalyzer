/* File parser.mly */
        %{
            module S = Set.Make(struct type t = string let compare = compare end)
            let vars = ref S.empty

            let to_list s = S.fold (fun s acc -> s :: acc) s []

            let add_var v = vars:= S.add v !vars

            let negate_lin_term y = 
              match y with 
              | PathExp.Times(n , x) -> PathExp.Times((-1)*n, x)
              | PathExp.Int n  -> PathExp.Int((-1) * n)
              | _ -> failwith "Negation called on not a term"

            let negate_first l =
              match l with
              | (first :: rest) -> (negate_lin_term first) :: rest
              | [] -> failwith "This can't happen"

            let add_Add l = 
              if List.length l = 1 then List.hd l
              else PathExp.Add l
              
              
        %}        
        %token <int> INT
        %token <string> IDENT
        %token PLUS MINUS TIMES ASSIGN
        %token IF THEN ENDIF ELSE DO WHILE DONE TRUE FALSE AND OR NOT ASSERTION ASSUME
        %token LE LESS GREATER GE EQUAL
        %token LPAREN RPAREN SC
        %token EOF
        %left PLUS MINUS OR     /* lowest precedence */
        %left TIMES AND         /* medium precedence */
        %nonassoc UMINUS UNOT   /* highest precedence */
        %start main             /* the entry point */
        %type <(PathExp.statement PathExp.pathexp * PathExp.boolexp option) * string list> main
        %%
        main:
            program EOF                 { $1, to_list !vars }
        ;
        program:
            assume statement            { PathExp.Mul($1, $2), None }
          | statement                   { $1, None }
          | statement assertion         { $1, Some $2 }
          | assume statement assertion     { PathExp.Mul($1, $2), Some $3 }
        ;
        assume:
            ASSUME LPAREN boolexp RPAREN    { PathExp.Letter(PathExp.Cond($3)) }
        ;
        assertion:
            ASSERTION LPAREN boolexp RPAREN    { $3 }
        ;
        statement:
            statement loop statement        { PathExp.Mul($1, PathExp.Mul($2, $3)) }
          | statement loop                  { PathExp.Mul($1, $2) }
          | loop statement                  { PathExp.Mul($1, $2) }
          | statement branch statement      { PathExp.Mul($1, PathExp.Mul($2, $3)) }
          | statement branch                { PathExp.Mul($1, $2) }
          | branch statement                { PathExp.Mul($1, $2) }
          | branch                          { $1 }
          | statement SC statement          { PathExp.Mul($1, $3) }
          | linassign                       { $1 }
        ;
        loop:
            WHILE LPAREN boolexp RPAREN DO statement DONE   { PathExp.Mul(PathExp.Star(PathExp.Mul(PathExp.Letter(PathExp.Cond($3)), $6)), PathExp.Letter(PathExp.Cond(PathExp.Not($3)))) }
        ;
        branch:
            IF LPAREN boolexp RPAREN THEN statement ENDIF                 { PathExp.Plus(PathExp.Mul(PathExp.Letter(PathExp.Cond($3)), $6), PathExp.Letter(PathExp.Cond(PathExp.Not($3)))) }
          | IF LPAREN boolexp RPAREN THEN statement ELSE statement ENDIF  { PathExp.Plus(PathExp.Mul(PathExp.Letter(PathExp.Cond($3)), $6), PathExp.Mul(PathExp.Letter(PathExp.Cond(PathExp.Not($3))), $8)) }
        ;
        linassign:
            IDENT ASSIGN linexp             { (add_var $1); PathExp.Letter(PathExp.Assign($1, add_Add $3)) }
        ;
        linexp:
            linterm PLUS linexp              { $1 :: $3 }
          | linterm MINUS linexp             { $1 :: (negate_first $3) }
          | MINUS linterm %prec UMINUS       { [negate_lin_term $2] }
          | linterm                          { [$1] }
        ;
        linterm:
            INT TIMES IDENT             { (add_var $3); PathExp.Times($1, $3) }
          | INT IDENT                   { (add_var $2); PathExp.Times($1, $2) }
          | IDENT                       { (add_var $1); PathExp.Times (1, $1) }
          | INT                         { PathExp.Int($1) }
        ;
        boolexp:
            LPAREN boolexp RPAREN       { $2 }
          | boolexp AND boolexp         { PathExp.And($1, $3) }
          | boolexp OR boolexp          { PathExp.Or($1, $3) }
          | NOT boolexp %prec UNOT      { PathExp.Not($2) }
          | ground                      { $1 }
        ;
        ground:
            TRUE                        { PathExp.True }
          | FALSE                       { PathExp.False }
          | linexp LE linexp            { PathExp.LessEq(add_Add $1, add_Add $3) }
          | linexp LESS linexp          { PathExp.Less(add_Add $1, add_Add $3) }
          | linexp GE linexp            { PathExp.GreaterEq(add_Add $1, add_Add $3) }
          | linexp GREATER linexp       { PathExp.Greater(add_Add $1, add_Add $3) }
          | linexp EQUAL linexp         { PathExp.Equal(add_Add $1, add_Add $3) }
        ;
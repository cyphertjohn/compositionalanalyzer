/* File parser.mly */
        %{
            module S = Set.Make(struct type t = string let compare = compare end)
            let vars = ref S.empty

            let to_list s = S.fold (fun s acc -> s :: acc) s []

            let add_var v = vars:= S.add v !vars

            let negate_lin_term y = 
              match y with 
              | Sigs.Expr.Times(n , x) -> Sigs.Expr.Times((-1)*n, x)
              | Sigs.Expr.Int n  -> Sigs.Expr.Int((-1) * n)

            let negate_first l =
              match l with
              | (first :: rest) -> (negate_lin_term first) :: rest
              | [] -> failwith "This can't happen"

            let add_Add l = Sigs.Expr.Add l
              
              
        %}        
        %token <int> INT
        %token <string> IDENT
        %token PLUS MINUS TIMES MOD
        %token TRUE FALSE AND OR NOT
        %token LE LESS GREATER GE EQUAL
        %token LPAREN RPAREN
        %token EOF EOL
        %right MOD
        %left PLUS MINUS OR     /* lowest precedence */
        %left TIMES AND         /* medium precedence */
        %nonassoc UMINUS UNOT   /* highest precedence */
        %start main             /* the entry point */
        %type <Sigs.Expr.boolexp  * string list> main
        %%
        main:
            boolexp EOF                 { $1, to_list !vars }
          | boolexp EOL                 { $1, to_list !vars }
        ;
        linexp:
            linterm PLUS linexp              { $1 :: $3 }
          | linterm MINUS linexp             { $1 :: (negate_first $3) }
          | MINUS linterm %prec UMINUS       { [negate_lin_term $2] }
          | linterm                          { [$1] }
        ;
        linterm:
            INT TIMES IDENT             { (add_var $3); Sigs.Expr.Times($1, $3) }
          | INT IDENT                   { (add_var $2); Sigs.Expr.Times($1, $2) }
          | IDENT                       { (add_var $1); Sigs.Expr.Times (1, $1) }
          | INT                         { Sigs.Expr.Int($1) }
        ;
        arith_expr:
            linexp                      { Sigs.Expr.Sum(add_Add $1) }
          | linexp MOD linexp           { Sigs.Expr.Mod(add_Add $1, add_Add $3) }
        boolexp:
            LPAREN boolexp RPAREN       { $2 }
          | boolexp AND boolexp         { Sigs.Expr.And($1, $3) }
          | boolexp OR boolexp          { Sigs.Expr.Or($1, $3) }
          | NOT boolexp %prec UNOT      { Sigs.Expr.Not($2) }
          | TRUE                        { Sigs.Expr.True }
          | FALSE                       { Sigs.Expr.False }
          | pred                        { Sigs.Expr.Pred $1 }
        ;
        pred:
          | arith_expr LE arith_expr            { Sigs.Expr.LessEq($1, $3) }
          | arith_expr LESS arith_expr          { Sigs.Expr.Less($1, $3) }
          | arith_expr GE arith_expr            { Sigs.Expr.GreaterEq($1, $3) }
          | arith_expr GREATER arith_expr       { Sigs.Expr.Greater($1, $3) }
          | arith_expr EQUAL arith_expr         { Sigs.Expr.Eq ($1, $3) }
        ;
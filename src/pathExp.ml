type linexp =
  | Int of int
  | Add of linexp list
  | Times of int * string

type boolexp = 
  | True
  | False
  | And of boolexp * boolexp
  | Or of boolexp * boolexp
  | Not of boolexp
  | LessEq of linexp * linexp
  | Less of linexp * linexp
  | GreaterEq of linexp * linexp
  | Greater of linexp * linexp
  | Equal of linexp * linexp

type statement = 
  | Assign of string * linexp
  | Cond of boolexp

type 'a pathexp = 
  | One
  | Zero
  | Letter of 'a
  | Plus of 'a pathexp * 'a pathexp
  | Mul of 'a pathexp * 'a pathexp
  | Star of 'a pathexp

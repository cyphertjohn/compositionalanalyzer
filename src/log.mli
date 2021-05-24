(**Module used for logging*)

(**Set the output of the logger. Default is stdout.*)
val set_chan : out_channel -> unit

(**Sets the verbosity level. Options are "always", "debug", and "trace".*)
val set_level : string -> unit

(**Log a string according to a verbosity level. Default verbosity is always. 
For example to log string s at level debug one uses [log ~level:`debug s].
If the level label is not given, the logging level is [`always].*)
val log : ?level:[< `always | `debug | `trace > `always ] -> string -> unit

(**Same as {!val:log} but prints a new line character at the end.*)
val log_line :
  ?level:[< `always | `debug | `trace > `always ] -> string -> unit

(**Log the evaluation time taken by a function. For example given a function 
[f:'a -> 'b] and argument [x:'a], [log_time "label" f x] returns [f x] and 
prints "label: \{time\}" to the outchannel.*)
val log_time : string -> ('a -> 'b) -> 'a -> 'b

(**Similar to {!val:log_time}, but prints the cumulative time taken by 
every function that was logged with the label string.*)
val log_time_cum : string -> ('a -> 'b) -> 'a -> 'b

(**Set to output the time logging information.*)
val log_times : bool ref

(**Close the out channel.*)
val close : unit -> unit
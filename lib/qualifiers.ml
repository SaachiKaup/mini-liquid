open Syntax

(* Set Q, we are manually providing, this is not derived, it is in the paper as something provided in the beginnning *)
let max_qualifiers = 
  [
    GreaterOrEqual (Name "result", Name "x");
    GreaterOrEqual (Name "result", Name "y");
    GreaterOrEqual (Name "result", Integer 0);
  ]




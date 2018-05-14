
type t =
| Defun of string * args * sequence
| Defvar of string * value * 
| Lambda
| If
| Cond
| While
| Save_excursion
| Let
| Let_star
| Interactive
| Progn
| Unwind_protect
| Defconst
| Or 
| And
  
and value = 
and args = string list
and sequence = t list
  
type program = sequence
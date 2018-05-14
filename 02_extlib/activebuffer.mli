(************************************************************************)
(*                                                                      *)
(*                        Caml Development Kit                          *)
(*                                                                      *)
(*                 Projet Moscova, INRIA Rocquencourt                   *)
(*                                                                      *)
(************************************************************************)

(*c This module implements string buffers that automatically expand
   as necessary.  It provides accumulative concatenation of strings
   in quasi-linear time (instead of quadratic time when strings are
   concatenated pairwise) and allows partial deletion. It is fully
   compliant with the [Buffer] module of the standard library *)

type t
     (*d The abstract type of buffers. *)

(*1 these functions already exist in [Buffer] *)

val create : int -> t
     (*d [create n] returns a fresh buffer, initially empty.
        The [n] parameter is the initial size of the internal string
        that holds the buffer contents.  That string is automatically
        reallocated when more than [n] characters are stored in the buffer,
        but shrinks back to [n] characters when [reset] is called.
        For best performance, [n] should be of the same order of magnitude
        as the number of characters that are expected to be stored in
        the buffer (for instance, 80 for a buffer that holds one output
        line).  Nothing bad will happen if the buffer grows beyond that
        limit, however.  In doubt, take [n = 16] for instance.
        If [n] is not between 1 and [Sys.max_string_length], it will
        be clipped to that interval. *)
val contents : t -> string
     (*d Return a copy of the current contents of the buffer.
        The buffer itself is unchanged. *)
val length : t -> int
     (*d Return the number of characters currently contained in the buffer. *)
val clear : t -> unit
     (*d Empty the buffer. *)
val reset : t -> unit
     (*d Empty the buffer and deallocate the internal string holding the
        buffer contents, replacing it with the initial internal string
        of length [n] that was allocated by [create n].
        For long-lived buffers that may have grown a lot, [reset] allows
        faster reclaimation of the space used by the buffer. *)
val add_char : t -> char -> unit
     (*d [add_char b c] appends the character [c] at the end of
        the buffer [b]. *)
val add_string : t -> string -> unit
     (*d [add_string b s] appends the string [s] at the end of
        the buffer [b]. *)
val add_substring : t -> string -> int -> int -> unit
     (*d [add_substring b s ofs len] takes [len] characters from offset
        [ofs] in string [s] and appends them at the end of the buffer [b]. *)
val add_buffer : t -> t -> unit
     (*d [add_buffer b1 b2] appends the current contents of buffer [b2]
        at the end of buffer [b1].  [b2] is not modified. *)
val add_channel : t -> in_channel -> int -> unit
     (*d [add_channel b ic n] reads exactly [n] character from the
        input channel [ic] and stores them at the end of buffer [b].
        Raise [End_of_file] if the channel contains fewer than [n]
        characters. *)
val output_buffer : out_channel -> t -> unit
     (*d [output_buffer oc b] writes the current contents of buffer [b]
        on the output channel [oc]. *)

(*1 These functions do not exist in [Buffer] *)

val add_subbuffer : t -> t -> int -> int -> unit
     (*d [add_subbuffer dst src pos len] appends the sub_part of buffer
        [src] starting at position [pos] and of length [len] to
        the buffer [dst]. [src] is not modified. Raise [Invalid_argument]
        if [pos] and [len] do not designate a valid sub_part of [src] *)
val sub : t -> int -> int -> unit
     (*d [sub b pos len] modify the buffer in order its content
        to designate the substring of the initial content,
        starting at offset [pos] and of length [len]. This function
        does not copy nor create strings. Raise [Invalid_argument]
        if [pos] and [len] do not designate a valid sub_part of the buffer *)
val buffer : t -> string*int
    (*d [buffer b] gives the internal string used by the buffer and the
       starting active posistion. It can be used in conjunction with
       [sub] when doing [write] in order to avoid unnecessary copy.
       [let len = length b in
        let str, pos = buffer b in
        let nb_written = write fds str pos len in
        sub b nb_written (len - nb_written)] *)
val before_read : t -> int -> string*int
val after_read : t -> int -> unit
    (*d [before_read b len] can be used if you want to do a read directly
       inside a buffer for avoiding copy. You should use a sequence
       like this : 
{{
  let str, pos = before_read b len in
  let nb_read = read fds str pos len in
  after_read b nb_read
}}
  You should always use
       [before_read] and [after_read] together. *)


; Align all pointers (functions and data) to 16 bytes (128 bits)?
;  - leaves 4 LSB of every pointer for tags

struct closure {
  char8 * code; ; Code pointer + tag bits (used for..?)
  int8 arity; ; Arity of the function addressed by `code`
  int8 args; ; Number of args that follow
  ; ... variable number of char8* args
}

struct box {} ; Any boxed value

box * function @eval(closure * %f, box * %a) {
  ; if (args+1)==arity call code with args:%a and ret
  ; else allocate copy of %f on the heap, inc arity, append %a and ret
}

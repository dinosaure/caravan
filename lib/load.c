#include <caml/mlvalues.h>
#include <string.h>
#include <stdio.h>

CAMLprim value
caml_load_vaddr(value v_vaddr, value v_buf, value v_off, value v_len)
{
  uint64_t vaddr = Int64_val (v_vaddr) ;
  uint8_t * buf = String_val (v_buf) + Long_val (v_off) ;
  uint8_t * src = (uint8_t *) vaddr ;

  for (int i = 0; i < Long_val (v_len); ++i)
    buf[i] = src[i] ;

  return Val_unit;
}

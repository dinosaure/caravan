#include <caml/mlvalues.h>
#include <caml/bigarray.h>

CAMLprim value
caml_load_vaddr_into_bytes(value v_vaddr, value v_buf, value v_off, value v_len)
{
  uint64_t vaddr = Int64_val (v_vaddr) ;
  uint8_t * buf = String_val (v_buf) + Long_val (v_off) ;
  uint8_t * src = (uint8_t *) vaddr ;

  for (int i = 0; i < Long_val (v_len); ++i)
    buf[i] = src[i] ;

  return Val_unit;
}

CAMLprim value
caml_load_vaddr_into_bigstring(value v_vaddr, value v_buf, value v_off, value v_len)
{
  uint64_t vaddr = Int64_val (v_vaddr) ;
  uint8_t * buf = Caml_ba_data_val (v_buf) + Long_val (v_off) ;
  uint8_t * src = (uint8_t *) vaddr ;

  for (int i = 0; i < Long_val (v_len); ++i)
    buf[i] = src[i] ;

  return Val_unit;
}

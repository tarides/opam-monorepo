#include "minilzo.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>

CAMLprim value
caml_lzo1x_1_compress(value v_src, value v_src_off, value v_src_len, value v_dst, value v_dst_off) {
  CAMLparam5(v_src, v_src_off, v_src_len, v_dst, v_dst_off);
  lzo_voidp wrkmem = malloc(LZO1X_1_MEM_COMPRESS);
  lzo_bytep src = Bytes_val (v_src) + Int_val (v_src_off);
  lzo_bytep dst = Bytes_val (v_dst) + Int_val (v_dst_off);
  lzo_uint  dst_len = 0;
  int       ret;

  ret = lzo1x_1_compress(src, Int_val (v_src_len), dst, &dst_len, wrkmem);
  free(wrkmem);

  if (ret != LZO_E_OK)
    caml_raise_with_string(*caml_named_value("lzo"), "Invalid LZO input");

  CAMLreturn(Val_int (dst_len));
}

CAMLprim value
caml_lzo1x_decompress(value v_src, value v_src_off, value v_src_len, value v_dst, value v_dst_off) {
  CAMLparam5(v_src, v_src_off, v_src_len, v_dst, v_dst_off);
  lzo_bytep src = Bytes_val (v_src) + Int_val (v_src_off);
  lzo_bytep dst = Bytes_val (v_dst) + Int_val (v_dst_off);
  lzo_uint  dst_len = 0;
  int       ret;

  ret = lzo1x_decompress(src, Int_val (v_src_len), dst, &dst_len, NULL);

  if (ret != LZO_E_OK)
    caml_raise_with_string(*caml_named_value("lzo"), "Invalid LZO output");

  CAMLreturn(Val_int (dst_len));
}

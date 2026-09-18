#if !defined(H__CHECKSUM)
#define H__CHECKSUM

#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/alloc.h>

/* XXX(dinosaure): snippet from digestif (<3 david). */

#define _ba_uuint8_off(ba, off) ((uint8_t*) Caml_ba_data_val (ba) + off)
#define _ba_uint8_off(ba, off)  ((uint8_t*) Caml_ba_data_val (ba) + Long_val (off))

#define _st_uuint8_off(st, off) ((uint8_t*) String_val (st) + off)
#define _st_uint8_off(st, off)  ((uint8_t*) String_val (st) + Long_val (off))

#endif /* H__CHECKSUM */

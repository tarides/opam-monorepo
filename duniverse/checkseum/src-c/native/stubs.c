#include "checkseum.h"
#include "adler32.h"
#include "crc32c.h"
#include "crc32.h"
#include "crc24.h"

#ifdef ARCH_SIXTYFOUR
/* XXX(dinosaure): un-boxed version for 64-bits architecture. */

#define __define_checkseum(name)                                                                    \
	CAMLprim value                                                                              \
        caml_checkseum_ ## name ## _ba(value t, value src, value off, value len) {                  \
          intnat res = checkseum_ ## name ## _digest (Int_val (t),                                  \
			                              _ba_uint8_off (src, off), Long_val (len)) ;   \
	  return (Val_int (res));                                                                   \
	}                                                                                           \
                                                                                                    \
        intnat                                                                                      \
        caml_checkseum_ ## name ## _ba_untagged(intnat t, value src, intnat off, intnat len) {      \
          intnat res = checkseum_ ## name ## _digest (t, _ba_uuint8_off (src, off), len) ;          \
	  return (res);                                                                             \
	}                                                                                           \
                                                                                                    \
        CAMLprim value                                                                              \
        caml_checkseum_ ## name ## _st(value t, value src, intnat off, intnat len) {                \
          intnat res = checkseum_ ## name ## _digest (Int_val (t), _st_uint8_off (src, off),        \
			  Long_val (len)) ;                                                         \
	  return (Val_int (res));                                                                   \
        }                                                                                           \
                                                                                                    \
        intnat                                                                                      \
        caml_checkseum_ ## name ## _st_untagged(intnat t, value src, intnat off, intnat len) {      \
          intnat res = checkseum_ ## name ## _digest (t, _st_uuint8_off (src, off), len) ;          \
	  return (res);                                                                             \
	}                                                                                           \

__define_checkseum (adler32)
__define_checkseum (crc32c)
__define_checkseum (crc32)
__define_checkseum (crc24)

#else
/* XXX(dinosaure): boxed version for 32-bits architecture. */

#define __define_checkseum(name)                                                                        \
	CAMLprim value                                                                                  \
        caml_checkseum_ ## name ## _ba(value t, value src, value off, value len) {                      \
	  uint32_t res = checkseum_ ## name ## _digest (Int32_val (t), _ba_uint8_off (src, off),        \
			  Long_val (len)) ;                                                             \
	  return (caml_copy_int32 (res));                                                               \
	}                                                                                               \
	                                                                                        	\
	uint32_t                                                                                        \
        caml_checkseum_ ## name ## _ba_untagged(uint32_t t, value src, intnat off, intnat len) {        \
	  uint32_t res = checkseum_ ## name ## _digest (t, _ba_uuint8_off (src, off), len) ;            \
	  return (res);                                                                                 \
	}                                                                                               \
                                                                                                        \
        CAMLprim value                                                                                  \
        caml_checkseum_ ## name ## _st(value t, value src, value off, value len) {                      \
	  uint32_t res = checkseum_ ## name ## _digest (Int32_val (t), _st_uint8_off (src, off),        \
			  Long_val (len)) ;                                                             \
	  return (caml_copy_int32 (res));                                                               \
        }                                                                                               \
	                                                                                                \
        uint32_t                                                                                        \
        caml_checkseum_ ## name ## _st_untagged(uint32_t t, value src, intnat off, intnat len) {        \
	  uint32_t res = checkseum_ ## name ## _digest (t, _st_uuint8_off (src, off), len) ;            \
	  return (res);                                                                                 \
        }                                                                                               \

__define_checkseum (adler32)
__define_checkseum (crc32c)
__define_checkseum (crc32)
__define_checkseum (crc24)

#endif

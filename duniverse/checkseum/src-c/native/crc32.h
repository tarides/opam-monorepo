#ifndef CHECKSUM_CRC32_H
#define CHECKSUM_CRC32_H

#include <stdint.h>
#include "size_t.h"
#include "ptrdiff_t.h"

#if !defined(CHECKSEUM_U4) && !defined(CHECKSEUM_SOLO)
#  include <limits.h>
#  if (UINT_MAX == 0xffffffffUL)
#    define CHECKSEUM_U4 unsigned
#  elif (ULONG_MAX == 0xffffffffUL)
#    define CHECKSEUM_U4 unsigned long
#  elif (USHRT_MAX == 0xffffffffUL)
#    define CHECKSEUM_U4 unsigned short
#  endif
#endif

#ifdef CHECKSEUM_U4
typedef CHECKSEUM_U4 crc_t;
#else
typedef unsigned long crc_t;
#endif

uint32_t checkseum_crc32_digest(uint32_t crc, const unsigned char *buf, size_t len);

#endif

#ifndef CHECKSUM_CRC24_H
#define CHECKSUM_CRC24_H

#define CRC24_INIT 0xB704CEL
#define CRC24_POLY 0x1864CFBL

#include "size_t.h"

unsigned long checkseum_crc24_digest(unsigned long crc, const unsigned char *buf, size_t len);

#endif

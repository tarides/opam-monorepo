/* See RFC4880 - 6.1 */

#include "crc24.h"

unsigned long
checkseum_crc24_digest(unsigned long crc, const unsigned char *buf, size_t len)
{
  int i;

  while (len--) {
    crc ^= (*buf++) << 16;
    for (i = 0; i < 8; i++) {
      crc <<= 1;
      if (crc & 0x1000000)
        crc ^= CRC24_POLY;
    }
  }

  return (crc & 0xFFFFFFL);
}

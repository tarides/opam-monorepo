/* adler32.c -- compute the Adler-32 checksum of a data stream
 * Copyright (C) 1995-2011, 2016 Mark Adler
 * For conditions of distribution and use, see copyright notice in zlib.h
 */

/* XXX(dinosaure): extracted from zlib. */

#include "adler32.h"

#define BASE 65521U /* largest prime smaller than 65536 */
#define NMAX 5552   /* NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */

#define DO1(buf,i)  {adler += (buf)[i]; sum2 += adler;}
#define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#define DO16(buf)   DO8(buf,0); DO8(buf,8);

/* XXX(dinosaure): zlib checks at the compile time if architecture has a
   division instruction. In MirageOS context, we can avoid this
   hardware-specialization. */

#define MOD(a) a %= BASE
#define MOD28(a) a %= BASE
#define MOD63(a) a %= BASE

static
uLong _adler32(uLong adler, const Bytef *buf, size_t len)
{
  unsigned long sum2;
  unsigned n;

  /* split Adler-32 into component sums. */

  sum2 = (adler >> 16) & 0xffff;
  adler &= 0xffff;

  /* in case user likes doing a byte at a time, keep it fast */
  if (len == 1) {
    adler += buf[0];
    if (adler >= BASE)
      adler -= BASE;
    sum2 += adler;
    if (sum2 >= BASE)
      sum2 -= BASE;
    return (adler | (sum2 << 16));
  }

  /* XXX(dinosaure): on zlib side, this function returns 1L when len = 0,
     however, we want to provide a functional interface with a default value of
     check-sum. */

  /* in case short lengths are provided, keep it somewhat fast */
  if (len < 16) {
    while (len--) {
      adler += *buf++;
      sum2 += adler;
    }
    if (adler >= BASE)
      adler -= BASE;
    MOD28(sum2); /* only added so many BASE's */
    return (adler | (sum2 << 16));
  }

  /* do length NMAX blocks -- requires just one modulo operation */
  while (len >= NMAX) {
    len -= NMAX; /* NMAX is divisible by 16 */
    n = NMAX / 16;
    do {
      DO16(buf); /* 16 sums unrolled */
      buf += 16;
    } while (--n);
    MOD(adler);
    MOD(sum2);
  }

  /* do remaining bytes (less than NMAX, still just one modulo) */
  if (len) {
    while (len >= 16) {
      len -= 16;
      DO16(buf);
      buf += 16;
    }
    while (len--) {
      adler += *buf++;
      sum2 += adler;
    }
    MOD(adler);
    MOD(sum2);
  }

  /* return recombined sums */
  return (adler | (sum2 << 16));
}

uLong checkseum_adler32_digest(uLong adler, const Bytef *buf, size_t len)
{
  return (_adler32(adler, buf, len));
}

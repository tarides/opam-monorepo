Tests of the otar binary
  $ mkdir archive
  $ echo "Hello World" > archive/foo
  $ otar archive/ v.tar.gz
  $ otar list v.tar.gz
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)

Tests decoding of various tar formats
  $ if tar --version | grep -q GNU; then
  >   tar -cz --format=gnu -f v-gnu.tar.gz archive/
  >   otar list v-gnu.tar.gz
  > else printf "archive/ (Directory, 0 byte)\narchive/foo (Normal, 12.00 B)\n"; fi
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)
  $ if tar --version | grep -q GNU; then
  >   tar -cz --format=oldgnu -f v-oldgnu.tar.gz archive/
  >   otar list v-oldgnu.tar.gz
  > else printf "archive/ (Directory, 0 byte)\narchive/foo (Normal, 12.00 B)\n"; fi
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)
  $ tar -cz --format=pax -f v-pax.tar.gz archive/
  $ otar list v-pax.tar.gz
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)
  $ tar -cz --format=posix -f v-posix.tar.gz archive/
  $ otar list v-posix.tar.gz
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)
  $ tar -cz --format=ustar -f v-ustar.tar.gz archive/
  $ otar list v-ustar.tar.gz
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)
  $ tar -cz --format=v7 -f v-v7.tar.gz archive/
  $ otar list v-v7.tar.gz
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)

Test decoding of git archive
  $ git init -q archive && cd archive
  $ git config user.email 'author@example.com' && git config user.name 'A U Thor'
  $ git add . && git commit -q -m "Initial commit"
  $ git archive -o ../archive.tar.gz --prefix=archive/ HEAD
  $ cd ../ && otar list archive.tar.gz
  archive/ (Directory, 0 byte)
  archive/foo (Normal, 12.00 B)

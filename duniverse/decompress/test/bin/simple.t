Simple tests
  $ echo "Hello World!" | decompress -d > simple.z
  $ decompress < simple.z
  Hello World!
  $ echo "Hello World!" | decompress -d -fzlib > simple.z
  $ decompress -fzlib < simple.z
  Hello World!
  $ cc -o zpipe zpipe.c -lz
  $ ./zpipe -d < simple.z
  Hello World!
  $ ./zpipe < ../corpus/news > news.z
  $ decompress -fzlib < news.z > news
  $ diff news ../corpus/news
  $ decompress -fzlib -d < ../corpus/news > news.z
  $ ./zpipe -d < news.z > news
  $ diff news ../corpus/news
  $ decompress -fgzip -d < ../corpus/news > news.gz
  $ decompress -fgzip < news.gz > news
  $ diff news ../corpus/news
  $ decompress -fzlib -d ../corpus/bib bib.zlib
  $ decompress -fzlib bib.zlib bib
  $ diff bib ../corpus/bib
  $ decompress -fzlib -d --level 0 ../corpus/bib bib.zlib
  $ decompress -fzlib bib.zlib bib
  $ diff bib ../corpus/bib
  $ decompress -fgzip -d --level 0 ../corpus/news news.gz
  $ decompress -fgzip news.gz news
  $ diff news ../corpus/news

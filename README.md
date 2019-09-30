```sh
$ dune build example/ex01.exe
$ echo "Hello World!" > provision
$ dune exec bin/caravan.exe -- _build/default/example/ex01.exe provision ex01.exe
$ chmod +x ex01.exe
$ ./ex01.exe
00000000: 4865 6c6c 6f20 576f 726c 6421 0a          Hello World!.
```

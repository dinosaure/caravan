## Caravan

`caravan` is a little software which wants to _inject_ a `.provision` section into given ELF binary file.
The goal of this program is to plug (in a non conventional way) a section which can be loaded by the binary itself.
By this way, we are able to not so statically load some extra data to serve then computation of the given program.

In the way of MirageOS, the idea is to configure and produce an unikernel and deploy an ELF binary file.
Then, user (with `caravan`) is able to plug some personal informations into the given unikernel and
run it/virtualize it.

## Example

```sh
$ dune build example/ex01.exe
$ echo "Hello World!" > provision
$ dune exec bin/caravan.exe -- -i _build/default/example/ex01.exe -p provision ex01.exe
$ chmod +x ex01.exe
$ ./ex01.exe
00000000: 4865 6c6c 6f20 576f 726c 6421 0a          Hello World!.
```

## With MirageOS

Compatible with MirageOS 3 and should be with MirageOS 4:

```sh
$ cd mirage
$ mirage configure -t unix
$ mirage build
$ cd ..
$ dune exec bin/caravan.exe -- -i mirage/filled -p mirage/provision filled
$ chmod +x filled
$ ./filled
9f4e47215a247af1c3d24f7aa33560d637e9389b
```

## Caution

`caravan` __injects__ a new section in a non-conventional way. This way is error-prone and an injection
can completely alterate behvior of your program. ELF scheme expected is really simple. `caravan` was not
think to plug in any ELF binary the new `.provision` section.

Then, the status of this project still is experimental.

## Ezport

Are you working on a cross-compilation architecture ? Such as `windows`(https://github.com/ocaml-cross/opam-cross-windows), `android`(https://github.com/ocaml-cross/opam-cross-android), `ios`(https://github.com/ocaml-cross/opam-cross-windows) or `esp32`(https://github.com/well-typed-lightbulbs/opam-cross-esp32). Then you might want to port some OCaml packages that haven't been already ported. 

This tool helps to port a package by automatically taking the latest package definition of dependencies and doing the appropriate updates if jbuilder
is recognised. This project is highly experimental, mainly for personal use.

A typical port command is `ezport <system> <package> <host_repo> <cross_repo>`.

For example: `ezport esp32 tcpip ~/.opam/repo/packages/ ~/opam-cross-esp32/packages/`.
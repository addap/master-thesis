This repository contains all files related to my Master's thesis about verifying the basic fiber abstraction of Eio. 
The Coq fomalization is split in half.
The main part is done using Iris 4.0 using the hazel program logic. 
However, this uses an axiomatization of the API of the broadcast data structure that was proven separately.
Since the CQS formalization that we used for the verification of the Eio broadcast specification uses an old version of Iris and is done in heaplang we keep it in the `kotlinx.coroutines` subdirectory. We assume that the proven specification in heaplang is also provable in the effect language, as no effects are used in the implenentation and apart from effects the sementics of the two languages are equal.

# Compiling the proofs of the Eio case study

Go to the `project/` directory which includes the adapted hazel sources from `https://gitlab.inria.fr/cambium/hazel`.
The Eio formalization is in the `theories/eio/` directory and an example program using it is in `theories/case-studies/`.

Execute the following command to install all dependencies in a local switch and activate it.
```bash
$ opam switch import eio.switch --switch .
$ eval $(opam env)
```

Then run the following to compile all proofs.
```bash
$ coq_makefile -f _CoqProject -o Makefile.coq
$ make -f Makefile.coq
```

# Compiling the proofs of the adapted CQS

Go to the `kotlinx.coroutines/formal-proofs/` directory which includes the adapted CQS sources from `https://github.com/Kotlin/kotlinx.coroutines`.
The adapted CQS formalization is in the `theories/lib/thread_queue/eio_broadcast.v` file, which uses the underlying formalization of the infinite array of the original CQS in the `theories/lib/concurrent_linked_list/` directory.

Execute the following command to install all dependencies in a local switch and activate it.
```bash
$ opam switch import cqs.switch --switch .
$ eval $(opam env)
```

Then run the following to compile all proofs.
```bash
$ coq_makefile -f _CoqProject -o Makefile.coq
$ make -f Makefile.coq
```

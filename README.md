BS-Containers
=============
[![travids](https://travis-ci.org/BuckleTypes/bs-containers.svg?branch=master)](https://travis-ci.org/BuckleTypes/bs-containers/builds)

This is a port/fork of the [ocaml-containers project](https://github.com/c-cube/ocaml-containers) for use with BuckleScript

The project is currently in a **highly unfinished state** and will see major changes to the entire API. The goal of the project is to provide an extended container library for BuckleScript that follows JavaScript conventions and compiles down to efficient JavaScript. More long term goals are to supplant the OCaml std lib entirely, and to provuide a cross-platform library for use with Reason.

**Note: You should not use this yet, unless you know exactly what you're doing, it will frequently break your code**

To build:
```
npm run build
```

To test:
```
npm run test
```

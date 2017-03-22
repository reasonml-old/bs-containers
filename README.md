BS-Containers
=============
![travids](https://travis-ci.org/BuckleTypes/bs-containers.svg?branch=master)

This is a bs port of the ocaml-containers project

Some changes I made:
* Removed CCIO module since we lack primitives for IO operations in BS
* Minor changes to syntax so it compiles with BS

To build:
```
npm run build
```

To test:
```
npm run test
```

# Haskell Programming from First Principles
This repository hosts my solutions to the exercises contained in the book "[Haskell Programming from first principles](http://haskellbook.com/)" by [Christopher Allen](https://twitter.com/bitemyapp) and [Julie Moronuki](https://twitter.com/argumatronic).

**This repo is in no way connected to the original book or authors. Code may have bugs or may not solve the problems completely.**

## Dependencies
The only dependency is [Stack](https://docs.haskellstack.org/en/stable/README/). Once setup, Stack takes care of any Haskell package dependencies.

## Project structure
This repository is organized as a single Stack project where `src` contains the solutions; each module represents a book chapter (so, for instance, `Ch31` represents Chapter 31).
`test` contains test suites following the same naming convention.

## Build the project
To build the project, simply do:
```bash
stack build
```

## Run the tests
To run the tests, simply do:
```bash
stack test
```

## License
Licensed under the "THE BEER-WARE LICENSE" (Revision 42): Andrew Bessi wrote this file. As long as you retain this notice you can do whatever you want with this stuff. If we meet some day, and you think this stuff is worth it, you can buy me a beer or coffee in return.
# Bam - Property-Based Testing Library for OCaml

## Overview

Bam is an OCaml library designed for writing property-based tests. It
simplifies the process of defining and using generators for tests,
offering a monad-like structure that integrates seamlessly with
shrinking strategies. This design aims to make shrinking both
predictable and effective, thereby enhancing the debugging experience.


## Key Features

- Standard Module: The {!module:Std} module provides some basic
  generators with predefined shrinking strategies

- Monad-like Generators: The library enables easy creation of new
  generators, following a monad-like pattern that works harmoniously
  with shrinking mechanisms.

- Shrinking Strategies: Various default shrinking strategies are
  available, aiding in the efficient identification of minimal
  counter-examples.

- Custom Shrinkers: The {!Gen} module allows for the definition of
  ad-hoc shrinkers.

- Documentation on Shrinking: For those interested in understanding
  the intricacies of shrinking within this library, a detailed primer
  is available [here](https://francoisthire.github.io/bam/bam/shrinking.html).

## Installation

### With opam

```bash
opam install bam tezt-bam
```


## Usage

A simple test can be run as follows:

```ocaml
open Tezt_bam

let register () =
  let gen = Std.int () in
  let property _x = Ok () in
  Pbt.register ~__FILE__ ~title:"Simple example of bam" ~tags:["bam"; "simple"]
    ~gen ~property ()	
```


More examples can be found [here](https://github.com/francoisthire/bam/tree/master/example).


## License

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

(** This library can be used to write property-based tests.

    Primarily, the library is structured so that the {!module:Std}
    module encapsulates most of the necessary functions. This module
    includes basic generators, which form the foundation for crafting
    custom generators tailored to specific testing needs.

    The library introduces a monad-like structure to facilitate the
    creation of new generators. This structure is particularly
    advantageous because it ensures predictable and manageable
    shrinking behavior. Consequently, it simplifies the debugging
    process by providing a consistent shrinking pattern, an essential
    feature for isolating and understanding failures in property-based
    testing.

    Recognizing the importance of finding minimal counter-examples in
    testing, the library provides various shrinking strategies. These
    strategies are designed to streamline the process of narrowing
    down the test cases to the simplest form that still demonstrates a
    bug or an issue, making the analysis and rectification of problems
    more efficient.

    For scenarios requiring specific shrinking behavior, the {!Gen}
    modules come into play. These modules allow for the definition of
    ad-hoc shrinkers, offering the flexibility to tailor the shrinking
    process to meet the unique requirements of different tests.

    For those interested in gaining a deeper understanding of the
    library's shrinking mechanism, a detailed resource is
    available. The document {{!page-shrinking} our primer on
    shrinking} provides insights and explanations about the inner
    workings of the shrinking process within this library, catering to
    both beginners and experienced users seeking a more profound
    comprehension of the system.
*)

(** Main module defining basic generators. *)
module Std = Std

(** Module defining what a random generator is. *)
module Gen = Gen

(** Module defining trees which is the main data-structure behind
    shrinking used by the {!module:Gen}. This module shoul be used
    only when defining a new runner for a new Test framework.*)
module Tree = Tree

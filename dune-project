(lang dune 3.7)

(name bam)

(generate_opam_files true)

(source
 (github francoisthire/bam))

(authors "Fran\195\167ois Thir\195\169")

(maintainers "Fran\195\167ois Thir\195\169")

(license MIT)

(documentation https://francoisthire.github.io/bam/)

(package
 (name bam)
 (synopsis "A property-based testing library with internal shrinking")
 (description
  "A property-based testing allowing to define generators with internal shrinking easily")
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.7))
  pringo
  (zarith
   (>= 1.13))
  (odoc :with-doc)
  (tezt
   (and
    (>= 4.0)
    :with-test)))
 (tags
  (test pbt shrinking internal)))

(package
 (name bam-ppx)
 (synopsis "A PPX deriving generators for OCaml types")
 (description
  "Provides a way to automatically get generators for a given type")
 (depends
  (ocaml
   (and
    (>= 4.14)
    (<= 5.2.0)))
  (dune
   (>= 3.7))
  (ppxlib
   (>= 0.32.0))
  (dmap
   (>= 0.5))
  (odoc :with-doc))
 (tags
  (test pbt shrinking internal bam ppx)))

(package
 (name tezt-bam)
 (synopsis "A plugin of [bam] for Tezt")
 (description "Provides a way to register PBT tests with Tezt")
 (depends
  ocaml
  (dune
   (>= 3.7))
  (tezt
   (>= 4.0))
  bam
  bam-ppx
  (mtime
   (>= 2.0))
  (odoc :with-doc))
 (tags
  (test tezt pbt shrinking internal bam)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

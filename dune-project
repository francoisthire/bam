(lang dune 3.7)

(name bam)

(generate_opam_files true)

(source
 (github francoisthire/bam))

(authors "François Thiré")

(maintainers "François Thiré")

(license MIT)

(documentation https://francoisthire.github.io/bam/)

(package
 (name bam)
 (synopsis "A property-based testing library with internal shrinking")
 (description "A property-based testing allowing to define generators with internal shrinking easily")
 (depends (ocaml (>= 4.14)) (dune (>= 3.7)) pringo (zarith (>= 1.13)) (odoc :with-doc) (tezt :with-test))
 (tags
  (test pbt shrinking internal)))

(package
 (name tezt-bam)
 (synopsis "A plugin of [bam] for Tezt")
 (description "Provides a way to register PBT tests with Tezt")
 (depends ocaml dune tezt bam (mtime (>= 2.0)) (odoc :with-doc))
 (tags
  (test tezt pbt shrinking internal bam)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

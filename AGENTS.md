# Contribution guidelines

- **Update `CHANGES`**: each pull request that modifies the library or
  tooling should add an entry to `CHANGES`. Use an "Unreleased" section
  when no new version has been tagged yet.
- **Run the tests**: before submitting, run `dune build .` and
  `dune exec test/main.exe -- --keep-going` if dependencies are
  available.

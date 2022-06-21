1.1.0
=====

- Compile with GHC 8.8, 9.0, 9.2.
- Set up Github CI to build that.
- Fix bug that did not allow to specify length of `NOBITS` sections.
- Misc minor fixes

1.0.2
=====

- Add documentation to `README.md`
- examples: Rework `DummyLd`, add an x86 test file.
- examples: Rework the last sections of the `README_ru.md`

1.0.1
=====

- examples: Rename Aarch64 -> AArch64
- examples: Fuse `MkObj` into `AsmAArch64`, rewrite `MkExe` as `DummyLd`
- Implement `elfFindSectionByName`
- Add test data into the hackage tarball

1.0.0
=====

Initial release

# melf

> A [Haskell](https://www.haskell.org/) library to parse/serialize
> Executable and Linkable Format ([ELF](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format))

## Related work

- [elf](https://github.com/wangbj/elf) - does not write ELF, only parses it
- [data-elf](https://github.com/mvv/data-elf) - parses just headers/tables; depends on a library that fails to build with modern GHCs

## History

For the early history look at the branch "[amakarov](https://github.com/aleksey-makarov/elf/tree/amakarov)" of
the my copy of the [elf](https://github.com/aleksey-makarov/elf) repo.

## Tests

Test data is committed with [git-lfs](https://git-lfs.github.com/).

## Development

- `melf.cabal` is generated from `package.yaml`.  Use `hpack` to do that.
- `melf.nix` is generated.  Use `cabal2nix . > melf.nix` to do that.

## License

BSD 3-Clause License (c) Aleksey Makarov

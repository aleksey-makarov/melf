{ mkDerivation, base, binary, bytestring, directory, exceptions
, filepath, hpack, lens, mtl, prettyprinter, singletons, stdenv
, tasty, tasty-golden, tasty-hunit, template-haskell
}:
mkDerivation {
  pname = "melf";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring exceptions lens mtl prettyprinter singletons
    template-haskell
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base binary bytestring directory exceptions filepath prettyprinter
    singletons tasty tasty-golden tasty-hunit
  ];
  prePatch = "hpack";
  homepage = "https://github.com/aleksey-makarov/melf";
  description = "An Elf parser";
  license = stdenv.lib.licenses.bsd3;
}

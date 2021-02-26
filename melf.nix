{ mkDerivation, base, binary, bytestring, directory, exceptions
, filepath, hpack, lens, lib, mtl, prettyprinter, singletons, tasty
, tasty-golden, tasty-hunit, template-haskell
}:
mkDerivation {
  pname = "melf";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring exceptions lens mtl prettyprinter singletons
    template-haskell
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base binary bytestring exceptions prettyprinter
  ];
  testHaskellDepends = [
    base binary bytestring directory exceptions filepath prettyprinter
    singletons tasty tasty-golden tasty-hunit
  ];
  prePatch = "hpack";
  homepage = "https://github.com/aleksey-makarov/melf";
  description = "An Elf parser";
  license = lib.licenses.bsd3;
}

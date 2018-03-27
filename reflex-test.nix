{ mkDerivation, base, containers, ghcjs-dom, jsaddle-dom, mtl
, reflex, reflex-basic-host, reflex-dom, stdenv, stm, text
, transformers
}:
mkDerivation {
  pname = "reflex-test";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers ghcjs-dom jsaddle-dom mtl reflex reflex-basic-host
    reflex-dom stm text transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}

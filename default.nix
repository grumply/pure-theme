{ mkDerivation, base, pure, pure-css, pure-txt, stdenv }:
mkDerivation {
  pname = "pure-theme";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-css pure-txt ];
  homepage = "github.com/grumply/pure-theme";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}

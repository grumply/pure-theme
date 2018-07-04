{ mkDerivation, base, pure, pure-css, pure-txt, pure-txt-trie, hashable, stdenv }:
mkDerivation {
  pname = "pure-theme";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-css pure-txt pure-txt-trie hashable ];
  homepage = "github.com/grumply/pure-theme";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}

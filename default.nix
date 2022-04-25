{ mkDerivation, base, ef, pure-core, pure-dom, pure-lifted, pure-css, pure-styles, pure-txt, pure-txt-trie, hashable, stdenv }:
mkDerivation {
  pname = "pure-theme";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ef pure-core pure-dom pure-lifted pure-css pure-styles pure-txt pure-txt-trie hashable ];
  homepage = "github.com/grumply/pure-theme";
  description = "Styled views";
  license = stdenv.lib.licenses.bsd3;
}

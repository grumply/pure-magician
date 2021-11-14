{ mkDerivation, ghc, base, pure-auth, pure-conjurer, pure-convoker, pure-elm, pure-hooks, pure-json, pure-sorcerer, pure-txt, pure-websocket, yaml, stdenv }:
mkDerivation {
  pname = "pure-magician";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base pure-auth pure-conjurer pure-convoker pure-elm pure-hooks pure-json pure-sorcerer pure-txt pure-websocket ];
  license = stdenv.lib.licenses.bsd3;
}

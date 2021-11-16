{ mkDerivation, ghc, base, pure-auth, pure-conjurer, pure-convoker, pure-elm, pure-hooks, pure-json, pure-maybe, pure-server, pure-sorcerer, pure-txt, pure-websocket, pure-websocket-cache, yaml, stdenv }:
mkDerivation {
  pname = "pure-magician";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base pure-auth pure-conjurer pure-convoker pure-elm pure-hooks pure-maybe pure-json pure-sorcerer pure-txt pure-websocket pure-websocket-cache 
    ] ++ (if ghc.isGhcjs or false
          then [ ]
          else [ pure-server yaml ]
          );
  license = stdenv.lib.licenses.bsd3;
}

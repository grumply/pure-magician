{ mkDerivation, ghc, base, excelsior, pure-auth, pure-async, pure-bloom, pure-conjurer, pure-convoker, pure-core, pure-elm, pure-hooks, pure-json, pure-lifted, pure-marker, pure-maybe, pure-random-pcg, pure-router, pure-server, pure-sorcerer, pure-time, pure-txt, pure-txt-interpolate, pure-websocket, pure-websocket-cache, yaml, hashable, stdenv }:
mkDerivation {
  pname = "pure-magician";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = 
    [ base excelsior pure-auth pure-async pure-bloom pure-conjurer pure-convoker pure-core pure-elm pure-hooks pure-marker pure-maybe pure-json pure-lifted pure-random-pcg pure-router pure-sorcerer pure-time pure-txt pure-txt-interpolate pure-websocket pure-websocket-cache hashable
    ] ++ (if ghc.isGhcjs or false
          then [ ]
          else [ pure-server yaml ]
          );
  license = stdenv.lib.licenses.bsd3;
}

with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        glibcLocales
        hlint
        ormolu
        parallel
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}

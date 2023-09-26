with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_16.stdenv; } {
    buildInputs = [
        ghc
        glibcLocales
        gmp
        hlint
        libffi
        llvmPackages_16.lld
        ormolu
        parallel
        shellcheck
    ];
    APPEND_LIBRARY_PATH = lib.makeLibraryPath [
        gmp
        libffi
    ];
    shellHook = ''
        export LD_LIBRARY_PATH="$APPEND_LIBRARY_PATH:$LD_LIBRARY_PATH"
        . .shellhook
    '';
}

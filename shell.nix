with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_17.stdenv; } {
    buildInputs = [
        elfutils
        ghc
        glibcLocales
        gmp
        hlint
        libffi
        llvmPackages_17.lld
        ormolu
        parallel
        shellcheck
    ];
    APPEND_LIBRARY_PATH = lib.makeLibraryPath [
        elfutils
        gmp
        libffi
    ];
    shellHook = ''
        export LD_LIBRARY_PATH="$APPEND_LIBRARY_PATH:$LD_LIBRARY_PATH"
        . .shellhook
    '';
}

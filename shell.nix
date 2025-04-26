with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_19.stdenv; } {
    buildInputs = [
        (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
            HUnit
            unbounded-delays
        ]))

        elfutils
        glibcLocales
        gmp
        hlint
        libffi
        llvmPackages_19.lld
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

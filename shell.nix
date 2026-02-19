with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_21.stdenv; } {
    buildInputs = [
        (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
            HUnit
            unbounded-delays
        ]))
        hlint
        llvmPackages_21.lld
        ormolu
    ];
    APPEND_LIBRARY_PATH = lib.makeLibraryPath [
        elfutils
        gmp
        libffi
        numactl
    ];
    shellHook = ''
        export LD_LIBRARY_PATH="$APPEND_LIBRARY_PATH:$LD_LIBRARY_PATH"
        . .shellhook
    '';
}

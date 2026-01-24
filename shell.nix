let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  pkgs = import nixpkgs {};
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4;
in

pkgs.mkShell {
  nativeBuildInputs = with ocamlPackages; [
    dune_3
    findlib
    merlin
    ocaml
    ocamlformat
    ocp-browser
    ocp-indent
    ocp-index
    odoc
    utop
    # === build ===
    crunch
    # =============

  ];
  buildInputs = with ocamlPackages; [
    astring
    bos
    cmarkit
    cow
    inotify
    otoml
  ];
}

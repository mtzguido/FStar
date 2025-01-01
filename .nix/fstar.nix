{ callPackage, installShellFiles, lib, makeWrapper, buildDunePackage, version, z3, bash,
    batteries,
    menhir,
    menhirLib,
    pprint,
    ppx_deriving,
    ppx_deriving_yojson,
    ppxlib,
    process,
    sedlex,
    stdint,
    yojson,
    zarith,
    memtrace,
    mtime } :

buildDunePackage {
  pname = "fstar";
  inherit version;

  duneVersion = "3";

  nativeBuildInputs = [ installShellFiles makeWrapper menhir ];

  buildInputs = [
    batteries
    menhir
    menhirLib
    pprint
    ppx_deriving
    ppx_deriving_yojson
    ppxlib
    process
    sedlex
    stdint
    yojson
    zarith
    memtrace
    mtime
  ];

  enableParallelBuilding = true;

  prePatch = ''
    patchShebangs .scripts/*.sh
    patchShebangs ulib/ml/app/ints/mk_int_file.sh

    # The Makefile specifies bash, change it to Nix's bash.
    sed -i 's,SHELL=.*,SHELL=${bash}/bin/bash,' Makefile
  '';

  src = lib.sourceByRegex ./.. [
    "Makefile"
    "src.*"
    "mk.*"
    "stage*.*"
    "ulib.*"
    "doc.*"
    "version.txt"
  ];

  buildPhase = ''
    export PATH="${z3}/bin:$PATH"
    make -j$(nproc)
  '';

  installPhase = ''
    PREFIX=$out make install

    for binary in $out/bin/*
    do
      chmod +x $binary
      wrapProgram $binary --prefix PATH ":" ${z3}/bin
    done

    cd $out
    installShellCompletion --bash ${../.completion/bash/fstar.exe.bash}
    installShellCompletion --fish ${../.completion/fish/fstar.exe.fish}
    installShellCompletion --zsh --name _fstar.exe ${
      ../.completion/zsh/__fstar.exe
    }
  '';
}

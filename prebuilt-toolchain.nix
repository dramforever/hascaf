{ stdenv, fetchurl, autoPatchelfHook
, expat, lzma, ncurses5
}:

stdenv.mkDerivation rec {
  arch = "riscv64-unknown-elf";
  name = "${arch}-gcc-8.3.0-2019.08.0-x86_64-linux-ubuntu14";

  src = fetchurl {
    url = "https://static.dev.sifive.com/dev-tools/${name}.tar.gz";
    sha256 = "03vyx11p110crck1g2fgrp8mv6b87i56avi5nw19045nmpv1p1cj";
  };

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ expat lzma ncurses5 stdenv.cc.cc.lib ];

  dontAutoPatchelf = true;
  dontFixup = true; # No need to fixup pre-packaged stuff

  installPhase = ''
    mkdir "$out"
    cp -rp . "$out"

    for dir in bin lib libexec "$arch"/bin; do
      autoPatchelf "$out/$dir"
    done
  '';

}

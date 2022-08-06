{ pkgs
, cole-db-converter
, db-analyser
, onlyImmutableDB ? true
}:
let
  immutableDBStr = if onlyImmutableDB then "--onlyImmutableDB" else "";
  bcc-mainnet-config = pkgs.fetchurl {
    url = https://raw.githubusercontent.com/The-Blockchain-Company/bcc-node/114ee7f3b1cb55d384f928552c6b0871d4ca27ff/configuration/mainnet-genesis.json;
    sha256 = "1ahkdhqh07096law629r1d5jf6jz795rcw6c4vpgdi5j6ysb6a2g";
  };
  bcc-mainnet-mirror = pkgs.fetchFromGitHub {
    owner = "The-Blockchain-Company";
    repo = "bcc-mainnet-mirror";
    rev = "a31ac7534ec855b715b9a6bb6a06861ee94935d9";
    sha256 = "1z51ak4f7klz5pv2kjgaj5jv6agn2aph2n172hjssmn8x1q2bdys";
  };
  mainnet-converted = pkgs.runCommand "convert-mainnet"
    { buildInputs = [ cole-db-converter ]; }
    ''
      ${cole-db-converter}/bin/db-converter \
        --epochDir ${bcc-mainnet-mirror}/epochs \
        --dbDir $out \
        --epochSlots 21600
    '';
in
pkgs.runCommand "validate-mainnet"
{ buildInputs = [ cole-db-converter ]; }
  ''
    mkdir $out
    cp -r ${mainnet-converted}/* $out
    chmod -R u+rw,g+rw,a+x $out/*
    ${db-analyser}/bin/db-analyser \
      --db $out \
      cole \
      --genesisHash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb \
      --configCole ${bcc-mainnet-config} \
      --threshold 0.22 \
      ${immutableDBStr}
  ''

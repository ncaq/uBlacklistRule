{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs =
    inputs@{
      nixpkgs,
      flake-parts,
      treefmt-nix,
      haskellNix,
      ...
    }:
    let
      # `cabal.project`の`with-compiler`で指定したGHCバージョンを尊重し、
      # 対応するnixpkgsのパッケージセットを選択します。
      # こうすることでGHCバージョンの管理が`cabal.project`に一元化されます。
      cabalHaskellGhcVersion =
        let
          m = builtins.match ".*with-compiler:[[:space:]]*ghc-([0-9.]+).*" (
            builtins.readFile ./cabal.project
          );
        in
        if m == null then throw "cabal.projectにwith-compilerが見つかりません" else builtins.head m;
      ghc-version = "ghc${builtins.replaceStrings [ "." ] [ "" ] cabalHaskellGhcVersion}";
      # toolのバージョン。
      cabal-gild-version = "1.6.0.4";
      cabal-version = "3.16.1.0";
      hls-version = "2.13.0.0";
      implicit-hie-version = "0.1.4.0";
      # cabalビルドに必要なファイルのみを含める。
      # 関係ないファイル(.editorconfig, .githubなど)の変更で内部derivationのhashが動き、
      # キャッシュミスが発生するのを避ける。
      cabalFileset =
        lib:
        lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            # dir
            ./app
            ./src
            ./test
            # `Host.hs`がfile-embedでコンパイル時に埋め込むためビルドに必要。
            ./asset
            # file
            ./cabal.project
            ./uBlacklistRule.cabal
            ./README.md
            ./LICENSE
          ];
        };
      overlays = [
        haskellNix.overlay
        (
          final: _prev:
          let
            # haskell.nixのtoolsで参照されるhaskell-language-server。
            tool-haskell-language-server =
              final.haskell-nix.tool ghc-version "haskell-language-server"
                hls-version;
          in
          {
            # nixpkgsとhaskell-language-serverの対応バージョンが異なる場合があるため、
            # hlsからパッケージを取ってくる。
            inherit (tool-haskell-language-server.project.hsPkgs.fourmolu.components.exes) fourmolu;
            inherit (tool-haskell-language-server.project.hsPkgs.hlint.components.exes) hlint;
          }
        )
        (final: prev: {
          project = final.haskell-nix.cabalProject' {
            src = cabalFileset final.lib;
            compiler-nix-name = ghc-version;
            modules = [
              # `nix flake check`レベルではcabalの警告をエラーとして扱います。
              # ライブラリの問題ない範囲の不一致とか考えるとcabalの警告はエラーにしないべきですが、
              # CIでは通したくないので警告も含めてエラーにします。
              # 注意点としてsrcやtestはデフォルトでビルドされますが、
              # executableであるappはデフォルトではビルドされません。
              # 「appはエントリーポイントとしてのみ使う」習慣を守っていれば、
              # 問題にはならないはずです。
              (
                { lib, config, ... }:
                {
                  # パッケージたちをハードコーディングすると変更忘れが発生するので、
                  # `config.package-keys`で取得。
                  options.packages = lib.genAttrs config.package-keys (
                    _name:
                    lib.mkOption {
                      type = lib.types.submodule (
                        { config, lib, ... }:
                        # `cabal.project`に`source-repository-package`などで書かれていたりする、
                        # 外部パッケージは変更したくないので、
                        # `isProject`でフィルタリングしています。
                        lib.mkIf config.package.isProject {
                          # この書き方で上書きではなく追加として扱われる。
                          ghcOptions = [ "-Werror" ];
                        }
                      );
                    }
                  );
                }
              )
            ];
            shell = {
              tools = {
                cabal = cabal-version;
                cabal-gild = cabal-gild-version; # treefmtで管理されているが広く使えるように。
                haskell-language-server = hls-version;
                implicit-hie = implicit-hie-version;
              };
              # ランタイム依存。
              buildInputs = with prev; [
                # treefmtで指定したプログラムの単体版。
                actionlint
                deadnix
                editorconfig-checker
                fourmolu
                hlint
                nixfmt
                prettier
                shellcheck
                shfmt
                statix
                typos
                zizmor

                # nixの関連ツール。
                nil

                # GitHub関連ツール。
                gh

                # Haskell関連ツール。
                parallel
                zlib # aesonを開発環境でビルド。

                (writeScriptBin "haskell-language-server-wrapper" ''
                  #!${stdenv.shell}
                  exec haskell-language-server "$@"
                '')
              ];
            };
          };
        })
      ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
      ];

      perSystem =
        { system, ... }:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          flake = pkgs.project.flake { };
        in
        {
          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              actionlint.enable = true;
              deadnix.enable = true;
              nixfmt.enable = true;
              prettier.enable = true;
              shellcheck.enable = true;
              shfmt.enable = true;
              statix.enable = true;
              typos.enable = true;
              zizmor.enable = true;

              hlint = {
                enable = true;
                package = pkgs.hlint;
              };

              fourmolu = {
                enable = true;
                package = pkgs.fourmolu;
              };

              # cabal-gildはモジュール自動発見対応のためsettings.formatterでカスタム設定します。
            };
            settings.formatter = {
              # cabal-gildのモジュール自動発見機能に対応するため、
              # Haskellソースファイルの変更も検知してcabal-gildを実行します。
              # treefmt-nixの上流では、
              # 変更されたファイルだけを修正したいと言われてマージされていませんが、
              # ローカルで使う分には問題ありません。
              # https://github.com/numtide/treefmt-nix/pull/384
              cabal-gild = {
                command = pkgs.lib.getExe (
                  pkgs.writeShellApplication {
                    name = "cabal-gild-wrapper";
                    runtimeInputs = [
                      (pkgs.haskell-nix.tool ghc-version "cabal-gild" cabal-gild-version)
                      pkgs.git
                      pkgs.parallel
                    ];
                    text = ''
                      git ls-files -z "*.cabal" | parallel --null "cabal-gild --io {}"
                    '';
                  }
                );
                includes = [
                  "*.cabal"
                  # Haskellソースファイルの変更を検知するために含める
                  "*.hs"
                  "*.lhs"
                  "*.hsc"
                  "*.chs"
                  "*.hsig"
                  "*.lhsig"
                ];
              };
              editorconfig-checker = {
                command = pkgs.editorconfig-checker;
                includes = [ "*" ];
                excludes = [
                  "dist-newstyle/*"
                ];
              };
              zizmor.options = [ "--pedantic" ];
            };
          };

          checks =
            # テストがないパッケージもビルドしてエラーを検出する。
            # テストの実行パッケージを後に書くことで上書き。
            flake.packages
            // flake.checks
            // {
              # テスト実行でStack Exchange APIへのネットワークアクセスが必要なため、
              # このderivationのみサンドボックスを無効にする。
              # またHTTPS接続のTLS検証にはCA証明書が必要なので、
              # `cacert`を渡して`SSL_CERT_FILE`を設定する。
              "uBlacklistRule:test:uBlacklistRule-test" =
                flake.checks."uBlacklistRule:test:uBlacklistRule-test".overrideAttrs
                  (old: {
                    __noChroot = true;
                    buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.cacert ];
                    SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
                  });
            };

          packages = flake.packages // {
            default = flake.packages."uBlacklistRule:exe:uBlacklistRule";
          };

          apps = flake.apps // {
            default = flake.apps."uBlacklistRule:exe:uBlacklistRule";
          };

          inherit (flake) devShells;
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org/"
      "https://niks3-public.ncaq.net/"
      "https://ncaq.cachix.org/"
      "https://nix-community.cachix.org/"
      "https://cache.iog.io/"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "niks3-public.ncaq.net-1:e/B9GomqDchMBmx3IW/TMQDF8sjUCQzEofKhpehXl04="
      "ncaq.cachix.org-1:XF346GXI2n77SB5Yzqwhdfo7r0nFcZBaHsiiMOEljiE="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true; # haskell.nixが必要とします。
    # __noChroot = true を持つderivationのみサンドボックス外で実行する。
    sandbox = "relaxed";
  };
}

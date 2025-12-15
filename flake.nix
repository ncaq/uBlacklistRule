{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2511";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs = {
        nixpkgs.follows = "haskellNix/nixpkgs-2511";
      };
    };
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      haskellNix,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            project = final.haskell-nix.stackProject' {
              src = final.haskell-nix.haskellLib.cleanSourceWith {
                src = ./.;
                name = "uBlacklistRule-source";
              };
              name = "uBlacklistRule";
              shell = {
                tools = {
                  fourmolu = "latest";
                  haskell-language-server = "latest";
                  hlint = "latest";
                  stack = "latest";
                };
                buildInputs = with pkgs; [
                  # Haskell
                  (writeScriptBin "haskell-language-server-wrapper" ''
                    #!${stdenv.shell}
                    exec haskell-language-server "$@"
                  '')
                ];
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.project.flake { };
        treefmtEval = treefmt-nix.lib.evalModule pkgs (_: {
          # actionlintはセルフホストランナーの設定ファイルを正常に読み込まなかった。
          # yamlfmtはprettierと競合する。
          projectRootFile = "flake.nix";
          programs = {
            cabal-gild.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            nixfmt.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
            statix.enable = true;

            fourmolu = {
              enable = false; # 保留。
              package = pkgs.fourmolu;
            };
            prettier = {
              enable = true;
              excludes = [ "*.md" ]; # 保留。
            };
          };
        });
      in
      # hydraJobsもGitHub Actionsを使うため不要なので除外。
      builtins.removeAttrs flake [
        "ciJobs"
        "hydraJobs"
      ]
      // {
        checks =
          flake.packages # テストがないパッケージもビルドしてエラーを検出する。
          // flake.checks
          // {
            # テスト実行でネットワークアクセスが必要なため、このderivationのみサンドボックスを無効にする。
            "uBlacklistRule:test:uBlacklistRule-test" =
              flake.checks."uBlacklistRule:test:uBlacklistRule-test".overrideAttrs
                (_: {
                  __noChroot = true;
                });
            formatting = treefmtEval.config.build.check self;
          };
        formatter = treefmtEval.config.build.wrapper;
        packages = flake.packages // {
          default = flake.packages."uBlacklistRule:exe:uBlacklistRule";
        };
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://cache.iog.io"
      "https://u-blacklist-rule.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "u-blacklist-rule.cachix.org-1:KQl91lyBwQa6a6Np6WLqstaRmRH+R4ogkLzG9IV4Sjk="
    ];
    allow-import-from-derivation = true;
    # __noChroot = true を持つderivationのみサンドボックス外で実行する。
    sandbox = "relaxed";
  };
}

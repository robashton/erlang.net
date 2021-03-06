
let
  erlangReleases = builtins.fetchTarball https://github.com/nixerl/nixpkgs-nixerl/archive/v1.0.18-devel.tar.gz;

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "acc5f7b18a60bc9b1024e5e1882bf7362e6492e6";
    };


  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
      ];
    };

  erlangChannel = nixpkgs.nixerl.erlang-23-2-1.overrideScope' (self: super: {
    erlang = super.erlang.override {
      wxSupport = false;
    };
  });
in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
in

mkShell {
  buildInputs = with pkgs; [
    erlangChannel.erlang
    erlangChannel.rebar3
    erlangChannel.erlang-ls
    dotnetCorePackages.sdk_5_0
   ];
  shellHook = ''
      export DOTNET_LOCATION=${dotnetCorePackages.net_5_0}/shared/Microsoft.NETCore.App/5.0.0
    '';
}

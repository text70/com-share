sh <(curl -L https://nixos.org/nix/install) --daemon && \
sudo nix-channel --add https://nixos.org/channels/nix-unstable &&\
nix-channel --update && \
reset && \
sudo nix-shell --packages 'ghc.withPackages (pkgs: [ networking ])' && \
ghc -threaded share.hs && \
exit





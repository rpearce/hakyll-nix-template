FROM nixos/nix

RUN echo -e '\
allow-import-from-derivation = true\n\
auto-optimise-store = true\n\
experimental-features = nix-command flakes\n\
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com\n\
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=\n\
' >> /etc/nix/nix.conf

WORKDIR /service
COPY . .

# Install cachix if cachix.dhall is included
RUN [[ -f cachix.dhall ]] && \
    mkdir -p /root/.config/cachix/ && \
    mv cachix.dhall /root/.config/cachix/ && \
    nix-env -iA cachix -f "https://cachix.org/api/v1/install" || \
    true

RUN nix build --accept-flake-config

CMD ["nix", "run", ".", "watch"]

FROM debian:bullseye

RUN apt-get update && apt-get install -y --no-install-recommends \
  build-essential   \
  ca-certificates   \
  curl              \
  libffi7           \
  libffi-dev        \
  libgmp10          \
  libgmp-dev        \
  libncurses5       \
  libncurses-dev    \
  libtinfo5         \
  llvm-9            \
  llvm-9-dev        \
  pkg-config

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1            \
  BOOTSTRAP_HASKELL_GHC_VERSION=9.4.8           \
  BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.3.0      \
  BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1          \
  BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK=1     \
  BOOTSTRAP_HASKELL_ADJUST_BASHRC=1             \
  BOOTSTRAP_HASKELL_VERBOSE=1                   \
  sh

RUN echo 'source /root/.ghcup/env' >> /root/.bashrc
RUN /root/.ghcup/bin/cabal update 'hackage.haskell.org,2024-06-19T23:07:37Z'

COPY test-llvm-haskell-project /code
WORKDIR /code

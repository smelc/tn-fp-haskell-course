FROM mcr.microsoft.com/devcontainers/base:ubuntu-22.04

ARG GHC_VERSION=9.10.3
ARG CABAL_VERSION=latest
ARG HLS_VERSION=2.14.0.0

# ghcup system requirements: https://www.haskell.org/ghcup/install/#system-requirements
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        build-essential \
        curl \
        libffi-dev \
        libffi8ubuntu1 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libncurses5 \
        libtinfo5 \
        pkg-config \
        zlib1g-dev \
        python3 \
        python3-pip \
        python3-venv \
    && rm -rf /var/lib/apt/lists/*

# Install grip (used to render exam.md to PDF, see README.md)
RUN pip3 install --no-cache-dir grip

USER vscode
WORKDIR /home/vscode

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL_VERSION} \
    BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
    BOOTSTRAP_HASKELL_HLS_VERSION=${HLS_VERSION} \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 \
    GHCUP_INSTALL_BASE_PREFIX=/home/vscode \
    PATH=/home/vscode/.cabal/bin:/home/vscode/.ghcup/bin:${PATH}

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
    && ghcup set ghc ${GHC_VERSION} \
    && cabal update

COPY --chown=vscode:vscode cabal.project tn-fp-course.cabal /home/vscode/
RUN cabal build --only-dependencies all

COPY --chown=vscode:vscode slides /home/vscode/slides/
COPY --chown=vscode:vscode tps /home/vscode/tps/

RUN cabal build all

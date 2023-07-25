# This image is based on Github's developers image.

FROM mcr.microsoft.com/vscode/devcontainers/universal:2.4.1

# Base dependencies: opam
# python3 (for interactive tests)
# libicu (for .NET, cf. https://aka.ms/dotnet-missing-libicu )
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      ca-certificates \
      wget \
      git \
      gnupg \
      sudo \
      python3 \
      python-is-python3 \
      opam \
    && apt-get clean -y

# TODO: this is hardcoded to work on Github codespaces.
ARG USER=codespace
USER $USER
ENV HOME /home/$USER
WORKDIR $HOME
RUN mkdir -p $HOME/bin

# Install dotnet
ENV DOTNET_ROOT $HOME/dotnet
RUN wget -nv https://download.visualstudio.microsoft.com/download/pr/cd0d0a4d-2a6a-4d0d-b42e-dfd3b880e222/008a93f83aba6d1acf75ded3d2cfba24/dotnet-sdk-6.0.400-linux-x64.tar.gz && \
    mkdir -p $DOTNET_ROOT && \
    tar xf dotnet-sdk-6.0.400-linux-x64.tar.gz -C $DOTNET_ROOT && \
    echo 'export PATH=$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools' | tee --append $HOME/.profile $HOME/.bashrc $HOME/.bash_profile && \
    rm -f dotnet-sdk*.tar.gz

# Install OCaml and packages
ARG OCAML_VERSION=4.12.0
RUN opam init --compiler=$OCAML_VERSION --disable-sandboxing
RUN opam install --yes \
    batteries \
    zarith \
    stdint \
    yojson \
    dune \
    menhir \
    menhirLib \
    pprint \
    sedlex \
    ppxlib \
    process \
    ppx_deriving \
    ppx_deriving_yojson

# Get compiled Z3 4.8.5 and install
RUN wget -nv https://github.com/Z3Prover/z3/releases/download/Z3-4.8.5/z3-4.8.5-x64-ubuntu-16.04.zip \
 && unzip z3-4.8.5-x64-ubuntu-16.04.zip \
 && cp z3-4.8.5-x64-ubuntu-16.04/bin/z3 $HOME/bin/z3 \
 && rm -r z3-4.8.5-*

# Instrument profile/bashrc to set the opam switch. Note that this
# just appends the *call* to eval $(opam env) in these files, so we
# compute the new environments fter the fact. Calling opam env here
# would perhaps thrash some variables set by the devcontainer infra.
RUN echo 'eval $(opam env --set-switch)' | tee --append $HOME/.profile $HOME/.bashrc $HOME/.bash_profile

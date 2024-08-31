FROM ubuntu:23.10

# Base dependencies: opam
# python3 (for interactive tests)
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
           git \
           sudo \
           python3 \
           python-is-python3 \
           opam \
           rustc \
           npm \
    && apt-get clean -y

RUN useradd --create-home --shell /bin/bash builder
RUN echo 'builder ALL=NOPASSWD: ALL' >> /etc/sudoers
USER builder
WORKDIR /home/builder

# Install OCaml
ARG OCAML_VERSION=4.14.2
RUN opam init --compiler=$OCAML_VERSION --disable-sandboxing --shell-setup
RUN opam option depext-run-installs=true
ENV OPAMYES=1

# F* dependencies. This is the only place where we read a file from
# the F* repo.
ADD fstar.opam ./fstar.opam
RUN opam install --confirm-level=unsafe-yes --deps-only ./fstar.opam && opam clean

# Some karamel dependencies
RUN opam install --confirm-level=unsafe-yes fix fileutils visitors camlp4 wasm ulex uucp ctypes ctypes-foreign && opam clean

# I really with I could get rid of this crap.
ENV HOME /home/builder
RUN opam env --set-switch | tee --append $HOME/.profile $HOME/.bashrc $HOME/.bash_profile

# Move up
RUN rm ./fstar.opam

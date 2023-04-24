FROM fstar_ci_base

RUN git clone https://github.com/mtzguido/z3
WORKDIR z3

RUN ./configure

RUN make -C build -j$(nproc)

USER root
RUN make -C build install

USER opam
WORKDIR $HOME

# Keep old Z3 accessible
RUN ln -sf $(which z3) /home/opam/bin/z3-4.8.5

# Links for new Z3
RUN ln -sf /usr/bin/z3 /home/opam/bin/z3
RUN ln -sf /usr/bin/z3 /home/opam/bin/z3-4.12.3

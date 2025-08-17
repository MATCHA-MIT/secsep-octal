# syntax=docker/dockerfile:1

FROM ocaml/opam:ubuntu-24.04-ocaml-5.2

USER root
ENV HOME=/root
WORKDIR $HOME/octal

RUN --mount=type=cache,target=/var/cache/apt \
    apt-get update && \
    rm -rf /var/cache/apt/archives/lock && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    software-properties-common \
    curl zsh git vim \
    pipx \
    autoconf pkg-config \
    g++ gdb \
    opam

# Install and use oh-my-zsh
RUN curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh > install.sh && \
    sh install.sh && \
    rm install.sh
ENV THEME="agnoster"
RUN sed -i 's/^ZSH_THEME=".\+"$/ZSH_THEME="$THEME"/g' ~/.zshrc && \
    echo "zsh" >> ~/.bashrc

# Install Poetry
RUN --mount=type=cache,target=$HOME/.cache,uid=1000,gid=1000 \
    pipx install poetry && \
    pipx install virtualenv
ENV PATH="$HOME/.local/bin:$PATH"
# Create virtualenv
ENV VIRTUAL_ENV=$HOME/.venv
RUN virtualenv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:$PATH"
# Install dependencies
COPY pyproject.toml poetry.lock /root/benchmark/
RUN --mount=type=cache,target=/root/.cache \
    poetry install

# Install ocaml and required packages
# Keep it at the end since it unfolds and sets the PATH variable in .*rc file
RUN opam init && \
    opam install -y dune core z3 sexp && \
    echo "eval $(opam env)" >> ~/.zshrc

ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

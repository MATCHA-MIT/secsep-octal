# syntax=docker/dockerfile:1

FROM debian:bookworm-slim

USER root
ENV HOME=/root

RUN --mount=type=cache,target=/var/cache/apt \
    apt-get update && \
    rm -rf /var/cache/apt/archives/lock && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    software-properties-common libgmp-dev \
    autoconf pkg-config \
    git curl zsh vim tmux \
    pipx opam

# Install and use oh-my-zsh
RUN curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh > install.sh && \
    sh install.sh && \
    rm install.sh
ENV THEME="agnoster"
RUN sed -i 's/^ZSH_THEME=".\+"$/ZSH_THEME="$THEME"/g' ~/.zshrc
SHELL ["/bin/zsh", "-c"]
RUN chsh -s /bin/zsh

# Install Poetry
WORKDIR $HOME/octal
RUN --mount=type=cache,target=$HOME/.cache \
    pipx install poetry && \
    pipx install virtualenv
ENV PATH="$HOME/.local/bin:$PATH"
ENV VIRTUAL_ENV=$HOME/.venv
RUN virtualenv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:$PATH"
COPY pyproject.toml poetry.lock $HOME/octal/
RUN --mount=type=cache,target=$HOME/.cache \
    poetry install

# Install ocaml and required packages
# Keep it at the end since it unfolds and sets the PATH variable in .*rc file
RUN opam init -y && \
    opam switch create octal 5.3.0 && \
    opam install -y dune core z3 sexp && \
    echo "eval $(opam env --switch=octal)" >> ~/.zshrc

ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

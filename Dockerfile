FROM rust:1.71-bookworm as builder

VOLUME [ "/var/rinha" ]


RUN apt-get update && apt-get install wget lsb-release software-properties-common gnupg -y
RUN cd /tmp && wget https://apt.llvm.org/llvm.sh && chmod +x ./llvm.sh && ./llvm.sh 15
# RUN apt-get update && apt-get install -y llvm-14-dev libpolly-14-dev

WORKDIR /usr/src/rinha
COPY . .

RUN cargo install --path .

FROM debian:bookworm-slim

WORKDIR /usr/src/rinha

RUN mkdir ./core

COPY --from=builder /usr/src/rinha/core/librinha_core.so ./core/librinha_core.so
COPY --from=builder /usr/local/cargo/bin/rinhac /usr/local/bin/rinhac

ENTRYPOINT [ "rinhac" ]


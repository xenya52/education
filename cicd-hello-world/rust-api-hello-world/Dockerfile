# 1. This tells docker to use the Rust official image
FROM rust:latest

# 2. Copy the files in your machine to the Docker image
COPY ./ ./

# Build your program for release
RUN cargo build --release

EXPOSE 6767

# Run the binary
# CMD ["./target/release/hello-world"]

FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y
COPY --from=builder ./target/release/hello-world /usr/local/bin/myapp
CMD ["hello-world"]

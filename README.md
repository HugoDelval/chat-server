# chat TCP server

The repository implements a simple TCP multithreaded chat server for the CS4032 Trinity course.

## Dependencies

* stack
* haskell libs (cf *chat-server.cabal*)
 * base
 * network
 * parallel-io
 * split
 * network-info
 * directory
 * iproute

Note : Everything is installed when running ```stack build``` (or when running ```./compile.sh```)

## Run using stack

```bash
git clone https://github.com/HugoDelval/chat-server
cd chat-server
./compile.sh
./start.sh <PORT>
```

## Run on OpenNebula using Docker + Docker-machine

```bash
docker pull hugodelval/chat-server
docker run -i -w /app -p 0.0.0.0:8000:8000 --add-host dockerhost:`docker-machine ip test` hugodelval/chat-server /usr/local/bin/chat-server-exe 8000
```

## Author

Hugo DELVAL - 16336620


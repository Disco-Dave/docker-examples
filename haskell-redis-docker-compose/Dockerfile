FROM ubuntu:latest AS build

RUN apt-get update && apt-get install -y wget
RUN wget -qO- https://get.haskellstack.org/ | sh 

COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --dependencies-only

COPY . .
RUN stack install




FROM ubuntu:latest

COPY --from=build /root/.local/bin/haskell-redis-docker-compose-exe .

EXPOSE 8081
CMD ./haskell-redis-docker-compose-exe

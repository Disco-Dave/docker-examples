FROM ubuntu:latest AS build

COPY . .

RUN apt-get update && apt-get install -y wget
RUN wget -qO- https://get.haskellstack.org/ | sh 

RUN stack install




FROM ubuntu:latest

COPY --from=build /root/.local/bin/haskell-servant-multistage-exe .

EXPOSE 8081
CMD ./haskell-servant-multistage-exe

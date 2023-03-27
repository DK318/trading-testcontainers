FROM haskell:9.0.2
FROM ubuntu

COPY . /app

WORKDIR /app

RUN stack setup && stack build --only-dependencies
RUN stack build

EXPOSE 8081
CMD [ "stack exec -- trading-server" ]

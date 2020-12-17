FROM haskell:8.8.4 as dependencies

RUN mkdir /opt/build
WORKDIR /opt/build

COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --only-dependencies -j1 interpolate diagrams servant
RUN stack build --system-ghc --only-dependencies

# ---- SECOND LAYER ---

FROM haskell:8.8.4 as build

COPY --from=dependencies /root/.stack /root/.stack
RUN mkdir -p /opt/freenatalchart/bin

# Copy all (source, config, static) into the workdir
COPY . /opt/freenatalchart
WORKDIR /opt/freenatalchart

## now build the targets. 
RUN stack --system-ghc build
RUN stack --local-bin-path /opt/freenatalchart/bin install

# -- FINAL LAYER ---

# Using multi-stage builds:
# https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#use-multi-stage-builds
FROM debian:latest

# Add user and setup path (for local testing, ignored by Heroku)
RUN adduser fnc-api
USER fnc-api

COPY --from=build /opt/freenatalchart/bin /opt/freenatalchart
COPY --from=build /opt/freenatalchart/config /opt/freenatalchart/config
COPY --from=build /opt/freenatalchart/static /opt/freenatalchart/static

WORKDIR /opt/freenatalchart

CMD [ "/opt/freenatalchart/freenatalchart-exe" ]

FROM haskell:8.8.4 as build

RUN mkdir -p /opt/freenatalchart/bin

# Copy all (source, config, static) into the workdir
COPY . /opt/freenatalchart
WORKDIR /opt/freenatalchart

# Install dependencies and move binary to `/bin`
RUN stack --system-ghc build -j1 servant diagrams
RUN stack --system-ghc build -j1
RUN stack --local-bin-path /opt/freenatalchart/bin install

# Using multi-stage builds:
# https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#use-multi-stage-builds
FROM debian:latest

# Add user and setup path (for local testing, ignored by Heroku)
RUN adduser fnc-api
USER fnc-api

COPY --from=build /opt/freenatalchart/bin /opt/freenatalchart/bin
COPY --from=build /opt/freenatalchart/config /opt/freenatalchart/config
COPY --from=build /opt/freenatalchart/static /opt/freenatalchart/static

CMD [ "/opt/freenatalchart/bin/freenatalchart-exe" ]

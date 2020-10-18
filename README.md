# freenatalchart.xyz

## Setup

Create a `.env` file with the following entries

    ALGOLIA_APP_ID=<ALGOLIA>
    ALGOLIA_APP_KEY=<ALGOLIA-PUBLIC-SEARCH-KEY>
    GEONAMES_USERNAME=<GET-FROM-GEONAMES>
    PORT=3030

## Docker

### Building the docker image

    docker build -t freenatalchart .

Should take about 15 mins if doing it from scratch. The final image uses Debian, so between the base
and our own files, it comes to about 160 MB.

### Testing docker image locally

    docker run -p 3030:3030 --env-file .env freenatalchart

### Deploy to Heroku

    heroku container:push web -a freenatalchart-staging # (or freenatalchart-prod)
    heroku container:release web -a freenatalchart-staging # (or freenatalchart-prod)
    heroku open # to see it in the local browser


## Execute locally

    stack run

You'll need to set `ALGOLIA_APP_ID` and `ALGOLIA_APP_KEY` as env vars. (**TODO**: read `.env` files in Haskell, too.)

## Run tests

`stack test`

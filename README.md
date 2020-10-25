# freenatalchart.xyz

![build](https://github.com/lfborjas/freenatalchart.xyz/workflows/Haskell%20CI/badge.svg)


## Setup

Create a `.env` file with the following entries

    ALGOLIA_APP_ID=<ALGOLIA>
    ALGOLIA_APP_KEY=<ALGOLIA-PUBLIC-SEARCH-KEY>
    GEONAMES_USERNAME=<GET-FROM-GEONAMES>
    PORT=3030
    DEPLOY_ENV=Development

## Development

I personally use [haskell-language-server](https://github.com/haskell/haskell-language-server) in VS Code. The `haskell` extension should
be able to download the right binaries, and use `hie.yaml` to configure the project. 

To get everything going the first time, you can run `stack build` (look into [`ghcup`](https://www.haskell.org/ghcup/) to install all the haskell stuffs.)

Working locally should be rather painless, it's a simple `stack` project; you can run the server with:

    stack run

(You'll need to set `ALGOLIA_APP_ID` and `ALGOLIA_APP_KEY` as env vars for geolocation to work locally -- **TODO**: read `.env` files in Haskell, too.)

### With Docker:

#### Build docker image

    docker build -t freenatalchart .

Should take about 25 mins if doing it from scratch. The final image uses Debian, so between the base
and our own files, it comes to about 160 MB.

#### Testing docker image locally

    docker run -p 3030:3030 --env-file .env freenatalchart

## Deployment to Heroku

The examples here use my own deployment pipeline, to which you won't have access. You can create your own
heroku app for free (with `heroku create your-app-name`) and replace `freenatalchart-staging` with your app's name.

    heroku container:push web -a freenatalchart-staging # (or freenatalchart-prod)
    heroku container:release web -a freenatalchart-staging # (or freenatalchart-prod)
    heroku open # to see it in the local browser

(More info at [heroku's guide to docker deployments](https://devcenter.heroku.com/articles/container-registry-and-runtime))

**Note:** `heroku container:push ... ` will build the docker image if it hasn't been built yet; this can take up to 30 mins,
depending on your hardware.

### Testing

For rendering HTML and SVG, we use [golden tests](https://ro-che.info/articles/2017-12-04-golden-tests) with the [hspec-golden](https://github.com/stackbuilders/hspec-golden) library. The files are written to `test/files` and we only track the "golden" (expected) file. You'll notice that we `.gitignore` the `actual` file -- but it is produced by the test runner. The files are saved without extension, but they should be viewable in a browser if one sets the extension, or with `python3 -m http.server 8000`. 

If you've updated the `Views` and the tests now fail because of the changes, but you've verified that they're working as expected (perhaps by manually producing the files for static inspection, see for example `renderTestIndex` in `Views.Index`,) you can have hspec-golden replace
the `golden` files with the current copy of the `actual` file (produced by running the now-failing test.)

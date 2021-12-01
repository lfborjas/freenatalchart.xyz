# freenatalchart.xyz

![build](https://github.com/lfborjas/freenatalchart.xyz/workflows/Haskell%20CI/badge.svg)


## Setup

Create a `.env` file with the following entries

    ALGOLIA_APP_ID=<ALGOLIA>
    ALGOLIA_APP_KEY=<ALGOLIA-PUBLIC-SEARCH-KEY>
    PORT=3030
    DEPLOY_ENV=Development

## Development

I personally use [haskell-language-server](https://github.com/haskell/haskell-language-server) in VS Code. The `haskell` extension should
be able to download the right binaries, and use `hie.yaml` to configure the project. 

This project uses `nix`, you can enter a `nix-shell` to get everything going, and preview anything that needs to be built
with `nix-build --dry-run ./nix/release.nix`

Working locally should be rather painless, from a `nix-shell`:

    cabal run

(You'll need to set `ALGOLIA_APP_ID` and `ALGOLIA_APP_KEY` as env vars for geolocation to work locally.)

### With Docker:

#### Build docker image

    docker build -t freenatalchart .

Should take about 25 mins if doing it from scratch. The final image uses Debian, so between the base
and our own files, it comes to about 160 MB.

#### Testing docker image locally

    docker run -p 3030:3030 --env-file .env freenatalchart

## Deployment to Heroku

See `.github/workflows/heroku.yml` (and its staging variant) for how deployment is automated. Any merges into `develop` will deploy to staging,
`master` deploys to production. These workflows **will not work** if you fork this repository, since they depend on my heroku key being available;
but you should be able to re-use them by substituting `freenatalchart-` in both files with your own app name.

(More info at [heroku's guide to docker deployments](https://devcenter.heroku.com/articles/container-registry-and-runtime))

### Testing

For rendering HTML and SVG, we use [golden tests](https://ro-che.info/articles/2017-12-04-golden-tests) with the [hspec-golden](https://github.com/stackbuilders/hspec-golden) library. The files are written to `test/files` and we only track the "golden" (expected) file. You'll notice that we `.gitignore` the `actual` file -- but it is produced by the test runner on each run, locally. The files are saved without extension, but they should be viewable in a browser if one sets the extension (once the extension is there, you can also navigate the directory with `python3 -m http.server 8000`.) I personally just use the `renderTest*` functions in `Dev.hs` while developing.

If you've updated the `Views` and the tests now fail because of the changes, but you've verified that they're working as expected (perhaps by manually producing the files for static inspection, see for example `renderTestIndex` in `Dev.hs`,) you can have hspec-golden replace
the `golden` files with the current copy of the `actual` file (produced by running the now-failing test,) by running

    cabal exec hgold -- -u test/files    

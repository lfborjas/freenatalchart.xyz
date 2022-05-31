# freenatalchart.xyz

![build](https://github.com/lfborjas/freenatalchart.xyz/workflows/Build%20and%20Test/badge.svg)


## Development

I personally use [haskell-language-server](https://github.com/haskell/haskell-language-server) in VS Code. The `haskell` extension should
be able to download the right binaries, and use `hie.yaml` to configure the project. 

This project uses `nix`, you can enter a `nix-shell` to get everything going, or you can preview anything that needs to be built
with `nix-build --dry-run ./nix/release.nix`. Ideally, _most_ of our dependencies are in the Nix cache, and you only need to build
`swiss-ephemeris`, `timezone-detect`, `almanac` and this project.

Working locally should be rather painless, from a `nix-shell`:

    cabal run freenatalchart-exe

(You'll need to set `GEOCODE_API_KEY` as env vars for geolocation to work locally.)

## Deployment to Heroku

See `.github/workflows/heroku.yml` (and its staging variant) for how deployment is automated. Any merges into `develop` will deploy to staging,
`master` deploys to production. These workflows **will not work** if you fork this repository, since they depend on my heroku key being available;
but you should be able to re-use them by substituting `freenatalchart-` in both files with your own app name (run `heroku authorizations:create`
to generate an API token that you can put in your GH secrets, too.)

(More info at [heroku's guide to docker deployments](https://devcenter.heroku.com/articles/container-registry-and-runtime))

### Testing

For rendering HTML and SVG, we use [golden tests](https://ro-che.info/articles/2017-12-04-golden-tests) with the [hspec-golden](https://github.com/stackbuilders/hspec-golden) library. The files are written to `test/files` and we only track the "golden" (expected) file. You'll notice that we `.gitignore` the `actual` file -- but it is produced by the test runner on each run, locally. The files are saved without extension, but they should be viewable in a browser if one sets the extension (once the extension is there, you can also navigate the directory with `python3 -m http.server 8000`.) I personally just use the `renderTest*` functions in `Dev.hs` while developing.

If you've updated the `Views` and the tests now fail because of the changes, but you've verified that they're working as expected (perhaps by manually producing the files for static inspection, see for example `renderTestIndex` in `Dev.hs`,) you can have hspec-golden replace
the `golden` files with the current copy of the `actual` file (produced by running the now-failing test,) by running

    cabal exec hgold -- -u test/files    

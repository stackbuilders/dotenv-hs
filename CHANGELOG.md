## MASTER
## Dotenv 0.5.1.0

* Add support for command substitution on env vars.

## Dotenv 0.5.0.2

* Set `.env` file as default file for environment variables.
* Add `--version` flag to check the version of dotenv that is in use.

## Dotenv 0.5.0.0

* Add [dotenv-safe functionality](https://www.npmjs.com/package/dotenv-safe)
* Add the `Config` type with options to override env variables, and setting the
path for .env and .env.example files.
* Changed `loadFile` function to get `Config` with the paths for the .env file
and the .env.example file.

## Dotenv 0.4.0.0

* Use Megaparsec 6.0
* Dropped support for GHC 7.6

## Dotenv 0.3.4.0

* Allow optparse-applicative 0.14

## Dotenv 0.3.3.0

* Add support for variable expansion. Thanks to حبيب الامين (GitHub: habibalamin) for making this contribution.

## Dotenv 0.3.2.0

* Add the option to pass arguments to the program passed to Dotenv. Thanks to
  Oleg Grenrus (GitHub: phadej) for making this contribution.

## Dotenv 0.3.1.0

* Made interface more polymorphic so the functions works in any instance of
  `MonadIO`, not only `IO`. This should reduce amount of lifting in some
  cases.

* Added `onMissingFile` helper to deal with possibly missing files.

* Parser was rewritten to take full advantage of Megaparsec.
  `hspec-megaparsec` is now used for testing of the parser.

* Dropped support for GHC 7.4.

## Dotenv 0.3.0.3

* Allow optparse-applicative 0.13

## Dotenv 0.3.0.1

* Remove unnecessary package dependencies.

## Dotenv 0.3.0.0

* Reverted change to Data.Text in favor of String, for maintaining compatibility
  with common Haskell system libraries. Added separate interface for parsing a
  file into tuples containing Data.Text values. Thanks to Daisuke Fujimura
  (GitHub: fujimura).
* Fixed parsing of CRLF characters for Windows users.

## Dotenv 0.2.0.0 (deprecated)

* Changed public interfaces to use Data.Text.
* Added function `parseFile` to read dotenv file without modifying the
  environment. Thanks to Daisuke Fujimura (GitHub: fujimura) for making this
  contribution.

## Dotenv 0.1.0.0

* First public release.

## MASTER
## Dotenv 0.8.0.5
* Extend ghc support to 8.8 and 8.10

## Dotenv 0.8.0.4
*  Fix test fixtures

## Dotenv 0.8.0.3
* Add suppport for `megaparsec-8.0.0`

## Dotenv 0.8.0.2
* Add support for GHC 8.6

## Dotenv 0.8.0.1
* Support for `optparse-applicative-0.15`

## Dotenv 0.8.0.0
* Add `Configuration.Dotenv.Environment` module exporting functions from `System.Environment`,
  `System.Environment.Compat`, or `System.Environment.Blank`, depending on `base` version.
* Add support for blank environment variables for `base` >= 4.11.0.0.

## Dotenv 0.7.0.0
* Hide helper modules in other-modules

## Dotenv 0.6.0.3
* Reexport `defaultConfig` in `Configuration.Dotenv` (thanks to: matsubara0507)

## Dotenv 0.6.0.2
* Support for `process-1.6.3.0`

## Dotenv 0.6.0.1
* Add support for `megaparsec-7.0.1`
* Drop support for `GHC 7.8.4`

## Dotenv 0.6.0.0
* Move `loadSafeFile` to `Configuration.Dotenv`
* Export `Configuration.Dotenv.Types` from `Configuration.Dotenv`
* Change `loadSafeFile` signature to accept different types of validators.
* Add `type ValidatorMap = Map Text (Text -> Bool)` for custom validations.

## Dotenv 0.5.2.5
* Update `exceptions` bounds `>= 0.8 && < 0.11`

## Dotenv 0.5.2.4
* Add error message when there is more than one definition in the Scheme for the
same env

## Dotenv 0.5.2.3
* Update bounds `exceptions` == 0.9.*
* Support `megaparsec` >= 6.4.0

## Dotenv 0.5.2.1

* Update documentation for Configuration.Dotenv.Types

## Dotenv 0.5.2.0

* Add `loadSafeFile` to typecheck the envs.
* Add `(--schema|-s) FILE` flag to the `dotenv` CLI tool to enable safe mode.
* Add `(--no-schema)` flag to the `dotenv` CLI tool to disable safe mode.
* Turn safe mode on automatically when the `.schema.yml` file is present.
* Make `required` optional in the `.schema.yml`.

## Dotenv 0.5.1.1

* Allow `.env` empty files

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

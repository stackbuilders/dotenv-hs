## MASTER

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

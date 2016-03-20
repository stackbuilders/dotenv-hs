## Dotenv 0.3.0.0

* Reverted change to Data.Text in favor of String, for maintaining compatibility
  with common Haskell system libraries. Added separate interface for parsing a
  file into tuples containing Data.Text values. Thanks to Daisuke Fujimura
  (Github: fujimura).
* Fixed parsing of CRLF characters for Windows users.

## Dotenv 0.2.0.0 (deprecated)

* Changed public interfaces to use Data.Text.
* Added function `parseFile` to read dotenv file without modifying the
  environment. Thanks to Daisuke Fujimura (Github: fujimura) for making this
  contribution.
  
## Dotenv 0.1.0.0

* First public release.

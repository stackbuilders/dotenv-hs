[![Build Status](https://travis-ci.org/stackbuilders/dotenv-hs.svg?branch=master)](https://travis-ci.org/stackbuilders/dotenv-hs) [![Hackage](https://img.shields.io/hackage/v/dotenv.svg)](http://hackage.haskell.org/package/dotenv)

# Dotenv files for Haskell

In most applications,
[configuration should be separated from code](http://12factor.net/config). While
it usually works well to keep configuration in the environment, there
are cases where you may want to store configuration in a file outside
of version control.

"Dotenv" files have become popular for storing configuration,
especially in development and test environments. In
[Ruby](https://github.com/bkeepers/dotenv),
[Python](https://github.com/theskumar/python-dotenv) and
[Javascript](https://www.npmjs.com/package/dotenv) there are libraries
to facilitate loading of configuration options from configuration
files. This library loads configuration to environment variables for
programs written in Haskell.

## Installation

In most cases you will just add `dotenv` to your cabal file. You can
also install the library and executable by invoking `stack install dotenv`.

## Usage

Set configuration variables in a file following the format below:

```
S3_BUCKET=YOURS3BUCKET
SECRET_KEY=YOURSECRETKEYGOESHERE
```

Then, calling `Dotenv.load` from your Haskell program reads the above
settings into the environment:

```haskell
import qualified Configuration.Dotenv as Dotenv
Dotenv.loadFile False "/path/to/your/file"
```

After calling `Dotenv.load`, you are able to read the values set in your
environment using standard functions from `System.Environment` such as
`lookupEnv` and `getEnv`.

### NOTE: Empty environment variables

If you need to have empty environment variables in your configuration, you can
use something like the code below:

```haskell
fromMaybe "" <$> lookupEnv "ENV_VAR"
```

Currently, `dotenv-hs` doesn't allow you to set empty environment variables,
because of [setEnv](https://hackage.haskell.org/package/base-4.9.1.0/docs/System-Environment.html#v:setEnv)
from our `System.Environment`. This is bug reported in [GHC ticket](https://ghc.haskell.org/trac/ghc/ticket/12494).
We have had many [dicussions](https://github.com/stackbuilders/dotenv-hs/issues/48)
about this. Fortunately, there is already some work for this issue in
[GHC Phabricator](https://phabricator.haskell.org/D3726).

## Configuration

The first argument to `loadFile` specifies whether you want to
override system settings. `False` means Dotenv will respect
already-defined variables, and `True` means Dotenv will overwrite
already-defined variables.

## Advanced Dotenv File Syntax

You can add comments to your Dotenv file, on separate lines or after
values. Values can be wrapped in single or double quotes. Multi-line
values can be specified by wrapping the value in double-quotes, and
using the "\n" character to represent newlines.

The [spec file](spec/Configuration/Dotenv/ParseSpec.hs) is the best
place to understand the nuances of Dotenv file parsing.

## Command-Line Usage

You can call dotenv from the command line in order to load settings
from one or more dotenv file before invoking an executable:

```
dotenv -f mydotenvfile myprogram
```

Aditionally you can pass arguments and flags to the program passed to
Dotenv:

```
dotenv -f mydotenvfile myprogram -- --myflag myargument
```

Hint: The `env` program in most Unix-like environments prints out the
current environment settings. By invoking the program `env` in place
of `myprogram` above you can see what the environment will look like
after evaluating multiple Dotenv files.

## Author

Justin Leitgeb

## License

MIT

## Copyright

(C) 2015-2017 [Stack Builders Inc.](http://www.stackbuilders.com)

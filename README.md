[![Build Status](https://travis-ci.org/stackbuilders/dotenv-hs.svg?branch=master)](https://travis-ci.org/stackbuilders/dotenv-hs) [![Hackage](https://img.shields.io/hackage/v/dotenv.svg)]()

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
also install the library and executable by invoking `cabal install dotenv`.

## Usage

Set configuration variables in a file following the format below:

```
S3_BUCKET=YOURS3BUCKET
SECRET_KEY=YOURSECRETKEYGOESHERE
```

Then, calling Dotenv.load from your Haskell program reads the above
settings into the environment.:

```haskell
import Configuration.Dotenv
Dotenv.loadFile False "/path/to/your/file"
```

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

Hint: The `env` program in most Unix-like environments prints out the
current environment settings. By invoking the program `env` in place
of `myprogram` above you can see what the environment will look like
after evaluating multiple  Dotenv files.

## Author

Justin Leitgeb

## License

MIT

## Copyright

(C) 2015 Stack Builders Inc.

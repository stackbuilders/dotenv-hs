[![Build Status](https://github.com/stackbuilders/dotenv-hs/actions/workflows/build.yml/badge.svg)](https://github.com/stackbuilders/dotenv-hs/actions/workflows/build.yml)[![Hackage](https://img.shields.io/hackage/v/dotenv.svg)](http://hackage.haskell.org/package/dotenv)

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

## Install

In most cases you will just add `dotenv` to your cabal file. You can
also install the library and executable by invoking `stack install dotenv` or
you can download the dotenv binaries from our
[releases](https://github.com/stackbuilders/dotenv-hs/releases) page.

## Usage

Set configuration variables in a file following the format below:

```
S3_BUCKET=YOURS3BUCKET
SECRET_KEY=YOURSECRETKEYGOESHERE
```

Then, calling `Dotenv.load` from your Haskell program reads the above
settings into the environment:

```haskell
import Configuration.Dotenv (loadFile, defaultConfig)
loadFile defaultConfig
```

After calling `Dotenv.load`, you are able to read the values set in your
environment using standard functions from `System.Environment` or
`System.Environment.Blank` (`base` >= 4.11.0.0), such as `getEnv`.

If your version of `base` is < 4.11.0.0, then setting an environment variable value to
a blank string will remove the variable from the environment entirely.

### Variable substitution

To use compound environment variables, use the following syntax within your environment variables: `${your_env_var}`. For instance:

```
DATABASE=postgres://${USER}@localhost/database
```

Running it on the CLI:

```
$ dotenv "echo $DATABASE"
postgres://myusername@localhost/database
```

### Command substitution

To use the standard output of a command in your environment variables, use the following syntax: `$(your_command)`. For instance:

```
DATABASE=postgres://$(whoami)@localhost/database
```

Running it on the CLI:

```
$ dotenv "echo $DATABASE"
postgres://myusername@localhost/database
```

### Configuration

The loadFile function accepts a configuration object as its first argument,
allowing you to customize the behavior of Dotenv.

You can use `defaultConfig` which parses the `.env` file in your current
directory without overriding your existing environment variables. You can
also define your own configuration with the `Config` type.

To define your own configuration, you can use the following configuration:

#### configPagh

CLI Option: `f`
Default: `[".env"]`

Specify a list of dotenv files where the environment variables are defined. For example:

```hs
configPath = [".env", ".tokens", ".public_keys"]
```

#### configOverride

CLI Option: `o` or `overload`
Default: `False`

Setting configOverride to `False` means Dotenv will respect already-defined variables.
If set to `True`, Dotenv will overwrite already-defined variables.

#### configExamplePath

CLI Option: `x` or `example`
Default: `[]`

You can specify a list of dotenv example files where you can define which environment
variables **must be defined** before running a program. For example:

```hs
configExamplePath = [".env.example", ".tokens.example", ".public_keys.example"]
```

If you don't need this functionality, you can set configExamplePath to an empty list.

### configVerbose

CLI Option: `verbose`
Default: `False`

Setting `configVerbose` to `False` means that Dotenv will not print any messages
when loading the environment variables. If set to `True`, Dotenv will print a message
when a variable is loaded.

#### allowDuplicates

CLI Option: `D` or `no-dups`
Default: `True`

Setting `allowDuplicate` to `False` means that Dotenv will not allow duplicate keys.
Instead, it will throw an error. If set to `True`, Dotenv will allow duplicate keys and
use the last one defined in the file (default behavior).

### Advanced Dotenv File Syntax

You can add comments to your Dotenv file, on separate lines or after
values. Values can be wrapped in single or double quotes. Multi-line
values can be specified by wrapping the value in double-quotes, and
using the "\n" character to represent newlines.

The [spec file](spec/Configuration/Dotenv/ParseSpec.hs) is the best
place to understand the nuances of Dotenv file parsing.

### Command-Line Usage

You can call dotenv from the command line in order to load settings
from one or more dotenv file before invoking an executable:

```shell
$ dotenv -f mydotenvfile myprogram
```

The `-f` flag is optional, by default it looks for the `.env` file in the current working directory.

```shell
$ dotenv myprogram
```

Additionally, you can pass arguments and flags to the program passed to
Dotenv:

```shell
$ dotenv -f mydotenvfile myprogram -- --myflag myargument
```

or:

```shell
$ dotenv -f mydotenvfile "myprogram --myflag myargument"
```

In addition to the configuration options mentioned earlier, Dotenv provides a [dotenv-safe functionality](https://www.npmjs.com/package/dotenv-safe)
that allows you to enforce the presence of specific environment variables before executing your program. You can utilize the `--example` flag to enable this feature.

By using the `--example` flag, you can define a list of strict environment variables
that must be defined either in the actual environment or in your dotenv files. This ensures that your program only runs when all the required environment variables are present.

For example:

```shell
$ cat .env.example
DOTENV=
FOO=
BAR=

$ cat .env
DOTENV=123

$ echo $FOO
123
```

This will fail:

```shell
$ dotenv -f .env --example .env.example "myprogram --myflag myargument"
> dotenv: The following variables are present in .env.example, but not set in the current environment, or .env: BAR
```

This will succeed:

```shell
$ export BAR=123 # Or you can do something like: "echo 'BAR=123' >> .env"
$ dotenv -f .env --example .env.example "myprogram --myflag myargument"
```

Hint: The `env` program in most Unix-like environments prints out the
current environment settings. By invoking the program `env` in place
of `myprogram` above you can see what the environment will look like
after evaluating multiple Dotenv files.

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---

<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)

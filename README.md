[![Build Status](https://github.com/stackbuilders/dotenv-hs/actions/workflows/build-and-test.yml/badge.svg)](https://github.com/stackbuilders/dotenv-hs/actions/workflows/build-and-test.yml) [![Hackage](https://img.shields.io/hackage/v/dotenv.svg)](http://hackage.haskell.org/package/dotenv)

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

In order to use compound env vars use the following sintax within your env vars
${your_env_var}. For instance:

```
DATABASE=postgres://${USER}@localhost/database
```

Running it on the CLI:

```
$ dotenv "echo $DATABASE"
postgres://myusername@localhost/database
```

### Command substitution

In order to use the standard output of a command in your env vars use the following
sintax $(your_command). For instance:

```
DATABASE=postgres://$(whoami)@localhost/database
```

Running it on the CLI:

```
$ dotenv "echo $DATABASE"
postgres://myusername@localhost/database
```

### Configuration

The first argument to `loadFile` specifies the configuration. You cans use
`defaultConfig` which parses the `.env` file in your current directory and
doesn't override your envs. You can also define your own configuration with
the `Config` type.

`False` in `configOverride` means Dotenv will respect
already-defined variables, and `True` means Dotenv will overwrite
already-defined variables.

In the `configPath` you can write a list of all the dotenv files where are
envs defined (e.g `[".env", ".tokens", ".public_keys"]`).

In the `configExamplePath` you can write a list of all the dotenv example files
where you can specify which envs **must be defined** until running a program
(e.g `[".env.example", ".tokens.example", ".public_keys.example"]`). If you don't
need this functionality you can set `configExamplePath` to an empty list.

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

The `-f` flag is optional, by default it looks for the `.env` file in the current
working directory.

```shell
$ dotenv myprogram
```

Aditionally you can pass arguments and flags to the program passed to
Dotenv:

```shell
$ dotenv -f mydotenvfile myprogram -- --myflag myargument
```

or:

```shell
$ dotenv -f mydotenvfile "myprogram --myflag myargument"
```

Also, you can use a `--example` flag to use [dotenv-safe functionality](https://www.npmjs.com/package/dotenv-safe)
so that you can have a list of strict envs that should be defined in the environment
or in your dotenv files before the execution of your program. For instance:

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
> dotenv: Missing env vars! Please, check (this/these) var(s) (is/are) set: BAR
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
<img src="https://www.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)

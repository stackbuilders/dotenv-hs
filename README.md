[![Build Status](https://github.com/stackbuilders/dotenv-hs/actions/workflows/build.yml/badge.svg)](https://github.com/stackbuilders/dotenv-hs/actions/workflows/build.yml)[![Hackage](https://img.shields.io/hackage/v/dotenv.svg)](http://hackage.haskell.org/package/dotenv)
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-24-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

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

In order to use compound env vars use the following syntax within your env vars
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

In order to use the standard output of a command in your env vars use the following syntax $(your_command). For instance:

```
DATABASE=postgres://$(whoami)@localhost/database
```

Running it on the CLI:

```
$ dotenv "echo $DATABASE"
postgres://myusername@localhost/database
```

### Surrond with quotes

If your value starts with a character that produces a parse error (e.g. `{`) . Surround your value
with quotes. You can also escape the quotes if they're inside your value. For example:

```
JSON_SQ='{"a":[1,2,3], "b": "\'asdf\'"}'
JSON_DQ="{\"a\":[1,2,3], \"b\": \"'asdf'\"}"
```

Run it:

```
$ dotenv "echo $JSON_SQ" | jq .a
[
  1,
  2,
  3
]
```

### Configuration

The first argument to `loadFile` specifies the configuration. You can use
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

A `False` in the `configVerbose` means that Dotenv will not print any message
when loading the envs. A `True` means that Dotenv will print a message when a variable is loaded.

When `configDryRyn` is `True`, Dotenv will print out the loaded environment variables without executing the program.

A `False` on `allowDuplicates` means that Dotenv will not allow duplicate keys, and instead it will throw
an error. A `True` means that Dotenv will allow duplicate keys, and it will use the last one defined in the file (default behavior).

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

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://cristhianmotoche.github.io/"><img src="https://avatars.githubusercontent.com/u/8370088?v=4?s=100" width="100px;" alt="Cristhian Motoche"/><br /><sub><b>Cristhian Motoche</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=CristhianMotoche" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://www.stackbuilders.com/news/author/justin-leitgeb"><img src="https://avatars.githubusercontent.com/u/9977?v=4?s=100" width="100px;" alt="Justin S. Leitgeb"/><br /><sub><b>Justin S. Leitgeb</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=jsl" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://markkarpov.com/"><img src="https://avatars.githubusercontent.com/u/8165792?v=4?s=100" width="100px;" alt="Mark Karpov"/><br /><sub><b>Mark Karpov</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=mrkkrp" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://juancarlos.io/"><img src="https://avatars.githubusercontent.com/u/2164411?v=4?s=100" width="100px;" alt="Juan Paucar"/><br /><sub><b>Juan Paucar</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=juanpaucar" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/jpvillaisaza"><img src="https://avatars.githubusercontent.com/u/584947?v=4?s=100" width="100px;" alt="Juan Pedro Villa Isaza"/><br /><sub><b>Juan Pedro Villa Isaza</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=jpvillaisaza" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="http://fujimuradaisuke.com/"><img src="https://avatars.githubusercontent.com/u/134072?v=4?s=100" width="100px;" alt="Daisuke Fujimura"/><br /><sub><b>Daisuke Fujimura</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=fujimura" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/cptrodolfox"><img src="https://avatars.githubusercontent.com/u/20303685?v=4?s=100" width="100px;" alt="William R. Arellano"/><br /><sub><b>William R. Arellano</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=cptrodolfox" title="Code">💻</a></td>
    </tr>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://dbalseiro.github.io/"><img src="https://avatars.githubusercontent.com/u/2053849?v=4?s=100" width="100px;" alt="Diego Balseiro"/><br /><sub><b>Diego Balseiro</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=dbalseiro" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/fefi95"><img src="https://avatars.githubusercontent.com/u/12057338?v=4?s=100" width="100px;" alt="Stefani Castellanos"/><br /><sub><b>Stefani Castellanos</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=fefi95" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://wikipedia.org/"><img src="https://avatars.githubusercontent.com/u/2220440?v=4?s=100" width="100px;" alt="Götz"/><br /><sub><b>Götz</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=goetzc" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://oleg.fi/"><img src="https://avatars.githubusercontent.com/u/51087?v=4?s=100" width="100px;" alt="Oleg Grenrus"/><br /><sub><b>Oleg Grenrus</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=phadej" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/sestrella"><img src="https://avatars.githubusercontent.com/u/2049686?v=4?s=100" width="100px;" alt="Sebastián Estrella"/><br /><sub><b>Sebastián Estrella</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=sestrella" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://git.sr.ht/~habibalamin"><img src="https://avatars.githubusercontent.com/u/1415293?v=4?s=100" width="100px;" alt="Habib Alamin"/><br /><sub><b>Habib Alamin</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=habibalamin" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/FranzGB"><img src="https://avatars.githubusercontent.com/u/46214532?v=4?s=100" width="100px;" alt="Franz Guzmán"/><br /><sub><b>Franz Guzmán</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=FranzGB" title="Code">💻</a></td>
    </tr>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://brisb.in/"><img src="https://avatars.githubusercontent.com/u/50812?v=4?s=100" width="100px;" alt="Pat Brisbin"/><br /><sub><b>Pat Brisbin</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=pbrisbin" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/aloussase"><img src="https://avatars.githubusercontent.com/u/84427521?v=4?s=100" width="100px;" alt="Alexander Goussas"/><br /><sub><b>Alexander Goussas</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=aloussase" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/alexisbcc"><img src="https://avatars.githubusercontent.com/u/38666191?v=4?s=100" width="100px;" alt="Alexis Crespo"/><br /><sub><b>Alexis Crespo</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=alexisbcc" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/anddriex"><img src="https://avatars.githubusercontent.com/u/42983405?v=4?s=100" width="100px;" alt="Andres Perez"/><br /><sub><b>Andres Perez</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=anddriex" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/ibarrae"><img src="https://avatars.githubusercontent.com/u/22796877?v=4?s=100" width="100px;" alt="Esteban Ibarra"/><br /><sub><b>Esteban Ibarra</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=ibarrae" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="http://ferandrade.com/"><img src="https://avatars.githubusercontent.com/u/6790161?v=4?s=100" width="100px;" alt="Fernanda Andrade"/><br /><sub><b>Fernanda Andrade</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=flandrade" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Jagl257"><img src="https://avatars.githubusercontent.com/u/27145248?v=4?s=100" width="100px;" alt="Jorge Guerra Landázuri"/><br /><sub><b>Jorge Guerra Landázuri</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=Jagl257" title="Code">💻</a></td>
    </tr>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://matsubara0507.github.io/whoami"><img src="https://avatars.githubusercontent.com/u/10684493?v=4?s=100" width="100px;" alt="MATSUBARA Nobutada"/><br /><sub><b>MATSUBARA Nobutada</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=matsubara0507" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/wojtekmach"><img src="https://avatars.githubusercontent.com/u/76071?v=4?s=100" width="100px;" alt="Wojtek Mach"/><br /><sub><b>Wojtek Mach</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=wojtekmach" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/DanRCM"><img src="https://avatars.githubusercontent.com/u/112514991?v=4?s=100" width="100px;" alt="DanRCM"/><br /><sub><b>DanRCM</b></sub></a><br /><a href="https://github.com/stackbuilders/dotenv-hs/commits?author=DanRCM" title="Code">💻</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---

<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)

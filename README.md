<!-- [![Hackage](http://img.shields.io/hackage/v/fhcontainer.png)](http://hackage.haskell.org/package/fhcontainer) -->
[![license](https://img.shields.io/badge/license-GPLv3+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)

# "Fedora Haskell" Container tool
This may eventually become a more general container tool.

# Usage
```shellsession
$ fhcontainer f40
$ fhcontainer c9s
$ fhcontainer debian
$ fhcontainer --list
```

`$ fhcontainer --version`

`$ fhcontainer --help`

```
Fedora container tool

Usage: fhcontainer [--version] [-n|--name NAME] [-p|--pull] [-V|--verbose]
                   [-m|--mount DIR] ((-l|--list) | DIST/IMAGE/CONTAINER)
                   [CMD+ARGs...]

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--name NAME           Container name
  -p,--pull                Pull latest image
  -V,--verbose             output more details
  -m,--mount DIR           mount directory into container
  -l,--list                List local images
```

## Build/installation

`stack install` or `cabal install`

In Fedora you can use `cabal-rpm builddep` to pre-install the dependencies
for cabal-install.

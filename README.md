# noaa-haskell

Client for CDO Web Services API in Haskell

## Introduction

This project contains a [Haskell][haskell] library intended to help clients use
the Climate Data Online (CDO) Web Services API. Please read the Web Services
[documentation][noaa] for more information.

## Development Instructions

This section is intended for developers of the project. Clients of the project
may safely skip this section.

We will assume that this project exists locally and we are logged into a shell
where the working directory is the root of the project.

This project uses [Stack][stack] to simplify dependency management.

### Build

Build the project with the command `stack build`. Build the documentation for
the project with the command `stack haddock`.

### Test

Test the project with the command `stack test`.

### Lint

If [HLint][hlint] is on the local path, then lint the project with the command
`hlint .`.

[haskell]: https://www.haskell.org
[stack]: https://www.haskellstack.org
[hlint]: https://github.com/ndmitchell/hlint
[noaa]: https://www.ncdc.noaa.gov/cdo-web/webservices/v2#gettingStarted

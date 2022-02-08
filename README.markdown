# Text-ICU: Comprehensive support for string manipulation

This package provides the Data.Text.ICU library, for performing
complex manipulation of Unicode text.  It provides features such as
the following:

* Unicode normalization

* Conversion to and from many common and obscure encodings

* Date and number formatting

* Comparison and collation

## Prerequisites

This library is implemented as bindings to the well-respected [ICU
library](https://icu.unicode.org/) (which is not bundled, and must
be installed separately).

### macOS

    brew install icu4c
    brew link icu4c --force

You might need:

    export PKG_CONFIG_PATH="/usr/local/opt/icu4c/lib/pkgconfig"

### Debian/Ubuntu 

    sudo apt-get update
    sudo apt-get install libicu-dev

### Fedora/CentOS

    sudo dnf install unzip libicu-devel

### Nix/NixOS

    nix-shell --packages icu

### Windows/stack

With `stack` on Windows, which comes with its own bundled MSYS2, the
following commands give up-to-date system dependencies for
`text-icu-0.8.0` (tested 2022-02-07):

    stack exec -- pacman --noconfirm -Sy msys2-keyring
    stack exec -- pacman --noconfirm -S mingw-w64-x86_64-icu
    stack exec -- pacman --noconfirm -S mingw-w64-x86_64-pkg-config


## Compatibility

Upstream ICU occasionally introduces backwards-incompatible API
breaks. This package tries to stay up to date with upstream, and is
currently more or less in sync with ICU 70.

Minimum required version is ICU 62.


## Get involved!

Please report bugs via the
[github issue tracker](https://github.com/haskell/text-icu/issues).


## Authors

This library was written by Bryan O'Sullivan.

# Text-ICU: Comprehensive support for string manipulation

[![Build Status](https://travis-ci.com/bos/text-icu.svg?branch=master)](https://travis-ci.com/bos/text-icu)

This package provides the Data.Text.ICU library, for performing
complex manipulation of Unicode text.  It provides features such as
the following:

* Unicode normalization

* Conversion to and from many common and obscure encodings


# Prerequisites

This library is implemented as bindings to the well-respected [ICU
library](http://www.icu-project.org/) (which is not bundled, and must
be installed separately).


# Compatibility

Upstream ICU occasionally introduces backwards-incompatible API
breaks.  This package tries to stay up to date with upstream, and is
currently more or less in sync with ICU 55.


# Get involved!

Please report bugs via the
[github issue tracker](http://github.com/bos/text-icu/issues).

Master [Mercurial repository](http://bitbucket.org/bos/text-icu):

* `hg clone http://bitbucket.org/bos/text-icu`

There's also a [git mirror](http://github.com/bos/text-icu):

* `git clone git://github.com/bos/text-icu.git`

(You can create and contribute changes using either Mercurial or git.)


# Authors

This library was written by Bryan O'Sullivan.

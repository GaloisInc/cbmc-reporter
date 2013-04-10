Summary
=======

*cbmc-reporter* is a driver for the
[CBMC model-checker](http://www.cprover.org/cbmc/) for use in verifying C
library code.  cbmc-reporter helps

1. utilize multi-threading when verifying a
large number of claims,
2. generates summary tables of resulting proofs,
3. aids in build-system integration for library code.

The authors of CBMC are not associated with nor do they endorse this software.
CBMC is open-source but please review its [license](http://www.cprover.org/cbmc/LICENSE) if you choose to use it.

License
=======
BSD-3 (see the license file).

Installation
============

cbmc-reporter is known to work on Linux and may work on OSX.  It will not work
on Windows (patches are welcomed to improve compatibility with other systems).

1. cbmc-reporter is written in Haskell, and requires
[GHC and cabal](http://www.haskell.org/platform/).  Install them.

2. CBMC (including binaries or sources) can be found
[here](http://www.cprover.org/cbmc/).  Install CBMC.

3. Clone the cbmc-reporter sources:

    > git clone git@github.com:GaloisInc/cbmc-reporter.git

4. Install its submodule dependency:

    > cd cbmc-reporter/submodules/simple-spreadsheet-tools ; cabal install

4. Build the cbmc-reporter executable

    > cd ../../ ; cabal install

5. The executable is located in your cabal install location, usually
`$HOME\.cabal/bin/` (alternatively, you may use `cabal-dev` for a sandboxed
build).

6. Execute

    > ./PATH_TO_EXEC/cbmc-reporter --help

to confirm you've successfully installed the executable.

Finally, we assume that the GNU
[`timeout`](http://www.gnu.org/software/coreutils/manual/html_node/timeout-invocation.html)
utility exists on your system (it is standard on most Linux distributions).

Usage
=====

cbmc-reporter takes a list of C sources, include files, and proves, using CBMC,
all the assertions found in the code.  The `--help` provides an overview of
features.  The following are a few examples:

    > cbmc-reporter -f foo -f bar -s s0.c -s s1.c -I hdr.h -- --win64

Here we ask cbmc-reporter to verify the claims reachable from functions `foo`
and `bar`, found in sources `s0.c` and `s1.c`, including the header `hdr.h`.
All options to be passed directly to CBMC come last after a `--` (see `cbmc
--help` to see a list of CBMC's options, or visit
<http://www.cprover.org/cprover-manual/>).

cbmc-reporter will spawn parallel threads executing the model-checker from `foo`
and `bar` concurrently.  Furthermore, cbmc-reporter executes a depth-first
set-cover algorithm to approximate the minimal number of entry functions
required to reach all claims.  So, for example, if every claim is `bar` is
reachable by symbolically executing `foo`, then only `foo` will be used as an
entry point.

Find a minimal set of entry points can reduce the model-checking effort, but it
can lead to false-positives.  For example, consider the following code:

    int foo() {
      bar(1);
      ...
    }

    int bar(int x) {
      assert(x);
      ...
    }

If it is only analyzed from `foo`, then the assertion in `bar` is True, while it
may be false with other callers.  The flag `--all-entries` forces model-checking
from each entry point, and a claim is reported to be true only if it succeeds
from every entry point.

If no functions are explicitly provided, e.g.,

    > cbmc-reporter -s s0.c -s s1.c -I hdr.h --threads=4 -- --win64

then cbmc-reporter parses the sources (after pre-processing) to discover all
top-level functions.  Also, in this example, the `--threads=4` flag says to
spawn no more than four concurrent CBMC instances simultaneously (the problem
with too many concurrent instances is typically too much memory being consumed,
leading to thrashing).

cbmc-reporter generates tables summarizing claims proved or disproved.  For example:

File  |Function|Line|Result|Entry
:-----|:-------|:---|:-----|:----
foo\.c|bar     |21  |True  |bar
foo\.c|foo     |44  |True  |foo
foo\.c|goo     |27  |False |foo

The table can be sent to stdout (default) or written to a file (`-o
PATH_TO_FILE`).  Pandoc can be used to generate all kinds of formats from the
Markdown table (HTML, LaTeX, etc.).  Optionally, a table can be generated in an
alternate ASCII format.

CBMC is primarily intended to verify loop-free code; it provides some options to
deal with loops.  However, the user can provide the option `-t n` to
cbmc-reporter, where `n` is some number of seconds.  Any model-checking thread
that takes more than `n` seconds is killed, and a warning is reported to the
user.

Build-System Integration
------------------------

CBMC provides some build system integration natively which may be suitable for
your needs: see <http://www.cprover.org/cprover-manual/goto-cc.shtml>.

A typical Makefile rule for using cbmc-reporter is as follows:

    SRCS := $(patsubst %, --src=%, $(ALL_SRCS))
    INCS := $(patsubst %, --incl=%, $(ALL_INCS))

    .PHONY: verify
    verify: $(SRCS) $(INCS)
      cbmc-reporter \
      --format=markdown \
      --threads=3 \
      --timeout=10 \
      --no-asserts \
      --cbmc=$(CBMC_EXEC) \
      $(INCLS) \
      $(SRCS) \
      -- -D CBMC

It is sometimes useful to define assertions as macros that are defined when
using CBMC; for example:

    #ifdef CBMC

    #define REQUIRES(arg) __CPROVER_assume(arg)
    #define ASSERTS(arg)  __CPROVER_assert(arg, "")
    #define ENSURES(arg)  __CPROVER_assert(arg, "")

    #endif /* CBMC */

    #ifdef DEPLOY

    #define REQUIRES(arg)
    #define ASSERTS(arg)
    #define ENSURES(arg)

    #endif /* DEPLOY */

See CBMC's documentation on [assertions and assumptions](http://www.cprover.org/cprover-manual/modeling-assertions.shtml) for more information.

Bug Reports and Feature Requests
================================

Please file bug reports on Github: <https://github.com/GaloisInc/cbmc-reporter>.







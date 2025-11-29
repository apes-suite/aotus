Advanced Options and Tables in Universal Scripting
==================================================

![Aotus Logo](doc_pages/log.svg | width=100)

The AOTUS library provides a Fortran wrapper around the C-API of the
[Lua](http://www.lua.org) scripting language, allowing a convenient usage of Lua
scripts as configuration files in Fortran applications.

![powered by Lua](doc_pages/powered-by-lua.gif)

Please visit the
[FORD generated documentation](https://apes-suite.github.io/aotus/)
for more information on its usage and details.


How To Build
------------

We are using [Waf](http://waf.io/) as build system.

Installing `aotus` from the sources should be as simple as:

~~~~~~~~~~~{.sh}
./waf configure build install
~~~~~~~~~~~

If you want to select a specific Fortran compiler, set the environment variable
*FC*.
To select a specific C compiler, set the environment variable *CC*.

The Fortran compiler flags are set with the help of fcopts, which provide
a set of compiler flag combinations for various compilers.
They are found in the `fortran_compiler.py` file in the root directory of the
project.

By running:

~~~~~~~~~~~{.sh}
./waf --help
~~~~~~~~~~~

you get a list of available options to the waf script.
Use the `--prefix=` to set the directory into which AOTUS should be
installed.

### Generating a Makefile

If you do not want to use waf for the actual compilation of the project,
you can generate a `Makefile` with waf by running the `dump` command.
To resemble more closely the usual auto-tool steps, there is a
`configure` script to run the configuration phase of waf and the
generation of the Makefile.
This will create a Makefile in the build subdirectory, so you can
compile aotus with:

~~~~~~~~~~~{.sh}
./configure
cd build
make
~~~~~~~~~~~

You can get a list of available options by running

~~~~~~~~~~~{.sh}
./configure --help
~~~~~~~~~~~


What is Built
-------------

For your convenience the Lua library is included in version 5.4.8 (released
2025-05-21).
Its objects are completely gathered into the final *libaotus* library, so you
only need to link against this single static library to gain the
configuration features of Aotus in your Fortran application.

The default build process will also create some unit test executables and
execute them to ensure functionality of the various parts in the library.

The documentation can be built with
[FORD](https://github.com/Fortran-FOSS-Programmers/ford) by running:

~~~~~~~~~~~{.sh}
ford aot_mainpage.md
~~~~~~~~~~~

This will build a docu directory with the resulting documentation.
Note that this requires
[FORD to be installed](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Installation)
beforehand.

This documentation is also [available online](https://apes-suite.org/aotus/).

### Example

There is an example program built, called `aotus_sample`, which you will find in
the *build* directory.
It can be used with the provided *config.lua* in the *sample* directory, where
you also find the source of this small program.

Getting Started
---------------

The central module of this library is the
[aotus_module](https://apes-suite.org/aotus/module/aotus_module.html).
Its documentation along with the
[Aotus overview](https://apes-suite.org/aotus/page/index.html) are
and the [small example](https://github.com/apes-suite/aotus/tree/branch/default/sample)
are good starting points.

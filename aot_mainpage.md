Advanced Options and Tables in Universal Scripting
==================================================

The AOTUS library provides a Fortran wrapper around the C-API of the
[Lua](http://www.lua.org) scripting language, allowing a convenient usage of Lua
scripts as configuration files in Fortran applications.

**It is available for download on [Bitbucket](https://bitbucket.org/haraldkl/aotus).**

Please visit the [Wiki](https://bitbucket.org/haraldkl/aotus/wiki/Home)
for further information on its usage.

*This library is released under a simplified MIT licence, please have a look into the COPYRIGHT file for details.*

Aotus is part of the APES suite, for which there is a
[mailing list](https://listserv.uni-siegen.de/cgi-bin/mailman/listinfo/apes)
where questions can be asked.


How To Build
------------

[Waf](http://code.google.com/p/waf/) is used as build system.
Run:

~~~~~~~~~~~{.sh}
./waf configure build
~~~~~~~~~~~

to build the aotus library.
If you want to select a specific Fortran compiler, set the environment variable
*FC*.
And for a specific C compiler, set the environment variable *CC*.
The Fortran compiler flags are set with the help of fc_flags, which provide
a set of compiler flag combinations for various compilers.
They are found in the waflib/extras directory, and the waflib directory is
created upon execution of waf in a:

~~~~~~~~~~~{.sh}
.waf-*
~~~~~~~~~~~

directory, if you need to have a look at them.
By running:

~~~~~~~~~~~{.sh}
./waf --help
~~~~~~~~~~~

you get a list of available options to the waf script.


What is Built
-------------

For your convenience the Lua library is included in version 5.2.2 (released
2013-03-27).
Its objects are completely gathered into the final *libaotus* library, so it is
only necessary to link against this single static library to gain the
configuration features of aotus in your Fortran application.
Due to the compiler specific module information required by any application
using the libaotus, the suggested approach to incorporate libaotus is to include
its building in the build process of the final application. This is straight
forward if waf is used for the complete project. But also in other build
environments it should not be too hard to make use of the generated *build*
directory.
Yet if you would rather install the *libaotus.a* and the module files into a
*$PREFIX* directory, you can make use of:

~~~~~~~~~~~{.sh}
./waf install
~~~~~~~~~~~

The default build process will also create some unit test executables and
execute them to ensure functionality of the various parts in the library.

The doxygen documentation can be built by running:

~~~~~~~~~~~{.sh}
./waf doxy
~~~~~~~~~~~

This will build a html directory in the build directory with the resulting
documentation. Note, that this requires an installed doxygen.
It is also online available at
[Aotus documentation](https://geb.sts.nt.uni-siegen.de/aotus).

### Example

There is an example program built, called aotus_sample, which you will find in
the *build* directory.
It can be used with the provided *config.lua* in the *sample* directory, where
also the source of this small program is found.

Getting Started
---------------
The central module in this library is the [aotus_module](@ref aotus_module),
its documentation and the [Aotus overview](@ref aot_overview) would be good
starting points.

Related Projects
----------------

Some projects with similar goals or related information:

* [f2k3-lua](https://github.com/MaikBeckmann/f2k3-lua/tree/simple)
* [FortLua](https://github.com/adolgert/FortLua)


License
=======

Aotus is licensed under the terms of the MIT license reproduced below.
This means that Aotus is free software and can be used for both academic and
commercial purposes at absolutely no cost. You are free to do with the code
whatever you want.
The only requirement is that some credit to the authors is given by putting this
copyright notice somewhere in your project.
The MIT license is chosen for full compatibility with Lua.

For the license of the underlying Lua library have a look at
http://www.lua.org/license.html.

---
Copyright (C) 2011-2013 German Research School for Simulation Sciences GmbH,
                        Aachen and others.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

---

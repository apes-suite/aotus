=====
AOTUS
=====

The AOTUS library provides a wrapper around the C-API of the Lua scripting
language, allowing a convenient usage of Lua scripts as configuration files in
Fortran applications.
Please have look at the Wiki_ for usage informations.

This library is released under a simplified MIT licence, please have look at the
COPYRIGHT file for details.

How To Build
============

waf_ is used as build system.
Run::

./waf configure build

to build the aotus library.
If you want to select a specific Fortran compiler, set the environment variable
*FC*.
And for a specific C compiler, set the environment variable *CC*.
The Fortran compiler flags are set with the help of fc_flags, which provide
a set of compiler flag combinations for various compilers.
They are found in the waflib/extras directory, and the waflib directory is
created upon execution of waf in a::

.waf-*

directory, if you need to have a look at them.


What is Built
=============

For your convenience the lua library is included in version 5.2.1 (released
2012-06-08).
Its objects are completely gathered into the final *libaotus* library, so it is
only necessary to link against this single static library to gain the
configuration features of aotus in your Fortran application.
Due to the compiler specific module informations required by any application
using the libaotus, the suggested approach to incorporate libaotus is to include
its building in the build process of the final application. This is straight
forward if waf is used for the complete project. But also in other build
environments it should not be too hard to make use of the generated *build*
directory.
Yet if you would rather install the *libaotus.a* and the module files into a
*$PREFIX* directory, you can make use of::

./waf install

The doxygen documentation can be built by running::

./waf doxy

This will build a html directory in the build directory with the resulting
documentation. Note, that this requires an installed doxygen.

Example
-------

There is an example program built, called aotus_sample, which you will find in
the *build* directory.
It can be used with the provided *config.lua* in the *sample* directory, where
also the source of this small program is found.

Related Projects
================

Some projects with similar goals or related information:

* f2k3-lua_
* FortLua_

.. _Wiki: https://bitbucket.org/haraldkl/aotus/wiki/Home
.. _waf: http://code.google.com/p/waf/
.. _f2k3-lua: https://github.com/MaikBeckmann/f2k3-lua/tree/simple
.. _FortLua: https://github.com/adolgert/FortLua

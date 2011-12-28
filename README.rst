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

Run::

./waf configure build

to build the aotus library.
If you want to select a specific Fortran compiler, set the environment variable
*FC*.
And for a specific C compiler, set the environment variable *CC*.


What is Built
=============

For your convenience the lua library is included in version 5.2.0 (released
2011-12-16).
Its objects are completely gathered into the final *libaotus* library, so it is
only necessary to link against this single static library to gain the
configuration features of aotus in your Fortran application.

Example
-------

There is an example program built, called aotus_test, which you will find in the
*build* directory.
It can be used with the provided *config.lua* in the *test* directory, where
also the source of this small program is found.

Related Projects
================

Some projects with similar goals or related information:

* f2k3-lua_
* FortLua_

.. _Wiki: https://bitbucket.org/haraldkl/aotus/wiki/Home
.. _f2k3-lua: https://github.com/MaikBeckmann/f2k3-lua/tree/simple
.. _FortLua: https://github.com/adolgert/FortLua

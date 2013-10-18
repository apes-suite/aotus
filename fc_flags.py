#! /usr/bin/env pyhton
# encoding: utf-8
# Harald Klimach 2011

### Definition of some default fortran flags for various compilers.
### Easily select and combine any of the desired features.
### The first key-element has to specify the compiler name as set
### by the waflib/extras/fc_* tools.
### * BGXLF (XLF on BlueGene)
### * CRAY (Cray Compiler)
### * GFORTRAN (Gnu Fortran Compiler from the GCC)
### * IFORT (Intel Fortran Compiler)
### * NAG (NAG Fortran Compiler)
### * OPEN64 (Open64 Compiler)
### * PGFC (PGI Fortran Compiler)
### * SOL (Oracle Solaris Studio Compiler)
### * XLF (IBM Fortran Compiler)

fcopts = {}

# The following sets of flags are provided for all compilers:
# * warn:     activate compile-time warnings
# * w2e:      turn warnings into errors
# * standard: check for standard compliance (Fortran 2003)
# * double:   turn default reals into double precision
# * debug:    activate debug symbols and runtime checks
# * optimize: activate aggressive optimization
# * openmp:   activate openmp support
# * noomp:    deactivate openmp support
# * profile:  activate profiling (with gprof)

fcopts['GFORTRAN', 'warn'] = ['-Wall', '-Wconversion', '-Wimplicit-interface', '-Wunderflow', '-W', '-frange-check']
fcopts['GFORTRAN', 'w2e'] = ['-Werror']
fcopts['GFORTRAN', 'standard'] = ['-std=f2003']
fcopts['GFORTRAN', 'double'] = ['-fdefault-real-8']
fcopts['GFORTRAN', 'debug'] = ['-fbacktrace', '-fbounds-check', '-g']
fcopts['GFORTRAN', 'optimize'] = ['-O3', '-march=native']
fcopts['GFORTRAN', 'openmp'] = ['-fopenmp']
fcopts['GFORTRAN', 'noomp'] = []
fcopts['GFORTRAN', 'profile'] = ['-pg']
fcopts['GFORTRAN', 'fixform'] = []
fcopts['GFORTRAN', 'freeform'] = []

fcopts['IFORT', 'warn'] = '-warn all'.split()
fcopts['IFORT', 'w2e'] = '-warn stderrors'.split()
fcopts['IFORT', 'standard'] = ['-stand']
fcopts['IFORT', 'double'] = ['-r8']
fcopts['IFORT', 'debug'] = '-check all -check noarg_temp_created'.split() + ['-traceback', '-g']
fcopts['IFORT', 'optimize'] = '-xHOST -O3 -ipo -no-prec-div'.split()
fcopts['IFORT', 'openmp'] = ['-openmp']
fcopts['IFORT', 'noomp'] = []
fcopts['IFORT', 'profile'] = ['-vec-report3', '-opt-report', '-pg']
fcopts['IFORT', 'fixform'] = []
fcopts['IFORT', 'freeform'] = []

fcopts['SOL', 'warn'] = ['-w4']
fcopts['SOL', 'w2e'] = ['-errwarn=%all']
fcopts['SOL', 'standard'] = []
fcopts['SOL', 'double'] = ['-xtypemap=real:64']
fcopts['SOL', 'debug'] = ['-C', '-xcheck', '-traceback', '-g']
fcopts['SOL', 'optimize'] = ['-fast', '-xipo']
fcopts['SOL', 'openmp'] = []
fcopts['SOL', 'noomp'] = []
fcopts['SOL', 'profile'] = ['-pg']
fcopts['SOL', 'fixform'] = []
fcopts['SOL', 'freeform'] = []

fcopts['PGFC', 'warn'] = ['-Minform=inform', '-Minfo=all']
fcopts['PGFC', 'w2e'] = []
fcopts['PGFC', 'standard'] = ['-Mstandard']
fcopts['PGFC', 'double'] = ['-Mr8']
fcopts['PGFC', 'debug'] = ['-Mbounds', '-Mchkptr', '-Mlist', '-traceback', '-g']
fcopts['PGFC', 'optimize'] = ['-O4']
fcopts['PGFC', 'openmp'] = []
fcopts['PGFC', 'noomp'] = []
fcopts['PGFC', 'profile'] = ['-pg']
fcopts['PGFC', 'fixform'] = []
fcopts['PGFC', 'freeform'] = []

fcopts['BGXLF', 'warn'] = []
fcopts['BGXLF', 'w2e'] = ['-qhalt=w']
fcopts['BGXLF', 'standard'] = ['-qlanglvl=2003pure']
fcopts['BGXLF', 'double'] = ['-qautodbl=dbl4']
fcopts['BGXLF', 'debug'] = ['-C', '-g', '-qflttrap', '-qfullpath']
fcopts['BGXLF', 'optimize'] = ['-O5']
fcopts['BGXLF', 'openmp'] = []
fcopts['BGXLF', 'noomp'] = []
fcopts['BGXLF', 'profile'] = []
fcopts['BGXLF', 'fixform'] = ['-qfixed=72']
fcopts['BGXLF', 'freeform'] = ['-qfree']

fcopts['CRAY', 'warn'] = ['-m0']
fcopts['CRAY', 'w2e'] = []
fcopts['CRAY', 'standard'] = ['-e', 'n']
fcopts['CRAY', 'double'] = ['-s', 'real64']
fcopts['CRAY', 'debug'] = ['-e', 'DcI', '-R', 'bcps']
fcopts['CRAY', 'optimize'] = ['-O3']
fcopts['CRAY', 'openmp'] = ['-h', 'omp']
fcopts['CRAY', 'noomp'] = ['-h', 'noomp']
fcopts['CRAY', 'profile'] = ['-h', 'profile_generate', '-h', 'func_trace', '-h', 'keepfiles']
fcopts['CRAY', 'fixform'] = []
fcopts['CRAY', 'freeform'] = []

fcopts['NAG', 'warn'] = []
fcopts['NAG', 'w2e'] = []
fcopts['NAG', 'standard'] = ['-f2003']
fcopts['NAG', 'double'] = ['-r8']
fcopts['NAG', 'debug'] = ['-C=all', '-mtrace=all', '-nan', '-gline', '-g', '-g90']
fcopts['NAG', 'optimize'] = ['-O4']
fcopts['NAG', 'openmp'] = ['-openmp']
fcopts['NAG', 'noomp'] = []
fcopts['NAG', 'profile'] = ['-pg']
fcopts['NAG', 'fixform'] = []
fcopts['NAG', 'freeform'] = []

### End of set of Fortran flags
#########################################################################

#! /usr/bin/env python
# encoding: utf-8
# Harald Klimach 2011
import os

APPNAME = 'aotus'
VERSION = '1'

top = '.'
out = 'build'

def options(opt):
    from waflib.Tools.compiler_fc import fc_compiler
    opt.load('compiler_c')
    opt.load('compiler_fc')

def configure(conf):
    from waflib import Utils
    buildsys = Utils.unversioned_sys_platform()

    fcopts = {}
    ### Definition of some default fortran flags for various compilers.
    ### Easily select and combine any of the desired features.
    fcopts['GFORTRAN', 'warn'] = ['-Wall', '-Wconversion', '-Wimplicit-interface', '-Wunderflow', '-W', '-frange-check']
    fcopts['GFORTRAN', 'w2e'] = ['-Werror']
    fcopts['GFORTRAN', 'standard'] = ['-std=f2003']
    fcopts['GFORTRAN', 'double'] = ['-fdefault-real-8']
    fcopts['GFORTRAN', 'debug'] = ['-fbacktrace', '-fbounds-check', '-g']
    fcopts['GFORTRAN', 'optimize'] = ['-O3']
    fcopts['GFORTRAN', 'profile'] = ['-Q', '-pg']

    fcopts['IFORT', 'warn'] = '-warn all'.split() + ['-WB', '-Winline']
    fcopts['IFORT', 'w2e'] = '-warn stderrors'.split()
    fcopts['IFORT', 'standard'] = ['-stand']
    fcopts['IFORT', 'double'] = ['-r8']
    fcopts['IFORT', 'debug'] = '-check all -check noarg_temp_created'.split() + ['-traceback', '-g']
    fcopts['IFORT', 'optimize'] = '-xHOST -O3 -ipo -no-prec-div'.split()
    fcopts['IFORT', 'profile'] = ['-vec-report3', '-opt-report', '-pg']

    fcopts['SOL', 'warn'] = ['-w4']
    fcopts['SOL', 'w2e'] = ['-errwarn=%all']
    fcopts['SOL', 'standard'] = []
    fcopts['SOL', 'double'] = ['-xtypemap=real:64']
    fcopts['SOL', 'debug'] = ['-C', '-xcheck', '-traceback', '-g']
    fcopts['SOL', 'optimize'] = ['-fast', '-xipo']
    fcopts['SOL', 'profile'] = ['-pg']

    fcopts['PGFC', 'warn'] = ['-Minform=inform', '-Minfo=all']
    fcopts['PGFC', 'w2e'] = []
    fcopts['PGFC', 'standard'] = ['-Mstandard']
    fcopts['PGFC', 'double'] = ['-Mr8']
    fcopts['PGFC', 'debug'] = ['-Mbounds', '-Mchkptr', '-Mlist', '-traceback', '-g']
    fcopts['PGFC', 'optimize'] = ['-O4']
    fcopts['PGFC', 'profile'] = ['-pg']

    fcopts['BGXLF', 'warn'] = []
    fcopts['BGXLF', 'w2e'] = ['-qhalt=w']
    fcopts['BGXLF', 'standard'] = ['-qlanglvl=2003pure']
    fcopts['BGXLF', 'double'] = ['-qautodbl=dbl4']
    fcopts['BGXLF', 'debug'] = ['-C', '-g', '-qflttrap', '-qfullpath']
    fcopts['BGXLF', 'optimize'] = ['-O5']
    fcopts['BGXLF', 'profile'] = []
    ### End of set of Fortran flags
    #########################################################################

    # Load the compiler informations
    conf.load('compiler_c')
    conf.load('compiler_fc')
    conf.env['FCSTLIB_MARKER'] = ''
    conf.env['FCSHLIB_MARKER'] = ''
    conf.vars = ['FC_NAME', 'FC_VERSION', 'FCFLAGS'] # Recompilation if any of these change
    conf.check_fortran()
    conf.check_cc(function_name='mkstemp', header_name=['stdlib.h', 'unistd.h'], define_name='LUA_USE_MKSTEMP', mandatory=False)
    conf.check_cc(function_name='popen', header_name=['stdio.h'], define_name='LUA_USE_POPEN', mandatory=False)

    # Flags for the default (production) variant
    conf.env['FCFLAGS'] = fcopts[conf.env.FC_NAME, 'warn'] + fcopts[conf.env.FC_NAME, 'standard'] + fcopts[conf.env.FC_NAME, 'optimize']
    conf.env['LINKFLAGS'] = conf.env['FCFLAGS']

    # Set flags for the debugging variant
    conf.setenv('debug',conf.env)
    conf.env['FCFLAGS'] = fcopts[conf.env.FC_NAME, 'warn'] + fcopts[conf.env.FC_NAME, 'standard'] + fcopts[conf.env.FC_NAME, 'w2e'] + fcopts[conf.env.FC_NAME, 'debug']
    conf.env['LINKFLAGS'] = conf.env['FCFLAGS']

def build(bld):
    core_sources = ['external/lua-5.1.4/src/lapi.c',
                    'external/lua-5.1.4/src/lcode.c',
                    'external/lua-5.1.4/src/ldebug.c',
                    'external/lua-5.1.4/src/ldo.c',
                    'external/lua-5.1.4/src/ldump.c',
                    'external/lua-5.1.4/src/lfunc.c',
                    'external/lua-5.1.4/src/lgc.c',
                    'external/lua-5.1.4/src/llex.c',
                    'external/lua-5.1.4/src/lmem.c',
                    'external/lua-5.1.4/src/lobject.c',
                    'external/lua-5.1.4/src/lopcodes.c',
                    'external/lua-5.1.4/src/lparser.c',
                    'external/lua-5.1.4/src/lstate.c',
                    'external/lua-5.1.4/src/lstring.c',
                    'external/lua-5.1.4/src/ltable.c',
                    'external/lua-5.1.4/src/ltm.c',
                    'external/lua-5.1.4/src/lundump.c',
                    'external/lua-5.1.4/src/lvm.c',
                    'external/lua-5.1.4/src/lzio.c']
    lib_sources = ['external/lua-5.1.4/src/lauxlib.c',
                   'external/lua-5.1.4/src/lbaselib.c',
                   'external/lua-5.1.4/src/ldblib.c',
                   'external/lua-5.1.4/src/liolib.c',
                   'external/lua-5.1.4/src/lmathlib.c',
                   'external/lua-5.1.4/src/loslib.c',
                   'external/lua-5.1.4/src/ltablib.c',
                   'external/lua-5.1.4/src/lstrlib.c',
                   'external/lua-5.1.4/src/loadlib.c',
                   'external/lua-5.1.4/src/linit.c']
    lua_sources = ['external/lua-5.1.4/src/lua.c']
    luac_sources = ['external/lua-5.1.4/src/luac.c',
                    'external/lua-5.1.4/src/print.c']

    flu_sources = ['LuaFortran/lua_fif.f90',
                   'LuaFortran/lua_parameters.f90',
                   'LuaFortran/flu_binding.f90']

    aotus_sources = ['source/aotus_module.f90',
                     'source/aot_fun_module.f90',
                     'source/aot_kinds_module.f90',
                     'source/aot_table_module.f90']

    bld(
        features = 'c',
        source = core_sources + lib_sources,
        defines = ['LUA_ANSI'],
        target = 'luaobjs')

    bld(
        features = 'c cstlib',
        defines = ['LUA_ANSI'],
        use = 'luaobjs',
        name = 'lualib',
        target = 'lua')

    bld(
        features = 'fc',
        source = flu_sources,
        target = 'fluobjs')

    bld(
        features = 'fc fcstlib',
        use = ['luaobjs', 'fluobjs'],
        target = 'flu')

    bld(
        features = 'fc fcstlib',
        source = aotus_sources,
        use = ['luaobjs', 'fluobjs'],
        target = 'aotus')

    bld(
        features = 'fc fcprogram',
        source = ['test/aotus_test.f90'],
        use = 'aotus',
        target = 'aotus_test')

    bld(
        features = 'fc fcprogram',
        source = ['LuaFortran/examples/test.f90'],
        use = 'flu',
        target = 'sample')

#    bld(
#        features = 'c cprogram',
#        use = 'lualib',
#        source = lua_sources,
#        defines = ['LUA_ANSI'],
#        stlib = bld.env['STLIBS'],
#        target = 'lua')


from waflib.Build import BuildContext
class debug(BuildContext):
    "Build a debug executable"
    cmd = 'debug'
    variant = 'debug'

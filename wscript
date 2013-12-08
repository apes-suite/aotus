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
    opt.load('compiler_fc')
    opt.load('compiler_c')
    opt.load('waf_unit_test')
    opt.load('utest_results')

def configure(conf):
    from waflib import Logs
    # The fcopts provide some sane flag combinations
    # for different variants in the various compilers.
    # They are found in the fc_flags.py in the same
    # directory as the wscript file.
    from fc_flags import fcopts
    # includes options for:
    # * 'warn': activate compile time warnings
    # * 'w2e': turn warnings into errors
    # * 'standard': check for standard compliance
    # * 'debug': activate debugging facilities
    # * 'optimize': turn optimization on
    # * 'profile': activate profiling facilities
    # * 'double': promote default reals to double precision

    # Load the compiler informations
    conf.load('compiler_fc')
    conf.load('compiler_c')
    conf.load('waf_unit_test')

    conf.env.stash()
    try:
        conf.load('doxygen')
    except conf.errors.ConfigurationError:
        Logs.debug('doxygen: %r' % conf.errors.ConfigurationError)
        conf.env.revert()

    conf.env['FCSTLIB_MARKER'] = ''
    conf.env['FCSHLIB_MARKER'] = ''

    conf.check_fortran()
    subconf(conf)

    # Flags for the default (production) variant
    conf.env['FCFLAGS'] = ( fcopts[conf.env.FC_NAME, 'optimize']
                          + fcopts[conf.env.FC_NAME, 'warn'] )
    conf.env['LINKFLAGS_fcprogram'] = conf.env['FCFLAGS']

    # Set flags for the debugging variant
    # DEBUG Variant
    conf.setenv('debug',conf.env)
    conf.env['FCFLAGS'] = ( fcopts[conf.env.FC_NAME, 'standard']
                          + fcopts[conf.env.FC_NAME, 'warn']
                          + fcopts[conf.env.FC_NAME, 'w2e']
                          + fcopts[conf.env.FC_NAME, 'debug'] )
    conf.env['LINKFLAGS_fcprogram'] = conf.env['FCFLAGS']

def subconf(conf):
    """
    Configure parts, which are relevant, even when called
    from parent wscripts.
    Useful to restrict parent recursions to just this part
    of the configuration.
    """
    # Do not change the DEFINES themselves, use the lib_store instead
    tmpDEF = conf.env.DEFINES
    conf.check_cc(function_name='mkstemp',
                  header_name=['stdlib.h', 'unistd.h'],
                  defines=['LUA_USE_MKSTEMP=1'],
                  uselib_store='MKSTEMP', mandatory=False)
    conf.check_cc(function_name='popen',
                  header_name=['stdio.h'],
                  defines=['LUA_USE_POPEN=1'],
                  uselib_store='POPEN', mandatory=False)
    conf.check_cc(lib='m', uselib_store='MATH')

    conf.check_fc(fragment = '''
       program check_iso_c
         use, intrinsic :: iso_c_binding
         implicit none
         write(*,*) c_int
       end program check_iso_c''',
                  msg = "Checking for ISO_C_Binding support",
                  mandatory = 'true')

    conf.in_msg = 1
    conf.check_fc(fragment = '''
       program checkquad
         implicit none
         integer, parameter :: quad_k = selected_real_kind(33)
         real(kind=quad_k) :: a_quad_real
         write(*,*) quad_k
       end program checkquad''',
                  mandatory=False, define_name='quadruple',
                  execute = True, define_ret = True)
    conf.in_msg = 0

    conf.env['quad_support'] = conf.is_defined('quadruple')
    if conf.env['quad_support']:
       conf.env['quad_k'] = int(conf.get_define('quadruple').replace('"', '').strip())
       if conf.env['quad_k'] < 1:
          conf.env['quad_support'] = False
    if conf.env['quad_support']:
       conf.msg('Checking for Quadruple Precision', 'yes', color='GREEN')
    else:
       conf.msg('Checking for Quadruple Precision', 'NO', color='RED')

    conf.in_msg = 1
    conf.check_fc(fragment = '''
       program checkxdble
         implicit none
         integer, parameter :: xdble_k = selected_real_kind(18)
         real(kind=xdble_k) :: a_xdble_real
         write(*,*) xdble_k
       end program checkxdble''',
                  mandatory=False, define_name='extdouble',
                  execute = True, define_ret = True)
    conf.in_msg = 0

    conf.env['xdble_support'] = False
    if conf.is_defined('extdouble'):
       conf.env['xdble_k'] = int(conf.get_define('extdouble').replace('"', '').strip())
       if conf.env['xdble_k'] > 0 and conf.env['xdble_k'] != conf.env['quad_k']:
          conf.env['xdble_support'] = True

    if conf.env['xdble_support']:
       conf.msg('Checking for Extended Double Precision', 'yes', color='GREEN')
    else:
       conf.msg('Checking for Extended Double Precision', 'NO', color='RED')

    # Cleanup the DEFINES again
    conf.env.DEFINES = tmpDEF


def build(bld):
    core_sources = ['external/lua-5.2.3/src/lapi.c',
                    'external/lua-5.2.3/src/lcode.c',
                    'external/lua-5.2.3/src/lctype.c',
                    'external/lua-5.2.3/src/ldebug.c',
                    'external/lua-5.2.3/src/ldo.c',
                    'external/lua-5.2.3/src/ldump.c',
                    'external/lua-5.2.3/src/lfunc.c',
                    'external/lua-5.2.3/src/lgc.c',
                    'external/lua-5.2.3/src/llex.c',
                    'external/lua-5.2.3/src/lmem.c',
                    'external/lua-5.2.3/src/lobject.c',
                    'external/lua-5.2.3/src/lopcodes.c',
                    'external/lua-5.2.3/src/lparser.c',
                    'external/lua-5.2.3/src/lstate.c',
                    'external/lua-5.2.3/src/lstring.c',
                    'external/lua-5.2.3/src/ltable.c',
                    'external/lua-5.2.3/src/ltm.c',
                    'external/lua-5.2.3/src/lundump.c',
                    'external/lua-5.2.3/src/lvm.c',
                    'external/lua-5.2.3/src/lzio.c']
    lib_sources = ['external/lua-5.2.3/src/lauxlib.c',
                   'external/lua-5.2.3/src/lbaselib.c',
                   'external/lua-5.2.3/src/lbitlib.c',
                   'external/lua-5.2.3/src/lcorolib.c',
                   'external/lua-5.2.3/src/ldblib.c',
                   'external/lua-5.2.3/src/liolib.c',
                   'external/lua-5.2.3/src/lmathlib.c',
                   'external/lua-5.2.3/src/loslib.c',
                   'external/lua-5.2.3/src/ltablib.c',
                   'external/lua-5.2.3/src/lstrlib.c',
                   'external/lua-5.2.3/src/loadlib.c',
                   'external/lua-5.2.3/src/linit.c']
    lua_sources = ['external/lua-5.2.3/src/lua.c']
    luac_sources = ['external/lua-5.2.3/src/luac.c']

    wrap_sources = ['LuaFortran/wrap_lua_dump.c']

    flu_sources = ['LuaFortran/lua_fif.f90',
                   'LuaFortran/dump_lua_fif_module.f90',
                   'LuaFortran/lua_parameters.f90',
                   'LuaFortran/flu_binding.f90']

    aotus_sources = ['source/aotus_module.f90',
                     'source/aot_err_module.f90',
                     'source/aot_fun_module.f90',
                     'source/aot_fun_declaration_module.f90',
                     'source/aot_kinds_module.f90',
                     'source/aot_table_module.f90',
                     'source/aot_table_ops_module.f90',
                     'source/aot_top_module.f90',
		     'source/aot_out_module.f90',
		     'source/aot_out_general_module.f90',
                     'source/aot_path_module.f90',
                     'source/aot_vector_module.f90']

    if bld.env['quad_support']:
        aotus_sources += ['source/quadruple/aot_quadruple_fun_module.f90']
        aotus_sources += ['source/quadruple/aot_quadruple_table_module.f90']
        aotus_sources += ['source/quadruple/aot_quadruple_top_module.f90']
        aotus_sources += ['source/quadruple/aot_quadruple_out_module.f90']
        aotus_sources += ['source/quadruple/aot_quadruple_vector_module.f90']
    else:
        aotus_sources += ['source/quadruple/dummy_quadruple_fun_module.f90']
        aotus_sources += ['source/quadruple/dummy_quadruple_table_module.f90']
        aotus_sources += ['source/quadruple/dummy_quadruple_top_module.f90']
        aotus_sources += ['source/quadruple/dummy_quadruple_out_module.f90']
        aotus_sources += ['source/quadruple/dummy_quadruple_vector_module.f90']

    if bld.env['xdble_support']:
        aotus_sources += ['source/extdouble/aot_extdouble_fun_module.f90']
        aotus_sources += ['source/extdouble/aot_extdouble_table_module.f90']
        aotus_sources += ['source/extdouble/aot_extdouble_top_module.f90']
        aotus_sources += ['source/extdouble/aot_extdouble_out_module.f90']
        aotus_sources += ['source/extdouble/aot_extdouble_vector_module.f90']
    else:
        aotus_sources += ['source/extdouble/dummy_extdouble_fun_module.f90']
        aotus_sources += ['source/extdouble/dummy_extdouble_table_module.f90']
        aotus_sources += ['source/extdouble/dummy_extdouble_top_module.f90']
        aotus_sources += ['source/extdouble/dummy_extdouble_out_module.f90']
        aotus_sources += ['source/extdouble/dummy_extdouble_vector_module.f90']

    bld(
        features = 'c',
        source = core_sources + lib_sources,
        defines = ['LUA_ANSI'],
        use = ['MKSTEMP', 'POPEN'],
        target = 'luaobjs')

    bld(
        features = 'c cstlib',
        defines = ['LUA_ANSI'],
        use = 'luaobjs',
        target = 'lualib')

    bld(
        features = 'c',
        source = wrap_sources,
        use = 'luaobjs',
        includes = 'external/lua-5.2.3/src',
        target = 'wrapobjs')

    bld(
        features = 'fc',
        source = flu_sources,
        target = 'fluobjs')

    bld(
        features = 'fc fcstlib',
        use = ['luaobjs', 'fluobjs', 'wrapobjs'],
        target = 'flu')

    bld(
        features = 'fc fcstlib',
        source = aotus_sources,
        use = ['luaobjs', 'fluobjs', 'wrapobjs'],
        target = 'aotus')

    bld(
        features = 'fc fcprogram',
        source = ['sample/aotus_sample.f90'],
        use = 'aotus',
        target = 'aotus_sample')

    bld(
        features = 'fc fcprogram',
        source = ['LuaFortran/examples/test.f90'],
        use = 'flu',
        target = 'flu_sample')

    from waflib.extras import utest_results
    utest_results.utests(bld, 'aotus')
    if bld.env['quad_support']:
        utest_results.utests(bld, use = 'aotus', path = 'utests/quadruple')
    bld.add_post_fun(utest_results.summary)

    if bld.cmd == 'doxy':
        bld(features = 'doxygen',
            doxyfile = 'Doxyfile')

    # install_files actually only done, if in install mode
    # however the if here, protects the ant_glob in the build directory
    # to be run if not in the install phase...
    if bld.cmd == 'install':
        bld.install_files('${PREFIX}/include',
                          bld.path.get_bld().ant_glob('*.mod'))
        bld.install_files('${PREFIX}/lib', 'libaotus.a')

## Building the lua interpreter, usually not needed.
    bld(
        features = 'c cprogram',
        use = ['lualib', 'MATH'],
        source = lua_sources,
        defines = ['LUA_ANSI'],
        stlib = bld.env['STLIBS'],
        target = 'lua')


from waflib.Build import BuildContext
class debug(BuildContext):
    "Build a debug executable"
    cmd = 'debug'
    variant = 'debug'

class doxy(BuildContext):
    "Doxygen documentation"
    cmd = 'doxy'

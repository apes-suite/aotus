# Checks for supported Fortran language features
# 
# Results are stored in the context environment with a fortsupp_ prefix.
#
def options(opt):
    flopts = opt.add_option_group('Fortran Language specific settings')
    flopts.add_option('--quad_kind', action='store',
                      help='Kind for quadruple precision reals')
    flopts.add_option('--xdble_kind', action='store',
                      help='Kind for extended double precision reals')

def supports_iso_c(conf, mandatory=True):
  '''
     Check whether the Fotran compiler from the context in conf supports the
     Fortran 2003 ISO_C_Binding.
     Set mandatory to False if the config should not abort upon missing
     ISO_C_Binding support.
     Result stored in conf.env.fortsupp_iso_c
  '''

  fcenv = conf.env
  fcenv.detach()

  conf.check_fc( fragment = '''
program check_iso_c
  use, intrinsic :: iso_c_binding
  implicit none
  write(*,*) c_int
end program check_iso_c''',
                 msg = "Checking for ISO_C_Binding support",
                 define_name = 'iso_c_binding',
                 mandatory = mandatory)

  fcenv['fortsupp_iso_c'] = conf.is_defined('iso_c_binding')
  conf.env = fcenv

real_kind_stub = '''
program check_r_kind
  implicit none
  integer, parameter :: real_k = selected_real_kind({0})
  real(kind=real_k) :: a_real
  write(*,*) real_k
end program check_r_kind
'''

def supports_quad_kind(conf, mandatory=True):
  '''
     Check whether the Fotran compiler from the context in conf supports the
     quadruple precision real numbers.
     Set mandatory to False if the config should not abort upon missing
     quadruple support.
     Resulting kind value for quadruple reals is stored in
     conf.env.fortsupp_quad_kind (negative if not available)
  '''

  conf.start_msg('Checking for Quadruple precision')

  fcenv = conf.env
  fcenv.detach()

  if conf.options.quad_kind:
    conf.check_fc( fragment = real_kind_stub.format(conf.options.quad_kind),
                   mandatory = mandatory,
                   define_name = 'quadruple')
    if conf.is_defined('quadruple'):
      fcenv['fortsupp_quad_kind'] = conf.options.quad_kind
    else:
      fcenv['fortsupp_quad_kind'] = -1
  else:
    conf.check_fc( fragment = real_kind_stub.format(33),
                   mandatory = mandatory,
                   define_name = 'quadruple',
                   execute = True, define_ret = True)

    if conf.is_defined('quadruple'):
      fcenv['fortsupp_quad_kind'] = int( conf.get_define('quadruple')
                                             .replace('"', '').strip() )
    else:
      fcenv['fortsupp_quad_kind'] = -1

  conf.env = fcenv

  if conf.env['fortsupp_quad_kind'] > 0:
     conf.end_msg('yes', color='GREEN')
  else:
     conf.end_msg('NO', color='RED')


def supports_xdble_kind(conf, mandatory=True):
  '''
     Check whether the Fotran compiler from the context in conf supports the
     extended double precision real numbers.
     Set mandatory to False if the config should not abort upon missing
     extended double support.
     Resulting kind value for extended double precision reals is stored in
     conf.env.fortsupp_xdble_kind (negative if not available)
  '''

  # Make sure to look for quadruple precision first:
  if not hasattr(conf.env, 'fortsupp_quad_kind'):
    supports_quadruple_kind(conf)

  conf.start_msg('Checking for Extended double precision')

  fcenv = conf.env
  fcenv.detach()

  if conf.options.xdble_kind:
    conf.check_fc( fragment = real_kind_stub.format(conf.options.xdble_kind),
                   mandatory = mandatory,
                   define_name = 'xdble')
    if conf.is_defined('xdble'):
      fcenv['fortsupp_xdble_kind'] = conf.options.xdble_kind
    else:
      fcenv['fortsupp_xdble_kind'] = -1
  else:

    conf.check_fc( fragment = real_kind_stub.format(18),
                   mandatory = mandatory,
                   define_name = 'xdble',
                   execute = True, define_ret = True)

    if conf.is_defined('xdble'):
      fcenv['fortsupp_xdble_kind'] = int( conf.get_define('xdble')
                                              .replace('"', '').strip() )
    else:
      fcenv['fortsupp_xdble_kind'] = -1

  # If the found kind is actually the quadruple precision, do not set the
  # extended double precision kind.
  if fcenv['fortsupp_xdble_kind'] == fcenv['fortsupp_quad_kind']:
    fcenv['fortsupp_xdble_kind'] = -1
  conf.env = fcenv

  if conf.env['fortsupp_xdble_kind'] > 0:
     conf.end_msg('yes', color='GREEN')
  else:
     conf.end_msg('NO', color='RED')


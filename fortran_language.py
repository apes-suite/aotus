# Checks for supported Fortran language features
# 
# Results are stored in the context environment with a fortsupp_ prefix.
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


def supports_quad_kind(conf, mandatory=True):
  '''
     Check whether the Fotran compiler from the context in conf supports the
     quadruple precision real numbers.
     Set mandatory to False if the config should not abort upon missing
     quadruple support.
     Resulting kind value for quadruple reals is stored in
     conf.env.fortsupp_quad_kind (negative if not available)
  '''

  fcenv = conf.env
  fcenv.detach()

  conf.start_msg('Checking for Quadruple precision')
  conf.check_fc( fragment = '''
program checkquad
  implicit none
  integer, parameter :: quad_k = selected_real_kind(33)
  real(kind=quad_k) :: a_quad_real
  write(*,*) quad_k
end program checkquad''',
                 mandatory = mandatory,
                 define_name = 'quadruple',
                 execute = True, define_ret = True)

  if conf.is_defined('quadruple'):
    fcenv['fortsupp_quad_kind'] = int( conf.get_define('quadruple')
                                           .replace('"', '').strip() )
  else:
    fcenv['fortsupp_quad_kind'] = -1

  if fcenv['fortsupp_quad_kind'] > 0:
     conf.end_msg('yes', color='GREEN')
  else:
     conf.end_msg('NO', color='RED')

  conf.env = fcenv

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

  fcenv = conf.env
  fcenv.detach()

  conf.start_msg('Checking for Extended double precision')
  conf.check_fc( fragment = '''
program checkxdble
  implicit none
  integer, parameter :: xdble_k = selected_real_kind(18)
  real(kind=xdble_k) :: a_xdble_real
  write(*,*) xdble_k
end program checkxdble''',
                 mandatory = mandatory,
                 define_name = 'xdble',
                 execute = True, define_ret = True)

  if conf.is_defined('xdble'):
    fcenv['fortsupp_xdble_kind'] = int( conf.get_define('xdble')
                                            .replace('"', '').strip() )
    # If the found kind is actually the quadruple precision, do not set the
    # extended double precision kind.
    if fcenv['fortsupp_xdble_kind'] == fcenv['fortsupp_quad_kind']:
      fcenv['fortsupp_xdble_kind'] = -1
  else:
    fcenv['fortsupp_xdble_kind'] = -1

  if fcenv['fortsupp_xdble_kind'] > 0:
     conf.end_msg('yes', color='GREEN')
  else:
     conf.end_msg('NO', color='RED')

  conf.env = fcenv

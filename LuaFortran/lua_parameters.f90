module lua_parameters
  use, intrinsic :: iso_c_binding

  implicit none

  ! System dependent, might need to be adapted:
  integer, parameter :: lua_int = c_long
  integer, parameter :: lua_num = c_double

  ! lua constants
  integer(kind=c_int), parameter :: LUA_GLOBALSINDEX = -10002
  integer(kind=c_int), parameter :: LUA_TBOOLEAN = 1
  integer(kind=c_int), parameter :: LUA_TTABLE = 5

end module lua_parameters

module lua_fif
  use, intrinsic :: iso_c_binding
  ! This module provides a direct translation of the
  ! lua 5.1.4
  ! C-Interfaces to Fortran 2003 interfaces using the
  ! ISO_C_BINDING facilities.

  implicit none

  ! lua constants
  integer(kind=c_int), parameter :: LUA_GLOBALSINDEX = -10002



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! lua API interfaces
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  interface

    subroutine lua_getfield(L, index, k) bind(c, name="lua_getfield")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      character(kind=c_char), dimension(*) :: k
    end subroutine lua_getfield

    subroutine lua_close(L) bind(c, name="lua_close")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
    end subroutine lua_close

    function lua_isnumber(L, index) bind(c, name="lua_isnumber")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: lua_isnumber
    end function lua_isnumber

    function lua_pcall(L, nargs, nresults, errfunc) bind(c, name="lua_pcall")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: nargs
      integer(kind=c_int), value :: nresults
      integer(kind=c_int), value :: errfunc
      integer(kind=c_int) :: lua_pcall
    end function lua_pcall

    function lua_tolstring(L, index, len) bind(c, name="lua_tolstring")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_size_t) :: len
      type(c_ptr) :: lua_tolstring
    end function lua_tolstring

    function lua_tonumber(L, index) bind(c, name="lua_tonumber")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      real(kind=c_double) :: lua_tonumber
    end function lua_tonumber

  end interface
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! lua auxiliary library interfaces
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! lua auxiliary library
  interface

    subroutine luaL_openlibs(L) bind(c, name="luaL_openlibs")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
    end subroutine luaL_openlibs

    function luaL_newstate() bind(c, name="luaL_newstate")
      use, intrinsic :: iso_c_binding
      type(c_ptr) :: luaL_newstate
    end function luaL_newstate

    function luaL_loadfile(L, filename) bind(c, name="luaL_loadfile")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: filename
      integer(kind=c_int) :: luaL_loadfile
    end function luaL_loadfile

  end interface

end module lua_fif

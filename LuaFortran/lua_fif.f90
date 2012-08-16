!> This module provides a direct translation of some
!! Lua 5.2.1
!! C-Interfaces to Fortran 2003 interfaces using the
!! ISO_C_BINDING facilities.
module lua_fif
  use, intrinsic :: iso_c_binding
  use lua_parameters

  implicit none

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! lua API interfaces
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  interface

    subroutine lua_close(L) bind(c, name="lua_close")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
    end subroutine lua_close

    subroutine lua_createtable(L, narr, nrec) bind(c, name="lua_createtable")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: narr
      integer(kind=c_int), value :: nrec
    end subroutine lua_createtable

    subroutine lua_getglobal(L, k) bind(c, name="lua_getglobal")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: k
    end subroutine lua_getglobal

    subroutine lua_getfield(L, index, k) bind(c, name="lua_getfield")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      character(kind=c_char), dimension(*) :: k
    end subroutine lua_getfield

    subroutine lua_gettable(L, index) bind(c, name="lua_gettable")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
    end subroutine lua_gettable

    function lua_gettop(L) bind(c, name="lua_gettop")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int) :: lua_gettop
    end function lua_gettop

    function lua_isNumber(L, index) bind(c, name="lua_isnumber")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: lua_isnumber
    end function lua_isnumber

    function lua_next(L, index) bind(c, name="lua_next")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: lua_next
    end function lua_next

    function lua_pcallk(L, nargs, nresults, errfunc, ctx, k) bind(c, name="lua_pcallk")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: nargs
      integer(kind=c_int), value :: nresults
      integer(kind=c_int), value :: errfunc
      integer(kind=c_int), value :: ctx
      type(c_ptr), value :: k
      integer(kind=c_int) :: lua_pcallk
    end function lua_pcallk

    subroutine lua_pushinteger(L, n) bind(c, name="lua_pushinteger")
      use, intrinsic :: iso_c_binding
      use lua_parameters, only: lua_int
      type(c_ptr), value :: L
      integer(kind=lua_int), value :: n
    end subroutine lua_pushinteger

    subroutine lua_pushnil(L) bind(c, name="lua_pushnil")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
    end subroutine lua_pushnil

    subroutine lua_pushnumber(L, n) bind(c, name="lua_pushnumber")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      real(kind=c_double), value :: n
    end subroutine lua_pushnumber

    subroutine lua_pushvalue(L, index) bind(c, name="lua_pushvalue")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
    end subroutine lua_pushvalue

    subroutine lua_setfield(L, index, k) bind(c, name="lua_setfield")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      character(kind=c_char), dimension(*) :: k
    end subroutine lua_setfield

    subroutine lua_setglobal(L, k) bind(c, name="lua_setglobal")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: k
    end subroutine lua_setglobal

    subroutine lua_settable(L, index) bind(c, name="lua_settable")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
    end subroutine lua_settable

    subroutine lua_settop(L, index) bind(c, name="lua_settop")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
    end subroutine lua_settop

    function lua_tolstring(L, index, len) bind(c, name="lua_tolstring")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_size_t) :: len
      type(c_ptr) :: lua_tolstring
    end function lua_tolstring

    function lua_tonumberx(L, index, isnum) bind(c, name="lua_tonumberx")
      use, intrinsic :: iso_c_binding
      use lua_parameters, only: lua_num
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: isnum
      real(kind=lua_num) :: lua_tonumberx
    end function lua_tonumberx

    function lua_toboolean(L, index) bind(c, name="lua_toboolean")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: lua_toboolean
    end function lua_toboolean

    function lua_type(L, index) bind(c, name="lua_type")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: lua_type
    end function lua_type

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

    function luaL_loadfilex(L, filename, mode) bind(c, name="luaL_loadfilex")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: filename
      character(kind=c_char), dimension(*) :: mode
      integer(kind=c_int) :: luaL_loadfilex
    end function luaL_loadfilex

    function luaL_loadstring(L, string) bind(c, name="luaL_loadstring")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: string
      integer(kind=c_int) :: luaL_loadstring
    end function luaL_loadstring

  end interface

end module lua_fif

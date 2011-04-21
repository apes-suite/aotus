module flu_binding
  use, intrinsic :: iso_c_binding
  use lua_fif

  implicit none

  private

  type :: flu_State
    private
    type(c_ptr) :: state
  end type flu_State

  public :: flu_State

  public :: flu_close
  public :: flu_getfield, flu_getglobal
  public :: flu_isnumber
  public :: flu_pcall
  public :: flu_tolstring, flu_tonumber

  public :: fluL_loadfile, fluL_newstate, fluL_openlibs

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Wrapper routines for the lua API
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine flu_close(L)
    type(flu_State) :: L

    call lua_close(L%state)
  end subroutine flu_close


  subroutine flu_getfield(L, index, k)
    type(flu_State)  :: L
    integer          :: index
    character(len=*) :: k

    integer(kind=c_int) :: c_index
    character(len=len_trim(k)+1) :: c_k

    c_k = trim(k) // c_null_char
    c_index = index
    call lua_getfield(L%state, c_index, c_k)
  end subroutine flu_getfield


  subroutine flu_getglobal(L, k)
    type(flu_State)  :: L
    character(len=*) :: k

    character(len=len_trim(k)+1) :: c_k

    c_k = trim(k) // c_null_char
    call lua_getfield(L%state, LUA_GLOBALSINDEX, c_k)
  end subroutine flu_getglobal


  function flu_isnumber(L, index) result(is_number)
    type(flu_State) :: L
    integer         :: index
    logical         :: is_number

    integer(kind=c_int) :: c_index

    c_index = index
    is_number = (lua_isnumber(L%state, c_index).eq.1)
  end function flu_isnumber


  function flu_pcall(L, nargs, nresults, errfunc) result(errcode)
    type(flu_State) :: L
    integer :: nargs
    integer :: nresults
    integer :: errfunc
    integer :: errcode

    integer(kind=c_int) :: c_nargs
    integer(kind=c_int) :: c_nresults
    integer(kind=c_int) :: c_errfunc
    integer(kind=c_int) :: c_errcode

    c_nargs = nargs
    c_nresults = nresults
    c_errfunc = errfunc

    c_errcode = lua_pcall(L%state, c_nargs, c_nresults, c_errfunc)
    errcode = c_errcode
  end function flu_pcall


  function flu_tolstring(L, index, len) result(string)
    type(flu_State) :: L
    integer :: index
    integer :: len
    character,pointer,dimension(:) :: string

    integer :: string_shape(1)
    integer(kind=c_int) :: c_index
    integer(kind=c_size_t) :: c_len
    type(c_ptr) :: c_string

    c_index = index
    c_string = lua_tolstring(L%state, c_index, c_len)
    len = int(c_len,kind=kind(len))
    string_shape(1) = len
    call c_f_pointer(c_string, string, string_shape)
  end function flu_tolstring


  function flu_tonumber(L, index) result(number)
    type(flu_State) :: L
    integer :: index
    real :: number

    integer(kind=c_int) :: c_index
    real(kind=c_double) :: c_number

    c_index = index
    c_number = lua_tonumber(L%state, c_index)
    number = real(c_number, kind=kind(number))
  end function flu_tonumber


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Wrapper routines for the auxiliary library 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function fluL_loadfile(L, filename) result(errcode)
    type(flu_State) :: L
    character(len=*) :: filename
    integer :: errcode

    character(len=len_trim(filename)+1) :: c_filename
    integer(kind=c_int) :: c_errcode

    c_filename = trim(filename) // c_null_char
    c_errcode = luaL_loadfile(L%state, c_filename)
    errcode = c_errcode
  end function fluL_loadfile


  function fluL_newstate() result(new_state)
    type(flu_State) :: new_state

    new_state%state = luaL_newstate()
  end function fluL_newstate


  subroutine fluL_openlibs(L)
    type(flu_State) :: L
    
    call luaL_openlibs(L%state)
  end subroutine fluL_openlibs
    
end module flu_binding


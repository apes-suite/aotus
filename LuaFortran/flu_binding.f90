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
  public :: flu_getField, flu_getGlobal, flu_getTable, flu_getTop
  public :: flu_isnumber, flu_isNoneOrNil, flu_isTable, flu_isBoolean
  public :: flu_pcall
  public :: flu_next
  public :: flu_tolstring, flu_tonumber, flu_toboolean
  public :: flu_pop
  public :: flu_pushinteger, flu_pushnil

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


  subroutine flu_gettable(L, index)
    type(flu_State) :: L
    integer :: index

    integer(kind=c_int) :: c_index

    c_index = index
    call lua_gettable(L%state, c_index)
  end subroutine flu_gettable


  function flu_gettop(L) result(stacktop)
    type(flu_state) :: L
    integer :: stacktop

    stacktop = int(lua_gettop(L%state), kind=kind(stacktop))
  end function flu_gettop


  function flu_isBoolean(L, index) result(is_boolean)
    type(flu_State) :: L
    integer         :: index
    logical         :: is_boolean

    integer(kind=c_int) :: c_index

    c_index = int(index, kind = c_int)
    !! Only defined as a Macro, using lua_type:
    is_boolean = (lua_type(L%state, c_index) == LUA_TBOOLEAN)
  end function flu_isBoolean


  function flu_isnumber(L, index) result(is_number)
    type(flu_State) :: L
    integer         :: index
    logical         :: is_number

    integer(kind=c_int) :: c_index

    c_index = int(index, kind = c_int)
    is_number = (lua_isnumber(L%state, c_index) .eq. 1)
  end function flu_isnumber


  function flu_isTable(L, index) result(is_Table)
    type(flu_State) :: L
    integer         :: index
    logical         :: is_Table

    integer(kind=c_int) :: c_index

    c_index = int(index, kind = c_int)
    !! Only defined as a Macro, using lua_type:
    is_Table = (lua_type(L%state, c_index) == LUA_TTABLE)
  end function flu_isTable


  function flu_isNoneOrNil(L, index) result(is_NoneOrNil)
    type(flu_State) :: L
    integer         :: index
    logical         :: is_NoneOrNil

    integer(kind=c_int) :: c_index

    c_index = int(index, kind = c_int)
    !! Only defined as a Macro, using lua_type:
    is_NoneOrNil = (lua_Type(L%state, c_index) <= 0)
  end function flu_isNoneOrNil


  function flu_next(L, index) result(exists)
    type(flu_State) :: L
    integer, intent(in) :: index
    logical :: exists

    integer(kind=c_int) :: retCode

    retCode = lua_next(L%state, index)
    exists = (retCode /= 0)
  end function flu_next


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


  subroutine flu_pop(L, n)
    type(flu_State) :: L
    integer, optional, intent(in) :: n

    integer(kind=c_int) :: n_c

    n_c = -2
    if (present(n)) n_c = -n-1
    call lua_settop(L%state, n_c)
  end subroutine flu_pop


  subroutine flu_pushinteger(L, n)
    type(flu_State) :: L
    integer :: n

    integer(kind=lua_int) :: n_c

    n_c = n
    call lua_pushinteger(L%state, n_c)
  end subroutine flu_pushinteger


  subroutine flu_pushnil(L)
    type(flu_State) :: L

    call lua_pushnil(L%state)
  end subroutine flu_pushnil


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


  function flu_toBoolean(L, index) result(bool)
    type(flu_State) :: L
    integer :: index
    logical :: bool

    integer(kind=c_int) :: c_index

    c_index = index
    bool = (lua_toBoolean(L%state, c_index) == 1)
  end function flu_toBoolean


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


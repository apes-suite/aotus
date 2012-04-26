!> This module provides high level Fortran interfaces
!! to retrieve values from a Lua script.
module aotus_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_top_module, only: aot_top_get_val

  implicit none

  private

  public :: aot_get_val, open_config, close_config

  !> Get a global configuration value from the script.
  !!
  !! This provides a convenient direct access to
  !! global variables from the Lua script.
  interface aot_get_val
    module procedure get_config_real
    module procedure get_config_double
    module procedure get_config_integer
    module procedure get_config_long
    module procedure get_config_string
    module procedure get_config_logical
  end interface

contains

  subroutine open_config(L, filename)
    type(flu_State) :: L
    character(len=*), intent(in) :: filename
    integer :: str_len

    L = fluL_newstate()

    if (fluL_loadfile(L, filename) .ne. 0) then
      write(*,*) "cannot load configuration file: ", &
        &        flu_tolstring(L, -1, str_len)
      STOP
    end if

    call fluL_openlibs(L)

    if (flu_pcall(L, 0, 0, 0) .ne. 0) then
      write(*,*) "cannot run configuration file: ", &
        &        flu_tolstring(L, -1, str_len)
      STOP
    end if

  end subroutine open_config


  subroutine close_config(L)
    type(flu_State) :: L

    call flu_close(L)

  end subroutine close_config


  subroutine get_config_real(L, val, ErrCode, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    real(kind=single_k), intent(out) :: val
    integer, intent(out) :: ErrCode
    real(kind=single_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_config_real


  subroutine get_config_double(L, val, ErrCode, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    real(kind=double_k), intent(out) :: val
    integer, intent(out) :: ErrCode
    real(kind=double_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_config_double


  subroutine get_config_integer(L, val, ErrCode, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    integer, intent(out) :: val
    integer, intent(out) :: ErrCode
    integer, optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_config_integer


  subroutine get_config_long(L, val, ErrCode, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    integer(kind=long_k), intent(out) :: val
    integer, intent(out) :: ErrCode
    integer(kind=long_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_config_long


  subroutine get_config_logical(L, val, ErrCode, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    logical, intent(out) :: val
    integer, intent(out) :: ErrCode
    logical, optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_config_logical


  subroutine get_config_string(L, val, ErrCode, key, default)
    type(flu_State) :: L
    character(len=*), intent(in) :: key
    character(len=*) :: val
    integer, intent(out) :: ErrCode
    character(len=*), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(L, val, ErrCode, default)

  end subroutine get_config_string

end module aotus_module

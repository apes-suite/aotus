module aot_extdouble_module
  use flu_binding
  use aot_extdouble_top_module, only: xdble_k
  use aot_top_module, only: aot_top_get_val, aot_err_handler, &
    &                       aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType

  implicit none

  private

  public :: aot_get_val

  interface aot_get_val
    module procedure get_config_extdouble
  end interface

contains

  subroutine get_config_extdouble(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    real(kind=xdble_k), intent(out) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    real(kind=xdble_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_extdouble

end module aot_extdouble_module

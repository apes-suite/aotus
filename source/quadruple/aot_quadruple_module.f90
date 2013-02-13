! Copyright (C) 2011-2013 German Research School for Simulation Sciences GmbH,
!              Aachen and others.
! Please see the ../COPYRIGHT file one directory above for details.

module aot_quadruple_module
  use flu_binding
  use aot_quadruple_top_module, only: quad_k
  use aot_top_module, only: aot_top_get_val, aot_err_handler, &
    &                       aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType

  implicit none

  private

  public :: aot_get_val

  interface aot_get_val
    module procedure get_config_quadruple
  end interface

contains

  subroutine get_config_quadruple(val, ErrCode, L, key, default)
    type(flu_State) :: L !< Handle for the Lua script to get the value from.
    character(len=*), intent(in) :: key !< Variable name to look for.

    !> Value of the Variable in the script
    real(kind=quad_k), intent(out) :: val

    !> ErrorCode to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value that should be used, if the variable is not set in the
    !! Lua script.
    real(kind=quad_k), optional, intent(in) :: default

    call flu_getglobal(L, key)
    call aot_top_get_val(val, ErrCode, L, default)

  end subroutine get_config_quadruple

end module aot_quadruple_module

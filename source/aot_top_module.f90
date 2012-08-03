!> Module for interaction with topmost element of the Lua stack.
!!
!! This is a basic module which provides the fundamental functionality to
!! access the topmost element in the stack of the Lua API.
!! All intrinsic variables except complex numbers can be accessed this way.
module aot_top_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k

  implicit none

  private

  public :: aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType
  public :: aot_top_get_val
  public :: aot_err_handler

  !> Some parameters for the error handling.
  !!
  !! They indicate the bits to set in case of
  !! the corresponding error, to allow appropiate
  !! reactions of the calling application.
  !! As a bitmask is used to encode the error, and combination of them
  !! might be returned.
  integer, parameter :: aoterr_Fatal = 0
  integer, parameter :: aoterr_NonExistent = 1
  integer, parameter :: aoterr_WrongType = 2

  !> Get the value on top of the stack
  !!
  !! This is the most basic operation to
  !! retrieve a value.
  !! It is also most flexible in the sense,
  !! that it does not matter how the value
  !! actually gets on top of the stack by
  !! previous Lua operations.
  !!
  !! The interface looks like this:
  !! `call aot_top_get_val(val, errCode, L, default)`.
  !! See for example aot_top_get_real() for a more detailed
  !! description of the parameters.
  !!
  !! The aot_top_get_val can not be in the same generic interface as the other
  !! aot_get_val routines, as it results in ambiguities of the interfaces.
  interface aot_top_get_val
    module procedure aot_top_get_real
    module procedure aot_top_get_double
    module procedure aot_top_get_integer
    module procedure aot_top_get_long
    module procedure aot_top_get_string
    module procedure aot_top_get_logical
  end interface

contains

  !> Interpret topmost entry on Lua stack as a single precision real.
  subroutine aot_top_get_real(val, ErrCode, L, default)
    type(flu_State) :: L !< Handle to the Lua script

    !> Value of the Variable in the script
    real(kind=single_k), intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    real(kind=single_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(L, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(L, -1)) then
        val = flu_toNumber(L, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(L)

  end subroutine aot_top_get_real


  !> Interpret topmost entry on Lua stack as a double precision real.
  subroutine aot_top_get_double(val, ErrCode, L, default)
    type(flu_State) :: L !< Handle to the Lua script

    !> Value of the Variable in the script
    real(kind=double_k), intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    real(kind=double_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(L, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(L, -1)) then
        val = flu_toDouble(L, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(L)

  end subroutine aot_top_get_double


  !> Interpret topmost entry on Lua stack as a default integer.
  subroutine aot_top_get_integer(val, ErrCode, L, default)
    type(flu_State) :: L !< Handle to the Lua script

    !> Value of the Variable in the script
    integer, intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    integer, optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(L, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(L, -1)) then
        val = int(flu_toDouble(L, -1))
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(L)

  end subroutine aot_top_get_integer


  !> Interpret topmost entry on Lua stack as a single precision real.
  subroutine aot_top_get_long(val, ErrCode, L, default)
    type(flu_State) :: L !< Handle to the Lua script

    !> Value of the Variable in the script
    integer(kind=long_k), intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    integer(kind=long_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(L, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(L, -1)) then
        val = int(flu_toDouble(L, -1),kind=long_k)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(L)

  end subroutine aot_top_get_long


  !> Interpret topmost entry on Lua stack as a single precision real.
  subroutine aot_top_get_logical(val, ErrCode, L, default)
    type(flu_State) :: L !< Handle to the Lua script

    !> Value of the Variable in the script
    logical, intent(out) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    logical, optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(L, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isBoolean(L, -1)) then
        val = flu_toBoolean(L, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(L)

  end subroutine aot_top_get_logical


  !> Interpret topmost entry on Lua stack as a single precision real.
  subroutine aot_top_get_string(val, ErrCode, L, default)
    type(flu_State) :: L !< Handle to the Lua script

    !> Value of the Variable in the script
    character(len=*) :: val

    !> Error code to indicate what kind of problem might have occured.
    integer, intent(out) :: ErrCode

    !> Some default value, that should be used, if the variable is not set in
    !! the Lua script.
    character(len=*), optional, intent(in) :: default

    logical :: not_retrievable
    character, pointer :: cstring(:)
    integer :: i, StrLen, StrLimit

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(L, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      cstring => flu_toLString(L, -1, StrLen)
      StrLimit = min(StrLen, len(val))
      val = ''
      do i=1,StrLimit
        val(i:i) = cstring(i)
      end do
    end if

    if (not_retrievable) then
      if (present(default)) then
        val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(L)

  end subroutine aot_top_get_string


  !> Error handler to capture Lua errors.
  !!
  !! This routine encapsulates the retrieval of error messages from the Lua
  !! stack upon a failing Lua operation.
  !! It should be be used after all flu functions, that return an err as result.
  !! Such as flu_binding::fluL_loadfile and flu_binding::flu_pcall.
  !! The ErrString and ErrCode parameters are both optional if none of them are
  !! provided, the execution will be stopped if an error had occured and err is
  !! not 0. The error message will be written to standard output in this case.
  !!
  !! If either of them are provide, the application will continue, and the
  !! calling side has to deal with the occured error.
  subroutine aot_err_handler(L, err, msg, ErrString, ErrCode)
    type(flu_State) :: L !< Handle to the Lua script

    !> Lua error code to evaluate
    integer, intent(in) :: err

    !> Some additional message that should be prepended to the Lua error
    !! message.
    character(len=*), intent(in) :: msg

    !> Resulting error string obtained by combination of msg and the error
    !! description on the Lua stack.
    character(len=*), intent(out), optional :: ErrString

    !> The Lua error code, just the same as err.
    integer, intent(out), optional :: ErrCode

    logical :: stop_on_error
    character, pointer, dimension(:) :: string
    integer :: str_len
    integer :: i

    stop_on_error = .not.(present(ErrString) .or. present(ErrCode))

    if (present(ErrCode)) then
      ErrCode = err
    end if

    if (err .ne. 0) then

      string => flu_tolstring(L, -1, str_len)
      if (present(ErrString)) then
        do i=1,min(str_len, len(ErrString))
          ErrString(i:i) = string(i)
        end do
      end if

      if (stop_on_error) then
        write(*,*) msg, string
        STOP
      end if

    end if

  end subroutine aot_err_handler


end module aot_top_module

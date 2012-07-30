!> Module for interaction with topmost element of Luas stack.
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
  interface aot_top_get_val
    module procedure aot_top_get_real
    module procedure aot_top_get_double
    module procedure aot_top_get_integer
    module procedure aot_top_get_long
    module procedure aot_top_get_string
    module procedure aot_top_get_logical
  end interface

  interface aot_get_val
    module procedure aot_top_get_real
    module procedure aot_top_get_double
    module procedure aot_top_get_integer
    module procedure aot_top_get_long
    module procedure aot_top_get_string
    module procedure aot_top_get_logical
  end interface


contains


  subroutine aot_top_get_real(val, ErrCode, L, default)
    type(flu_State) :: L
    real(kind=single_k), intent(out) :: val
    integer, intent(out) :: ErrCode
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


  subroutine aot_top_get_double(val, ErrCode, L, default)
    type(flu_State) :: L
    real(kind=double_k), intent(out) :: val
    integer, intent(out) :: ErrCode
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


  subroutine aot_top_get_integer(val, ErrCode, L, default)
    type(flu_State) :: L
    integer, intent(out) :: val
    integer, intent(out) :: ErrCode
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

  subroutine aot_top_get_long(val, ErrCode, L, default)
    type(flu_State) :: L
    integer(kind=long_k), intent(out) :: val
    integer, intent(out) :: ErrCode
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

  subroutine aot_top_get_logical(val, ErrCode, L, default)
    type(flu_State) :: L
    logical, intent(out) :: val
    integer, intent(out) :: ErrCode
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


  subroutine aot_top_get_string(val, ErrCode, L, default)
    type(flu_State) :: L
    character(len=*) :: val
    integer, intent(out) :: ErrCode
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


  subroutine aot_err_handler(L, err, msg, ErrString, ErrCode)
    type(flu_State) :: L
    integer, intent(in) :: err
    character(len=*), intent(in) :: msg
    character(len=*), intent(out), optional :: ErrString
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

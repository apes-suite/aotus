!> This module provides high level Fortran interfaces
!! to retrieve values from a Lua script.
module aotus_module
  use flu_binding
  use aot_kinds_module, only: double_k, single_k, long_k
  use aot_table_module, only: aot_table_getval

  implicit none

  private

  public :: aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType
  public :: get_config_val, open_config, close_config
  public :: get_top_val, get_table_val

  !> Some parameters for the error handling.
  !! They indicate the bits to set in case of
  !! the corresponding error, to allow appropiate
  !! reactions of the calling application.
  integer, parameter :: aoterr_Fatal = 0
  integer, parameter :: aoterr_NonExistent = 1
  integer, parameter :: aoterr_WrongType = 2

  !> Get the value on top of the stack
  !! This is the most basic operation to
  !! retrieve a value.
  !! It is also most flexible in the sense,
  !! that it does not matter how the value
  !! actually gets on top of the stack by
  !! previous Lua operations.
  interface get_top_val
    module procedure get_top_real
    module procedure get_top_double
    module procedure get_top_integer
    module procedure get_top_long
    module procedure get_top_string
    module procedure get_top_logical
  end interface

  !> Get a global configuration value from the script.
  !! This provides a convenient direct access to
  !! global variables from the Lua script.
  !! \todo unify the arguments and naming with the rest
  !!       of the library, rename the config_val to key,
  !!       to make the interface more consistent.
  interface get_config_val
    module procedure get_config_real
    module procedure get_config_double
    module procedure get_config_integer
    module procedure get_config_long
    module procedure get_config_string
    module procedure get_config_logical
  end interface

  !> Get a value from a table, first the given
  !! key is looked up, if this fails, the value
  !! at the given position is looked up, and if
  !! this also fails, the default value is returned.
  !! Positional addressing is only valid, as long,
  !! as no value was provided by an explicit key
  !! in the list before the entry in question.
  !! \todo add convenience functions which return
  !!       complete vectors at once.
  interface get_table_val
    module procedure get_table_real
    module procedure get_table_double
    module procedure get_table_integer
    module procedure get_table_long
    module procedure get_table_string
    module procedure get_table_logical
  end interface

contains

  subroutine open_config(conf, filename)
    type(flu_State) :: conf
    character(len=*), intent(in) :: filename
    integer :: str_len

    conf = fluL_newstate()

    if (fluL_loadfile(conf, filename) .ne. 0) then
      write(*,*) "cannot load configuration file: ", flu_tolstring(conf, -1, str_len)
      STOP
    end if

    call fluL_openlibs(conf)

    if (flu_pcall(conf, 0, 0, 0) .ne. 0) then
      write(*,*) "cannot run configuration file: ", flu_tolstring(conf, -1, str_len)
      STOP
    end if

  end subroutine open_config


  subroutine close_config(conf)
    type(flu_State) :: conf

    call flu_close(conf)

  end subroutine close_config


  subroutine get_top_real(conf, top_val, ErrCode, default)
    type(flu_State) :: conf
    real(kind=single_k), intent(out) :: top_val
    integer, intent(out) :: ErrCode
    real(kind=single_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        top_val = flu_toNumber(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        top_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_top_real


  subroutine get_top_double(conf, top_val, ErrCode, default)
    type(flu_State) :: conf
    real(kind=double_k), intent(out) :: top_val
    integer, intent(out) :: ErrCode
    real(kind=double_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        top_val = flu_toDouble(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        top_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_top_double


  subroutine get_top_integer(conf, top_val, ErrCode, default)
    type(flu_State) :: conf
    integer, intent(out) :: top_val
    integer, intent(out) :: ErrCode
    integer, optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        top_val = int(flu_toNumber(conf, -1))
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        top_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_top_integer

  subroutine get_top_long(conf, top_val, ErrCode, default)
    type(flu_State) :: conf
    integer(kind=long_k), intent(out) :: top_val
    integer, intent(out) :: ErrCode
    integer(kind=long_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        top_val = int(flu_toNumber(conf, -1),kind=long_k)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        top_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_top_long

  subroutine get_top_logical(conf, top_val, ErrCode, default)
    type(flu_State) :: conf
    logical, intent(out) :: top_val
    integer, intent(out) :: ErrCode
    logical, optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isBoolean(conf, -1)) then
        top_val = flu_toBoolean(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        top_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_top_logical


  subroutine get_top_string(conf, top_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*) :: top_val
    integer, intent(out) :: ErrCode
    character(len=*), optional, intent(in) :: default

    logical :: not_retrievable
    character, pointer :: cstring(:)
    integer :: i, StrLen, StrLimit

    ErrCode = 0
    not_retrievable = .false.

    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      cstring => flu_toLString(conf, -1, StrLen)
      StrLimit = min(StrLen, len(top_val))
      top_val = ''
      do i=1,StrLimit
        top_val(i:i) = cstring(i)
      end do
    end if

    if (not_retrievable) then
      if (present(default)) then
        top_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_top_string


  subroutine get_config_real(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    real(kind=single_k), intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    real(kind=single_k), optional, intent(in) :: default

    call flu_getglobal(conf, var)
    call get_top_val(conf, conf_val, ErrCode, default)

  end subroutine get_config_real


  subroutine get_config_double(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    real(kind=double_k), intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    real(kind=double_k), optional, intent(in) :: default

    call flu_getglobal(conf, var)
    call get_top_val(conf, conf_val, ErrCode, default)

  end subroutine get_config_double


  subroutine get_config_integer(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    integer, intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    integer, optional, intent(in) :: default

    call flu_getglobal(conf, var)
    call get_top_val(conf, conf_val, ErrCode, default)

  end subroutine get_config_integer


  subroutine get_config_long(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    integer(kind=long_k), intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    integer(kind=long_k), optional, intent(in) :: default

    call flu_getglobal(conf, var)
    call get_top_val(conf, conf_val, ErrCode, default)

  end subroutine get_config_long


  subroutine get_config_logical(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    logical, intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    logical, optional, intent(in) :: default

    call flu_getglobal(conf, var)
    call get_top_val(conf, conf_val, ErrCode, default)

  end subroutine get_config_logical


  subroutine get_config_string(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    character(len=*) :: conf_val
    integer, intent(out) :: ErrCode
    character(len=*), optional, intent(in) :: default

    call flu_getglobal(conf, var)
    call get_top_val(conf, conf_val, ErrCode, default)

  end subroutine get_config_string


  subroutine get_table_real(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    real(kind=single_k), intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    real(kind=single_k), intent(in), optional :: default

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    call get_top_val(conf, tab_val, ErrCode, default)

  end subroutine get_table_real


  subroutine get_table_double(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    real(kind=double_k), intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    real(kind=double_k), intent(in), optional :: default

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    call get_top_val(conf, tab_val, ErrCode, default)

  end subroutine get_table_double


  subroutine get_table_integer(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    integer, intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    integer, intent(in), optional :: default

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    call get_top_val(conf, tab_val, ErrCode, default)

  end subroutine get_table_integer

  subroutine get_table_long(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    integer(kind=long_k), intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    integer(kind=long_k), intent(in), optional :: default

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    call get_top_val(conf, tab_val, ErrCode, default)

  end subroutine get_table_long

  subroutine get_table_logical(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    logical, intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    logical, intent(in), optional :: default

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    call get_top_val(conf, tab_val, ErrCode, default)

  end subroutine get_table_logical


  subroutine get_table_string(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    character(len=*) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: default

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    call get_top_val(conf, tab_val, ErrCode, default)

  end subroutine get_table_string


end module aotus_module

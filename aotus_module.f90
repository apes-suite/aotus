module aotus_module
  use flu_binding
  use aot_table_module, only: aot_table_getval

  implicit none

  private

  public :: aoterr_Fatal, aoterr_NonExistent, aoterr_WrongType
  public :: get_config_val, open_config, close_config
  public :: get_table_val

  integer, parameter :: aoterr_Fatal = 0
  integer, parameter :: aoterr_NonExistent = 1
  integer, parameter :: aoterr_WrongType = 2

  integer, parameter :: double_k = selected_real_kind(15)
  integer, parameter :: single_k = selected_real_kind(6)

  !> Get a global configuration value from the script
  interface get_config_val
    module procedure get_config_real
    module procedure get_config_double
    module procedure get_config_integer
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
  interface get_table_val
    module procedure get_table_real
    module procedure get_table_double
    module procedure get_table_integer
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

    if (flu_pcall(conf, 0, 0, 0) .ne. 0) then
      write(*,*) "cannot run configuration file: ", flu_tolstring(conf, -1, str_len)
      STOP
    end if

  end subroutine open_config


  subroutine close_config(conf)
    type(flu_State) :: conf

    call flu_close(conf)

  end subroutine close_config


  subroutine get_config_real(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    real(kind=single_k), intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    real(kind=single_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call flu_getglobal(conf, var)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        conf_val = flu_toNumber(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        conf_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_config_real


  subroutine get_config_double(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    real(kind=double_k), intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    real(kind=double_k), optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call flu_getglobal(conf, var)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        conf_val = flu_toNumber(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        conf_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_config_double


  subroutine get_config_integer(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    integer, intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    integer, optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call flu_getglobal(conf, var)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        conf_val = int(flu_toNumber(conf, -1))
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        conf_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_config_integer


  subroutine get_config_logical(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    logical, intent(out) :: conf_val
    integer, intent(out) :: ErrCode
    logical, optional, intent(in) :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call flu_getglobal(conf, var)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isBoolean(conf, -1)) then
        conf_val = flu_toBoolean(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        conf_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_config_logical


  subroutine get_config_string(conf, var, conf_val, ErrCode, default)
    type(flu_State) :: conf
    character(len=*), intent(in) :: var
    character(len=*) :: conf_val
    integer, intent(out) :: ErrCode
    character(len=*), optional, intent(in) :: default

    logical :: not_retrievable
    character, pointer :: cstring(:)
    integer :: i, StrLen, StrLimit

    ErrCode = 0
    not_retrievable = .false.

    call flu_getglobal(conf, var)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      cstring => flu_toLString(conf, -1, StrLen)
      StrLimit = min(StrLen, len(conf_val))
      conf_val = ''
      do i=1,StrLimit
        conf_val(i:i) = cstring(i)
      end do
    end if

    if (not_retrievable) then
      if (present(default)) then
        conf_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_config_string


  subroutine get_table_real(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    real(kind=single_k), intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    real(kind=single_k), intent(in), optional :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        tab_val = flu_toNumber(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        tab_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_table_real


  subroutine get_table_double(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    real(kind=double_k), intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    real(kind=double_k), intent(in), optional :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        tab_val = flu_toNumber(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        tab_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_table_double


  subroutine get_table_integer(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    integer, intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    integer, intent(in), optional :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isNumber(conf, -1)) then
        tab_val = int(flu_toNumber(conf, -1))
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        tab_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_table_integer


  subroutine get_table_logical(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    logical, intent(out) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    logical, intent(in), optional :: default

    logical :: not_retrievable

    ErrCode = 0
    not_retrievable = .false.

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      if (flu_isBoolean(conf, -1)) then
        tab_val = flu_toBoolean(conf, -1)
      else
        ErrCode = ibSet(ErrCode, aoterr_WrongType)
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
        not_retrievable = .true.
      end if
    end if

    if (not_retrievable) then
      if (present(default)) then
        tab_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_table_logical


  subroutine get_table_string(conf, thandle, tab_val, ErrCode, var, pos, default)
    type(flu_State) :: conf
    integer, intent(in) :: thandle
    character(len=*) :: tab_val
    integer, intent(out) :: ErrCode

    character(len=*), intent(in), optional :: var
    integer, intent(in), optional :: pos
    character(len=*), intent(in), optional :: default

    logical :: not_retrievable
    character, pointer :: cstring(:)
    integer :: i, StrLen, StrLimit

    ErrCode = 0
    not_retrievable = .false.

    call aot_table_getval(L=conf, thandle=thandle, &
      &                   key=var, pos=pos)
    if (flu_isNoneOrNil(conf, -1)) then
      ErrCode = ibSet(ErrCode, aoterr_NonExistent)
      not_retrievable = .true.
    else
      cstring => flu_toLString(conf, -1, StrLen)
      StrLimit = min(StrLen, len(tab_val))
      tab_val = ''
      do i=1,StrLimit
        tab_val(i:i) = cstring(i)
      end do
    end if

    if (not_retrievable) then
      if (present(default)) then
        tab_val = default
      else
        ErrCode = ibSet(ErrCode, aoterr_Fatal)
      end if
    end if
    call flu_pop(conf)

  end subroutine get_table_string


end module aotus_module
